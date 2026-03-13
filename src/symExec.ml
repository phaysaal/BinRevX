type branch_state = {
  tag : string;
  state : SymState.t;
}

module SM = Map.Make (String)

let from_summary (s : ModSummary.summary) =
  SymState.of_inputs s.ModSummary.pre

let sym_value st = function
  | MicroIR.VReg v -> (
      match SM.find_opt v st.SymState.env with
      | Some e -> e
      | None -> "sym_" ^ v)
  | MicroIR.VConst n -> string_of_int n
  | MicroIR.VUndef -> "undef"

let sym_expr st =
  let rec go = function
    | MicroIR.EVal v -> sym_value st v
    | MicroIR.EBinop (op, a, b) ->
        Printf.sprintf "(%s %s %s)" (sym_value st a) op (sym_value st b)
    | MicroIR.ELoad v ->
        let key = sym_value st v in
        (match SM.find_opt key st.SymState.mem with
        | Some e -> e
        | None -> Printf.sprintf "mem[%s]" key)
    | MicroIR.EAddr (v, off) -> Printf.sprintf "addr(%s,%d)" (sym_value st v) off
    | MicroIR.ECmp (pred, a, b) ->
        Printf.sprintf "(%s %s %s)" (sym_value st a) pred (sym_value st b)
  in
  go

let exec_instr st = function
  | MicroIR.IAssign (dst, e) ->
      let rhs = sym_expr st e in
      SymState.bind st dst rhs
  | MicroIR.IStore (dst, src) ->
      let addr = sym_value st dst in
      let value = sym_value st src in
      SymState.store st addr value
  | MicroIR.IAssume e ->
      SymState.add_path st (sym_expr st e)
  | MicroIR.IAssert e ->
      SymState.add_path st ("assert " ^ sym_expr st e)
  | MicroIR.ICall (ret, callee, args) ->
      let call =
        Printf.sprintf
          "%s(%s)"
          (sym_value st callee)
          (String.concat ", " (List.map (sym_value st) args))
      in
      (match ret with
      | Some v -> SymState.bind st v call
      | None -> st)

let exec_block_body st blk =
  List.fold_left exec_instr (SymState.at st blk.MicroIR.label) blk.MicroIR.body

let stop_at_branch fn loop_headers label =
  CfgAnalysis.SS.mem label loop_headers
  ||
  match (CfgAnalysis.SM.find label (CfgAnalysis.block_map fn)).MicroIR.term with
  | MicroIR.TBranch _ | MicroIR.TSwitch _ | MicroIR.TReturn _ | MicroIR.TStop -> true
  | MicroIR.TJump _ -> false

let follow_straightline fn loop_headers st =
  let bmap = CfgAnalysis.block_map fn in
  let rec aux seen st =
    let label = st.SymState.location in
    if CfgAnalysis.SS.mem label seen then
      st
    else
      let seen' = CfgAnalysis.SS.add label seen in
      match CfgAnalysis.SM.find_opt label bmap with
      | None -> st
      | Some blk ->
          let st' = exec_block_body st blk in
          if stop_at_branch fn loop_headers label then
            st'
          else
            match blk.MicroIR.term with
            | MicroIR.TJump succ -> aux seen' (SymState.at st' succ)
            | _ -> st'
  in
  aux CfgAnalysis.SS.empty st

let states_at_region_headers fn seed =
  let loops = LoopRecovery.recover_loops fn in
  let loop_header_set =
    List.fold_left
      (fun acc lp -> CfgAnalysis.SS.add lp.LoopRecovery.header acc)
      CfgAnalysis.SS.empty
      loops
  in
  let bmap = CfgAnalysis.block_map fn in
  let rec work queue visited acc =
    match queue with
    | [] -> acc
    | (lbl, st_in) :: qs ->
        if CfgAnalysis.SM.mem lbl visited then
          work qs visited acc
        else
          match CfgAnalysis.SM.find_opt lbl bmap with
          | None -> work qs (CfgAnalysis.SM.add lbl st_in visited) acc
          | Some blk ->
              let st_body = exec_block_body (SymState.at st_in lbl) blk in
              let visited' = CfgAnalysis.SM.add lbl st_body visited in
              let acc' = CfgAnalysis.SM.add lbl st_body acc in
              let next_q =
                match blk.MicroIR.term with
                | MicroIR.TJump succ ->
                    if CfgAnalysis.SS.mem succ loop_header_set then (succ, st_body) :: qs
                    else (succ, st_body) :: qs
                | MicroIR.TBranch _ | MicroIR.TSwitch _ | MicroIR.TReturn _ | MicroIR.TStop -> qs
              in
              work next_q visited' acc'
  in
  let seed0 = SymState.at seed fn.MicroIR.entry in
  let header_states = work [ (fn.MicroIR.entry, seed0) ] CfgAnalysis.SM.empty CfgAnalysis.SM.empty in
  (header_states, loop_header_set)

let split_region fn header_states loop_headers base = function
  | RegionRecovery.RCond c ->
      let base =
        match CfgAnalysis.SM.find_opt c.RegionRecovery.header header_states with
        | Some st -> st
        | None -> base
      in
      [
        {
          tag = "then";
          state =
            base
            |> fun st -> SymState.at st c.RegionRecovery.header
            |> fun st -> SymState.add_path st c.RegionRecovery.guard_true
            |> fun st -> SymState.at st c.RegionRecovery.then_lbl
            |> follow_straightline fn loop_headers;
        };
        {
          tag = "else";
          state =
            base
            |> fun st -> SymState.at st c.RegionRecovery.header
            |> fun st -> SymState.add_path st c.RegionRecovery.guard_false
            |> fun st -> SymState.at st c.RegionRecovery.else_lbl
            |> follow_straightline fn loop_headers;
        };
      ]
  | RegionRecovery.RLoop lp ->
      let base =
        match CfgAnalysis.SM.find_opt lp.LoopRecovery.header header_states with
        | Some st -> st
        | None -> base
      in
      let guard =
        match lp.LoopRecovery.guard with
        | Some g -> sym_expr base g
        | None -> "loop_guard_unknown"
      in
      let neg_guard =
        match lp.LoopRecovery.guard with
        | Some g -> sym_expr base (MicroIR.negate_expr g)
        | None -> "!(loop_guard_unknown)"
      in
      let exit_loc =
        match lp.LoopRecovery.exits with
        | x :: _ -> x
        | [] -> "loop_exit_unknown"
      in
      [
        {
          tag = "loop-continue";
          state =
            base
            |> fun st -> SymState.at st lp.LoopRecovery.header
            |> fun st -> SymState.add_path st guard;
        };
        {
          tag = "loop-exit";
          state =
            base
            |> fun st -> SymState.at st lp.LoopRecovery.header
            |> fun st -> SymState.add_path st neg_guard
            |> fun st -> SymState.at st exit_loc
            |> follow_straightline fn loop_headers;
        };
      ]
  | RegionRecovery.RResidual nodes ->
      [
        {
          tag = "residual";
          state =
            base
            |> fun st ->
            SymState.at st
              (match nodes with x :: _ -> x | [] -> "residual_unknown");
        };
      ]

let explore_function fn =
  let summary = ModSummary.infer fn in
  let seed = from_summary summary in
  let regions = RegionRecovery.recover_regions fn in
  let header_states, loop_headers = states_at_region_headers fn seed in
  ( summary,
    seed,
    header_states,
    List.concat_map (split_region fn header_states loop_headers seed) regions )

let render_branch_state bs =
  Printf.sprintf "%s -> %s" bs.tag (SymState.render bs.state)
