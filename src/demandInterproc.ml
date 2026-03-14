module SM = Map.Make (String)
module SS = Set.Make (String)

type call_site = {
  caller : string;
  block : string;
  instr_index : int;
  callee : string;
  args : string list;
  ret_dst : string option;
  state : SymState.t;
  internal : bool;
}

type func_result = {
  func : MicroIR.func;
  summary : ModSummary.summary;
  entry_state : SymState.t;
  call_sites : call_site list;
}

type analysis = {
  entry : string;
  order : string list;
  results : func_result SM.t;
  truncated : bool;
}

type env = {
  path : string;
  defined : SS.t;
}

let is_internal_name env name =
  SS.mem name env.defined
  && not (String.contains name '@')
  && not (String.ends_with ~suffix:"_plt" name)

let import_func env fname =
  match BinImport.import ~func:fname env.path with
  | [ fn ] -> fn
  | _ -> failwith ("unexpected import shape for " ^ fname)

let callee_seed fn summary args state =
  let seed = SymExec.from_summary summary in
  let rec zip xs ys =
    match xs, ys with
    | x :: xs, y :: ys -> (x, y) :: zip xs ys
    | _ -> []
  in
  let bindings =
    let rec take n xs =
      match n, xs with
      | 0, _ | _, [] -> []
      | n, x :: rest -> x :: take (n - 1) rest
    in
    zip fn.MicroIR.params (take (List.length fn.MicroIR.params) args)
  in
  let st =
    List.fold_left (fun acc (p, a) -> SymState.bind acc p a) seed bindings
  in
  { st with SymState.mem = state.SymState.mem; location = fn.MicroIR.entry }

let walk_function env fn seed ~on_call =
  let blocks = CfgAnalysis.block_map fn in
  let visited = ref SS.empty in
  let rec traverse lbl st =
    if SS.mem lbl !visited then
      ()
    else begin
      visited := SS.add lbl !visited;
      match SM.find_opt lbl blocks with
      | None -> ()
      | Some blk -> exec_instrs blk 0 (SymState.at st lbl)
    end
  and exec_instrs blk idx st =
    if idx >= List.length blk.MicroIR.body then
      exec_term blk st
    else
      match List.nth blk.MicroIR.body idx with
      | MicroIR.ICall (ret, MicroIR.VReg callee, args) as ins ->
          let call_state = SymState.at st blk.MicroIR.label in
          let arg_values = List.map (SymExec.sym_value st) args in
          let internal = is_internal_name env callee in
          on_call
            {
              caller = fn.MicroIR.fname;
              block = blk.MicroIR.label;
              instr_index = idx;
              callee;
              args = arg_values;
              ret_dst = ret;
              state = call_state;
              internal;
            };
          exec_instrs blk (idx + 1) (SymExec.exec_instr st ins)
      | ins ->
          exec_instrs blk (idx + 1) (SymExec.exec_instr st ins)
  and exec_term blk st =
    match blk.MicroIR.term with
    | MicroIR.TJump succ -> traverse succ st
    | MicroIR.TBranch (g, t_lbl, f_lbl) ->
        let t_state =
          st
          |> fun st' -> SymState.add_path st' (SymExec.sym_expr st g)
          |> fun st' -> SymState.at st' t_lbl
        in
        let f_state =
          st
          |> fun st' ->
          SymState.add_path st' (SymExec.sym_expr st (MicroIR.negate_expr g))
          |> fun st' -> SymState.at st' f_lbl
        in
        traverse t_lbl t_state;
        traverse f_lbl f_state
    | MicroIR.TSwitch (v, cases, dflt) ->
        let v_s = SymExec.sym_value st v in
        List.iter
          (fun (n, lbl) ->
            traverse lbl
              (st
              |> fun st' ->
              SymState.add_path st' (Printf.sprintf "(%s eq %d)" v_s n)
              |> fun st' -> SymState.at st' lbl))
          cases;
        traverse dflt (SymState.at st dflt)
    | MicroIR.TReturn _ | MicroIR.TStop -> ()
  in
  traverse fn.MicroIR.entry (SymState.at seed fn.MicroIR.entry)

let analyze_binary ?(max_functions = 32) ~path ~entry () =
  let env =
    { path; defined = BinImport.defined_functions path |> List.to_seq |> SS.of_seq }
  in
  let results = ref SM.empty in
  let order = ref [] in
  let active = ref SS.empty in
  let truncated = ref false in
  let rec ensure ?seed fname =
    if SM.mem fname !results || SS.mem fname !active then
      ()
    else if List.length !order >= max_functions then
      truncated := true
    else begin
      active := SS.add fname !active;
      let fn = import_func env fname in
      let summary = ModSummary.infer fn in
      let entry_state =
        match seed with
        | Some st -> st
        | None -> SymState.at (SymExec.from_summary summary) fn.MicroIR.entry
      in
      let call_sites = ref [] in
      walk_function env fn entry_state ~on_call:(fun cs ->
          call_sites := cs :: !call_sites;
          if cs.internal then begin
            let callee_fn = import_func env cs.callee in
            let callee_summary = ModSummary.infer callee_fn in
            let callee_state = callee_seed callee_fn callee_summary cs.args cs.state in
            ensure ~seed:callee_state cs.callee
          end);
      let result =
        {
          func = fn;
          summary;
          entry_state;
          call_sites = List.rev !call_sites;
        }
      in
      results := SM.add fname result !results;
      order := !order @ [ fname ];
      active := SS.remove fname !active
    end
  in
  ensure entry;
  { entry; order = !order; results = !results; truncated = !truncated }

let render_call_site cs =
  Printf.sprintf
    "callsite(block=%s, idx=%d, callee=%s, internal=%b, ret=%s, args=[%s], state=%s)"
    cs.block cs.instr_index cs.callee cs.internal
    (match cs.ret_dst with Some r -> r | None -> "void")
    (String.concat ", " cs.args)
    (SymState.render cs.state)

let render_result r =
  let calls =
    if r.call_sites = [] then
      "    none"
    else
      r.call_sites
      |> List.map (fun cs -> "    " ^ render_call_site cs)
      |> String.concat "\n"
  in
  String.concat "\n"
    [
      Printf.sprintf "Function %s:" r.func.MicroIR.fname;
      Printf.sprintf
        "  signature: params=[%s] ret=%s"
        (String.concat "," r.func.MicroIR.params)
        (match r.func.MicroIR.ret with Some r -> r | None -> "void");
      "  " ^ ModSummary.render r.summary;
      "  entry-state:";
      "    " ^ SymState.render r.entry_state;
      "  call-sites:";
      calls;
    ]

let render a =
  let body =
    a.order
    |> List.filter_map (fun fname -> SM.find_opt fname a.results)
    |> List.map render_result
    |> String.concat "\n\n"
  in
  String.concat "\n"
    [
      "Demand-driven reachable analysis:";
      "  entry=" ^ a.entry;
      "  reachable=[" ^ String.concat "," a.order ^ "]";
      "  truncated=" ^ string_of_bool a.truncated;
      "";
      body;
    ]
