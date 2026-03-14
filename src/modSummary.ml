type summary = {
  fname : string;
  params : string list;
  pre : string list;
  post : string list;
  modifies : string list;
  returns : string option;
  calls : string list;
}

let sort_uniq xs = List.sort_uniq String.compare xs

let starts_with s prefix =
  let ns = String.length s in
  let np = String.length prefix in
  ns >= np && String.sub s 0 np = prefix

let summary_noise v =
  v = "esp" || v = "rsp" || v = "ebp" || v = "rbp" || v = "sp" || v = "fp"
  || v = "rdi" || v = "rsi" || v = "rdx" || v = "rcx" || v = "r8" || v = "r9"
  || v = "r0" || v = "r1" || v = "r2" || v = "r3"
  || v = "flag"
  || starts_with v "esp_"
  || starts_with v "rsp_"
  || starts_with v "ebp_"
  || starts_with v "rbp_"
  || starts_with v "gs_"
  || starts_with v "__x86.get_pc_thunk."

let sanitize_summary_vars xs =
  xs |> List.filter (fun v -> not (summary_noise v)) |> sort_uniq

let used_value = function
  | MicroIR.VReg v -> [ v ]
  | MicroIR.VConst _ | MicroIR.VUndef -> []

let used_expr = function
  | MicroIR.EVal v -> used_value v
  | MicroIR.EBinop (_, a, b) -> used_value a @ used_value b
  | MicroIR.ELoad v -> used_value v
  | MicroIR.EAddr (v, _) -> used_value v
  | MicroIR.EIndex (base, idx, _) -> used_value base @ used_value idx
  | MicroIR.EField (base, _) -> used_value base
  | MicroIR.ECmp (_, a, b) -> used_value a @ used_value b

let defs_of_instr = function
  | MicroIR.IAssign (v, _) -> [ v ]
  | MicroIR.ICall (Some v, _, _) -> [ v ]
  | _ -> []

let uses_of_instr = function
  | MicroIR.IAssign (_, e) -> used_expr e
  | MicroIR.IStore (a, b) -> used_value a @ used_value b
  | MicroIR.IAssume e | MicroIR.IAssert e -> used_expr e
  | MicroIR.ICall (_, callee, args) -> used_value callee @ List.flatten (List.map used_value args)

let calls_of_instr = function
  | MicroIR.ICall (_, MicroIR.VReg f, _) -> [ f ]
  | MicroIR.ICall (_, MicroIR.VConst n, _) -> [ "fn_" ^ string_of_int n ]
  | MicroIR.ICall (_, MicroIR.VUndef, _) -> [ "fn_undef" ]
  | _ -> []

let returns_of_term = function
  | MicroIR.TReturn None -> Some "void"
  | MicroIR.TReturn (Some (MicroIR.VReg v)) -> Some v
  | MicroIR.TReturn (Some (MicroIR.VConst n)) -> Some (string_of_int n)
  | MicroIR.TReturn (Some MicroIR.VUndef) -> Some "undef"
  | _ -> None

let uses_of_term = function
  | MicroIR.TJump _ | MicroIR.TReturn _ | MicroIR.TStop -> []
  | MicroIR.TBranch (e, _, _) -> used_expr e
  | MicroIR.TSwitch (v, _, _) -> used_value v

let empty fname =
  {
    fname;
    params = [];
    pre = [];
    post = [];
    modifies = [];
    returns = None;
    calls = [];
  }

let defs_of_block blk =
  List.fold_left
    (fun acc ins -> defs_of_instr ins @ acc)
    []
    blk.MicroIR.body
  |> sort_uniq

let local_uses_before_defs blk =
  let seen_defs = ref CfgAnalysis.SS.empty in
  let uses = ref [] in
  let record_use v =
    if not (CfgAnalysis.SS.mem v !seen_defs) then uses := v :: !uses
  in
  List.iter
    (fun ins ->
      List.iter record_use (uses_of_instr ins);
      List.iter
        (fun v -> seen_defs := CfgAnalysis.SS.add v !seen_defs)
        (defs_of_instr ins))
    blk.MicroIR.body;
  List.iter record_use (uses_of_term blk.MicroIR.term);
  sort_uniq !uses

let infer (fn : MicroIR.func) =
  let defs = ref [] in
  let uses = ref [] in
  let modifies = ref [] in
  let calls = ref [] in
  let returns = ref [] in
  let loops = LoopRecovery.recover_loops fn in
  List.iter
    (fun blk ->
      List.iter
        (fun ins ->
          defs := !defs @ defs_of_instr ins;
          uses := !uses @ uses_of_instr ins;
          modifies := !modifies @ defs_of_instr ins;
          (match ins with
          | MicroIR.IStore _ -> modifies := "memory" :: !modifies
          | _ -> ());
          calls := !calls @ calls_of_instr ins)
        blk.MicroIR.body;
      uses := !uses @ uses_of_term blk.MicroIR.term;
      match returns_of_term blk.MicroIR.term with
      | Some r -> returns := r :: !returns
      | None -> ())
    fn.MicroIR.blocks;
  let preds = CfgAnalysis.predecessors fn in
  let block_defs =
    fn.MicroIR.blocks
    |> List.fold_left
         (fun acc blk -> CfgAnalysis.SM.add blk.MicroIR.label (defs_of_block blk) acc)
         CfgAnalysis.SM.empty
  in
  let all_defs =
    List.fold_left (fun acc v -> CfgAnalysis.SS.add v acc) CfgAnalysis.SS.empty !defs
  in
  let labels = CfgAnalysis.labels fn in
  let init_must_in =
    List.fold_left
      (fun acc lbl ->
        let seed =
          if lbl = fn.MicroIR.entry then CfgAnalysis.SS.empty else all_defs
        in
        CfgAnalysis.SM.add lbl seed acc)
      CfgAnalysis.SM.empty
      labels
  in
  let add_defs set defs =
    List.fold_left (fun s v -> CfgAnalysis.SS.add v s) set defs
  in
  let pred_out must_in pred_lbl =
    let pin =
      match CfgAnalysis.SM.find_opt pred_lbl must_in with
      | Some x -> x
      | None -> CfgAnalysis.SS.empty
    in
    let pdefs =
      match CfgAnalysis.SM.find_opt pred_lbl block_defs with
      | Some xs -> xs
      | None -> []
    in
    add_defs pin pdefs
  in
  let rec fix must_in =
    let changed = ref false in
    let next =
      List.fold_left
        (fun acc lbl ->
          let in_set =
            if lbl = fn.MicroIR.entry then
              CfgAnalysis.SS.empty
            else
              match CfgAnalysis.SM.find_opt lbl preds with
              | Some pred_set -> (
                  match CfgAnalysis.SS.elements pred_set with
                  | [] -> CfgAnalysis.SS.empty
                  | p :: ps ->
                      List.fold_left
                        (fun s p' -> CfgAnalysis.SS.inter s (pred_out must_in p'))
                        (pred_out must_in p)
                        ps)
              | None -> CfgAnalysis.SS.empty
          in
          let prev =
            match CfgAnalysis.SM.find_opt lbl must_in with
            | Some x -> x
            | None -> CfgAnalysis.SS.empty
          in
          if not (CfgAnalysis.SS.equal prev in_set) then changed := true;
          CfgAnalysis.SM.add lbl in_set acc)
        CfgAnalysis.SM.empty
        labels
    in
    if !changed then fix next else next
  in
  let must_in = fix init_must_in in
  let inputs =
    fn.MicroIR.blocks
    |> List.concat_map (fun blk ->
           let available =
             match CfgAnalysis.SM.find_opt blk.MicroIR.label must_in with
             | Some s -> s
             | None -> CfgAnalysis.SS.empty
           in
           local_uses_before_defs blk
           |> List.filter (fun v -> not (CfgAnalysis.SS.mem v available)))
    |> sort_uniq
  in
  let params = sort_uniq fn.MicroIR.params in
  let ambient_inputs =
    inputs |> List.filter (fun v -> not (List.mem v params))
  in
  let returns =
    match fn.MicroIR.ret, sort_uniq !returns with
    | Some r, rs when List.mem r rs -> Some r
    | Some r, _ -> Some r
    | None, [] -> None
    | None, r :: _ -> Some r
  in
  {
    fname = fn.MicroIR.fname;
    params;
    pre = sanitize_summary_vars ambient_inputs;
    post =
      ((if loops = [] then [] else [ "loops=" ^ string_of_int (List.length loops) ])
      @
      match returns with
      | Some r -> [ "ret=" ^ r ]
      | None -> []);
    modifies =
      !modifies
      |> List.filter (fun v -> not (List.mem v params))
      |> sanitize_summary_vars;
    returns;
    calls = sanitize_summary_vars !calls;
  }

let render s =
  Printf.sprintf
    "summary(%s): params=[%s] pre=[%s] post=[%s] modifies=[%s] calls=[%s]"
    s.fname
    (String.concat "," s.params)
    (String.concat "," s.pre)
    (String.concat "," s.post)
    (String.concat "," s.modifies)
    (String.concat "," s.calls)
