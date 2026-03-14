module SM = Map.Make (String)
module SS = Set.Make (String)

type call_class =
  | DescendInternal
  | StubInternal
  | StubExternal

type return_state = {
  location : string;
  value : string option;
  state : SymState.t;
}

type call_site = {
  caller : string;
  block : string;
  instr_index : int;
  callee : string;
  args : string list;
  ret_dst : string option;
  state : SymState.t;
  class_ : call_class;
  resumed_state : SymState.t;
  summary_key : string option;
}

type func_result = {
  key : string;
  input_shape : string;
  func : MicroIR.func;
  summary : ModSummary.summary;
  entry_state : SymState.t;
  call_sites : call_site list;
  return_states : return_state list;
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

let is_driver_local_name name =
  name = "main"
  || name = "tester_main"
  || name = "warmup"
  || String.ends_with ~suffix:"_tester" name
  || String.starts_with ~prefix:"__x86.get_pc_thunk." name

let classify_call env ~entry ~caller callee =
  if not (is_internal_name env callee) then
    StubExternal
  else if callee = entry || callee = caller || is_driver_local_name callee then
    DescendInternal
  else
    StubInternal

let import_func env fname =
  match BinImport.import ~func:fname env.path with
  | [ fn ] -> fn
  | _ -> failwith ("unexpected import shape for " ^ fname)

let arg_shape_atom arg =
  if arg = "undef" then
    "undef"
  else if String.length arg > 0 && arg.[0] = '-' then
    "const"
  else if String.length arg > 0 && arg.[0] >= '0' && arg.[0] <= '9' then
    "const"
  else if String.starts_with ~prefix:"addr(" arg then
    "addr"
  else if String.starts_with ~prefix:"mem[" arg then
    "mem"
  else if String.contains arg '(' then
    "call"
  else if String.starts_with ~prefix:"sym_arg" arg then
    "arg"
  else if String.starts_with ~prefix:"sym_local_" arg || String.starts_with ~prefix:"local_" arg then
    "local"
  else if String.starts_with ~prefix:"sym_" arg then
    "sym"
  else
    "expr"

let input_shape args =
  match args with
  | [] -> "unit"
  | _ -> String.concat "," (List.map arg_shape_atom args)

let summary_key fname shape = fname ^ "|" ^ shape

let string_of_class = function
  | DescendInternal -> "descend-internal"
  | StubInternal -> "stub-internal"
  | StubExternal -> "stub-external"

let return_of_term st = function
  | MicroIR.TReturn None -> Some "void"
  | MicroIR.TReturn (Some v) -> Some (SymExec.sym_value st v)
  | _ -> None

let callee_seed fn summary args state =
  let seed = SymExec.from_summary summary in
  let rec zip xs ys =
    match xs, ys with
    | x :: xs, y :: ys -> (x, y) :: zip xs ys
    | _ -> []
  in
  let bindings = zip fn.MicroIR.params args in
  let st =
    List.fold_left (fun acc (p, a) -> SymState.bind acc p a) seed bindings
  in
  { st with SymState.mem = state.SymState.mem; location = fn.MicroIR.entry }

let sort_uniq xs = List.sort_uniq String.compare xs

let joined_return_value (rets : return_state list) =
  match rets |> List.filter_map (fun r -> r.value) |> sort_uniq with
  | [] -> None
  | [ v ] -> Some v
  | vs -> Some ("phi(" ^ String.concat " | " vs ^ ")")

let function_like name =
  String.ends_with ~suffix:"_new" name
  || String.ends_with ~suffix:"alloc" name
  || String.contains name '_'

let havoc_if_present st name =
  if SM.mem name st.SymState.env then
    SymState.bind st name ("havoc_" ^ name)
  else
    st

let apply_modifies st summary =
  List.fold_left
    (fun acc v ->
      if v = "memory" then
        { acc with SymState.mem = SM.empty }
      else if String.starts_with ~prefix:"local_" v || String.starts_with ~prefix:"arg" v then
        acc
      else
        havoc_if_present acc v)
    st
    summary.ModSummary.modifies

let alloc_like callee =
  String.ends_with ~suffix:"_new" callee
  || String.ends_with ~suffix:"malloc" callee
  || String.ends_with ~suffix:"alloc" callee
  || String.starts_with ~prefix:"bio_new" callee

let free_like callee =
  String.ends_with ~suffix:"_free" callee || String.contains callee 'f' && String.starts_with ~prefix:"free" callee

let read_like callee =
  String.contains callee '_'
  && (String.ends_with ~suffix:"_read" callee
     || String.starts_with ~prefix:"read" callee
     || String.starts_with ~prefix:"decode_" callee
     || String.starts_with ~prefix:"pem_read_" callee)

let write_like callee =
  String.contains callee '_'
  && (String.ends_with ~suffix:"_write" callee
     || String.ends_with ~suffix:"_sign" callee
     || String.starts_with ~prefix:"encode_" callee
     || String.starts_with ~prefix:"pem_write_" callee)

let pure_like callee =
  String.ends_with ~suffix:"_size" callee
  || String.ends_with ~suffix:"_len" callee
  || String.starts_with ~prefix:"printf" callee
  || String.starts_with ~prefix:"putchar" callee
  || String.starts_with ~prefix:"fwrite" callee

let apply_stub_summary st ret_dst callee args =
  let ret_value =
    if alloc_like callee then
      Some ("alloc_" ^ callee ^ "(" ^ String.concat ", " args ^ ")")
    else if free_like callee then
      None
    else
      Some (callee ^ "(" ^ String.concat ", " args ^ ")")
  in
  let st =
    if free_like callee then
      { st with SymState.mem = SM.empty }
    else if read_like callee || write_like callee then
      SymState.store st "memory" ("havoc_" ^ callee)
    else if pure_like callee || function_like callee then
      st
    else
      SymState.store st "memory" ("havoc_" ^ callee)
  in
  match ret_dst, ret_value with
  | Some dst, Some v -> SymState.bind st dst v
  | _ -> st

let apply_internal_summary st ret_dst (result : func_result) =
  let st = apply_modifies st result.summary in
  let st =
    match ret_dst, joined_return_value result.return_states with
    | Some dst, Some v -> SymState.bind st dst v
    | _ -> st
  in
  if List.length result.return_states > 1 then
    SymState.add_path st ("summary(" ^ result.key ^ ")")
  else
    st

let recursive_stub_result key fn summary entry_state =
  {
    key;
    input_shape = "recursive";
    func = fn;
    summary;
    entry_state;
    call_sites = [];
    return_states =
      [
        {
          location = fn.MicroIR.entry;
          value = Some ("recursive_" ^ fn.MicroIR.fname);
          state = entry_state;
        };
      ];
  }

let analyze_binary ?(max_functions = 32) ~path ~entry () =
  let env =
    { path; defined = BinImport.defined_functions path |> List.to_seq |> SS.of_seq }
  in
  let results = ref SM.empty in
  let order = ref [] in
  let active = ref SS.empty in
  let truncated = ref false in
  let max_label_visits = 2 in
  let rec ensure ~seed fname args =
    let shape = input_shape args in
    let key = summary_key fname shape in
    match SM.find_opt key !results with
    | Some r -> r
    | None when SS.mem key !active ->
        let fn = import_func env fname in
        let summary = ModSummary.infer fn in
        recursive_stub_result key fn summary seed
    | None ->
        if List.length !order >= max_functions then begin
          truncated := true;
          let fn = import_func env fname in
          let summary = ModSummary.infer fn in
          recursive_stub_result key fn summary seed
        end else begin
          active := SS.add key !active;
          let fn = import_func env fname in
          let summary = ModSummary.infer fn in
          let entry_state =
            if fname = entry && args = [] then
              SymState.at (SymExec.from_summary summary) fn.MicroIR.entry
            else
              seed
          in
          let call_sites = ref [] in
          let return_states = ref [] in
          let blocks = CfgAnalysis.block_map fn in
          let visited = ref SM.empty in
          let rec traverse lbl st =
            let count =
              match SM.find_opt lbl !visited with
              | Some n -> n
              | None -> 0
            in
            if count >= max_label_visits then
              ()
            else begin
              visited := SM.add lbl (count + 1) !visited;
              match SM.find_opt lbl blocks with
              | None -> ()
              | Some blk -> exec_instrs blk 0 (SymState.at st lbl)
            end
          and exec_instrs blk idx st =
            if idx >= List.length blk.MicroIR.body then
              exec_term blk st
            else
              match List.nth blk.MicroIR.body idx with
              | MicroIR.ICall (ret, MicroIR.VReg callee, call_args) ->
                  let call_state = SymState.at st blk.MicroIR.label in
                  let arg_values = List.map (SymExec.sym_value st) call_args in
                  let class_ = classify_call env ~entry ~caller:fn.MicroIR.fname callee in
                  let resumed_state, subkey =
                    match class_ with
                    | DescendInternal ->
                        let callee_fn = import_func env callee in
                        let callee_summary = ModSummary.infer callee_fn in
                        let callee_state = callee_seed callee_fn callee_summary arg_values call_state in
                        let callee_result = ensure ~seed:callee_state callee arg_values in
                        (apply_internal_summary st ret callee_result, Some callee_result.key)
                    | StubInternal | StubExternal ->
                        (apply_stub_summary st ret callee arg_values, None)
                  in
                  call_sites :=
                    {
                      caller = fn.MicroIR.fname;
                      block = blk.MicroIR.label;
                      instr_index = idx;
                      callee;
                      args = arg_values;
                      ret_dst = ret;
                      state = call_state;
                      class_;
                      resumed_state = SymState.at resumed_state blk.MicroIR.label;
                      summary_key = subkey;
                    }
                    :: !call_sites;
                  exec_instrs blk (idx + 1) resumed_state
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
                  |> fun st' -> SymState.add_path st' (SymExec.sym_expr st (MicroIR.negate_expr g))
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
            | MicroIR.TReturn _ ->
                return_states :=
                  {
                    location = blk.MicroIR.label;
                    value = return_of_term st blk.MicroIR.term;
                    state = SymState.at st blk.MicroIR.label;
                  }
                  :: !return_states
            | MicroIR.TStop -> ()
          in
          traverse fn.MicroIR.entry (SymState.at entry_state fn.MicroIR.entry);
          let result =
            {
              key;
              input_shape = shape;
              func = fn;
              summary;
              entry_state;
              call_sites = List.rev !call_sites;
              return_states = List.rev !return_states;
            }
          in
          results := SM.add key result !results;
          order := !order @ [ key ];
          active := SS.remove key !active;
          result
        end
  in
  ignore (ensure ~seed:SymState.empty entry []);
  { entry; order = !order; results = !results; truncated = !truncated }

let render_return_state rs =
  Printf.sprintf
    "return(loc=%s, value=%s, state=%s)"
    rs.location
    (match rs.value with Some v -> v | None -> "void")
    (SymState.render rs.state)

let render_call_site cs =
  Printf.sprintf
    "callsite(block=%s, idx=%d, callee=%s, class=%s, key=%s, ret=%s, args=[%s], state=%s, resumed=%s)"
    cs.block cs.instr_index cs.callee (string_of_class cs.class_)
    (match cs.summary_key with Some k -> k | None -> "-")
    (match cs.ret_dst with Some r -> r | None -> "void")
    (String.concat ", " cs.args)
    (SymState.render cs.state)
    (SymState.render cs.resumed_state)

let render_result r =
  let calls =
    if r.call_sites = [] then
      "    none"
    else
      r.call_sites
      |> List.map (fun cs -> "    " ^ render_call_site cs)
      |> String.concat "\n"
  in
  let returns =
    if r.return_states = [] then
      "    none"
    else
      r.return_states
      |> List.map (fun rs -> "    " ^ render_return_state rs)
      |> String.concat "\n"
  in
  String.concat "\n"
    [
      Printf.sprintf "Function %s [%s]:" r.func.MicroIR.fname r.key;
      Printf.sprintf "  input-shape=%s" r.input_shape;
      Printf.sprintf
        "  signature: params=[%s] ret=%s"
        (String.concat "," r.func.MicroIR.params)
        (match r.func.MicroIR.ret with Some r -> r | None -> "void");
      "  " ^ ModSummary.render r.summary;
      "  entry-state:";
      "    " ^ SymState.render r.entry_state;
      "  call-sites:";
      calls;
      "  returns:";
      returns;
    ]

let render a =
  let body =
    a.order
    |> List.filter_map (fun key -> SM.find_opt key a.results)
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
