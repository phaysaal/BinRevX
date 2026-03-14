type transition = {
  var : string;
  before : string;
  after : string;
}

type t = {
  header : string;
  guard : string;
  modified : string list;
  transitions : transition list;
  recurrence_hints : string list;
  induction_vars : string list;
  accumulators : string list;
  folded_mirrors : (string * string) list;
}

let sort_uniq xs = List.sort_uniq String.compare xs

let starts_with s prefix =
  let ns = String.length s in
  let np = String.length prefix in
  ns >= np && String.sub s 0 np = prefix

let replace_all s old repl =
  if old = "" then s
  else
    let ns = String.length s in
    let no = String.length old in
    let buf = Buffer.create ns in
    let rec loop i =
      if i >= ns then ()
      else if i + no <= ns && String.sub s i no = old then begin
        Buffer.add_string buf repl;
        loop (i + no)
      end else begin
        Buffer.add_char buf s.[i];
        loop (i + 1)
      end
    in
    loop 0;
    Buffer.contents buf

let normalize_text subs s =
  subs
  |> List.sort (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
  |> List.fold_left (fun acc (old, repl) -> replace_all acc old repl) s

let render_transition tr =
  Printf.sprintf "%s: %s -> %s" tr.var tr.before tr.after

let classify_recurrence tr =
  let self = tr.var in
  let plus_one = Printf.sprintf "(%s add 1)" self in
  let minus_one = Printf.sprintf "(%s sub 1)" self in
  if tr.after = plus_one then
    Some (Printf.sprintf "%s' = %s + 1" self self)
  else if tr.after = minus_one then
    Some (Printf.sprintf "%s' = %s - 1" self self)
  else if tr.after <> tr.before then
    Some (Printf.sprintf "%s' = %s" self tr.after)
  else
    None

let generalize_header_state st lp =
  lp.LoopRecovery.carried
  |> List.fold_left
       (fun st v -> SymState.bind st v v)
       st

let parse_rhs_expr s =
  let s = String.trim s in
  let n = String.length s in
  let s =
    if n >= 2 && s.[0] = '(' && s.[n - 1] = ')' then
      String.sub s 1 (n - 2)
    else
      s
  in
  match String.split_on_char ' ' s |> List.filter (fun x -> x <> "") with
  | [ a; op; b ] -> Some (a, op, b)
  | _ -> None

let header_aliases fn header =
  let bmap = CfgAnalysis.block_map fn in
  match CfgAnalysis.SM.find_opt header bmap with
  | None -> []
  | Some blk ->
      let term_uses =
        match blk.MicroIR.term with
        | MicroIR.TBranch (e, _, _) -> LoopRecovery.used_expr e
        | MicroIR.TSwitch (v, _, _) -> LoopRecovery.used_value v
        | _ -> []
      in
      blk.MicroIR.body
      |> List.filter_map (function
           | MicroIR.IAssign (dst, MicroIR.EVal (MicroIR.VReg src))
             when List.mem dst term_uses ->
               Some (dst, src)
           | _ -> None)

let guard_lhs guard =
  let s = String.trim guard in
  let n = String.length s in
  let s =
    if n >= 2 && s.[0] = '(' && s.[n - 1] = ')' then
      String.sub s 1 (n - 2)
    else
      s
  in
  match String.split_on_char ' ' s |> List.filter (fun x -> x <> "") with
  | lhs :: _ -> Some lhs
  | _ -> None

let detect_roles guard transitions folded_mirrors =
  let induction =
    transitions
    |> List.filter_map (fun tr ->
           match parse_rhs_expr tr.after with
           | Some (a, ("add" | "sub"), "1") when a = tr.var -> Some tr.var
           | _ -> None)
    |> sort_uniq
  in
  let induction =
    match guard_lhs guard with
    | Some lhs
      when starts_with lhs "local_" && not (List.mem lhs induction) ->
        sort_uniq (lhs :: induction)
    | _ -> induction
  in
  let accum =
    transitions
    |> List.filter_map (fun tr ->
           match parse_rhs_expr tr.after with
           | Some (a, ("add" | "sub"), b) when a = tr.var && b <> "1" -> Some tr.var
           | _ -> None)
    |> sort_uniq
  in
  (induction, accum)

let interesting_extra_vars header_state body_state aliases =
  let alias_sources = List.map snd aliases in
  let vars =
    SymExec.SM.bindings header_state.SymState.env
    |> List.map fst
    |> List.filter (fun v ->
           starts_with v "local_"
           || starts_with v "arg"
           || List.mem v alias_sources)
  in
  vars
  |> List.filter (fun v ->
         let before = SymExec.SM.find_opt v header_state.SymState.env in
         let after = SymExec.SM.find_opt v body_state.SymState.env in
         before <> after)
  |> sort_uniq

let body_assignment_patterns fn (lp : LoopRecovery.loop_desc) =
  let bmap = CfgAnalysis.block_map fn in
  lp.LoopRecovery.body_nodes
  |> List.filter (fun lbl -> lbl <> lp.LoopRecovery.header)
  |> List.filter_map (fun lbl -> CfgAnalysis.SM.find_opt lbl bmap)
  |> List.concat_map (fun blk ->
         blk.MicroIR.body
         |> List.filter_map (function
              | MicroIR.IAssign (dst, MicroIR.EBinop (op, MicroIR.VReg a, rhs))
                when dst = a ->
                  Some (dst, op, rhs)
              | _ -> None))
  |> List.sort_uniq compare

let summarize fn header_state (lp : LoopRecovery.loop_desc) =
  let bmap = CfgAnalysis.block_map fn in
  let header_state = generalize_header_state header_state lp in
  let aliases = header_aliases fn lp.LoopRecovery.header in
  let start = SymState.at header_state lp.LoopRecovery.header in
  let body_state =
    lp.LoopRecovery.body_nodes
    |> List.filter (fun lbl -> lbl <> lp.LoopRecovery.header)
    |> List.fold_left
         (fun st lbl ->
           match CfgAnalysis.SM.find_opt lbl bmap with
           | Some blk -> SymExec.exec_block_body st blk
           | None -> st)
         start
  in
  let transition_vars =
    sort_uniq (lp.LoopRecovery.carried @ interesting_extra_vars header_state body_state aliases)
  in
  let transitions0 =
    transition_vars
    |> List.map (fun v ->
           let before =
             match SymExec.SM.find_opt v header_state.SymState.env with
             | Some x -> x
             | None -> "sym_" ^ v
           in
           let after =
             match SymExec.SM.find_opt v body_state.SymState.env with
             | Some x -> x
             | None -> before
           in
           { var = v; before; after })
    |> List.filter (fun tr -> tr.after <> tr.before)
  in
  let syntactic_patterns = body_assignment_patterns fn lp in
  let transitions =
    let existing = transitions0 |> List.map (fun tr -> tr.var) in
    let synthetic =
      syntactic_patterns
      |> List.filter (fun (v, _, _) -> not (List.mem v existing))
      |> List.map (fun (v, op, rhs) ->
             let before =
               match SymExec.SM.find_opt v header_state.SymState.env with
               | Some x -> x
               | None -> v
             in
             let rhs_s =
               match rhs with
               | MicroIR.VReg r -> "sym_" ^ r
               | MicroIR.VConst n -> string_of_int n
               | MicroIR.VUndef -> "undef"
             in
             { var = v; before; after = Printf.sprintf "(%s %s %s)" v op rhs_s })
    in
    transitions0 @ synthetic
  in
  let modified_names =
    transitions |> List.map (fun tr -> tr.var) |> sort_uniq
  in
  let alias_subs =
    header_state.SymState.mem
    |> SymExec.SM.bindings
    |> List.filter_map (fun (k, v) ->
           if starts_with k "sym_local_" && starts_with v "sym_arg" then
             let local = String.sub k 4 (String.length k - 4) in
             if List.mem local modified_names then None else Some (k, v)
           else
             None)
  in
  let alias_subs =
    alias_subs
    @ List.concat_map (fun (dst, src) -> [ (dst, src); ("sym_" ^ dst, "sym_" ^ src) ]) aliases
  in
  let transitions =
    transitions
    |> List.map (fun tr ->
           {
             tr with
             before = normalize_text alias_subs tr.before;
             after = normalize_text alias_subs tr.after;
           })
    |> List.filter (fun tr ->
           match List.assoc_opt tr.var aliases with
           | Some src -> List.exists (fun tr2 -> tr2.var = src) transitions
           | None -> true)
  in
  let hints =
    transitions
    |> List.filter_map classify_recurrence
    |> sort_uniq
  in
  let folded_mirrors =
    aliases |> List.sort_uniq compare
  in
  let guard_rendered =
    match lp.LoopRecovery.guard with
    | Some g -> SymExec.sym_expr header_state g |> normalize_text alias_subs
    | None -> "loop_guard_unknown"
  in
  let induction_vars, accumulators = detect_roles guard_rendered transitions folded_mirrors in
  {
    header = lp.LoopRecovery.header;
    guard = guard_rendered;
    modified = sort_uniq (List.map (fun tr -> tr.var) transitions);
    transitions;
    recurrence_hints = hints;
    induction_vars;
    accumulators;
    folded_mirrors;
  }

let render s =
  let tr_s =
    s.transitions |> List.map render_transition |> String.concat "; "
  in
  let hints =
    match s.recurrence_hints with
    | [] -> "none"
    | xs -> String.concat "; " xs
  in
  let roles =
    [
      (if s.induction_vars = [] then None
       else Some ("induction=" ^ String.concat "," s.induction_vars));
      (if s.accumulators = [] then None
       else Some ("accumulators=" ^ String.concat "," s.accumulators));
      (if s.folded_mirrors = [] then None
       else
         Some
           ("folded="
           ^ String.concat ","
               (List.map (fun (a, b) -> a ^ "->" ^ b) s.folded_mirrors)));
    ]
    |> List.filter_map (fun x -> x)
    |> String.concat "; "
  in
  Printf.sprintf
    "loop-summary(header=%s, guard=%s, modified=[%s], transitions=[%s], recurrences=[%s], roles=[%s])"
    s.header
    s.guard
    (String.concat "," s.modified)
    tr_s
    hints
    roles
