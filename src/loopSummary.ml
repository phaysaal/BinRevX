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

let summarize fn header_state (lp : LoopRecovery.loop_desc) =
  let bmap = CfgAnalysis.block_map fn in
  let header_state = generalize_header_state header_state lp in
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
  let transitions =
    lp.LoopRecovery.carried
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
  let transitions =
    transitions
    |> List.map (fun tr ->
           {
             tr with
             before = normalize_text alias_subs tr.before;
             after = normalize_text alias_subs tr.after;
           })
  in
  let hints =
    transitions
    |> List.filter_map classify_recurrence
    |> sort_uniq
  in
  {
    header = lp.LoopRecovery.header;
    guard =
      (match lp.LoopRecovery.guard with
      | Some g -> SymExec.sym_expr header_state g |> normalize_text alias_subs
      | None -> "loop_guard_unknown");
    modified = sort_uniq (List.map (fun tr -> tr.var) transitions);
    transitions;
    recurrence_hints = hints;
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
  Printf.sprintf
    "loop-summary(header=%s, guard=%s, modified=[%s], transitions=[%s], recurrences=[%s])"
    s.header
    s.guard
    (String.concat "," s.modified)
    tr_s
    hints
