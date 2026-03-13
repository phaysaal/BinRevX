type t = {
  header : string;
  facts : string list;
}

let sort_uniq xs = List.sort_uniq String.compare xs

let strip_parens s =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && s.[0] = '(' && s.[n - 1] = ')' then
    String.sub s 1 (n - 2)
  else
    s

let parse_guard guard =
  let toks =
    String.split_on_char ' ' guard
    |> List.filter (fun x -> x <> "")
    |> List.map strip_parens
  in
  match toks with
  | [ lhs; pred; rhs ] -> Some (lhs, pred, rhs)
  | _ -> None

let parse_transition_rhs s =
  let s = strip_parens s in
  match String.split_on_char ' ' s with
  | [ a; op; b ] -> Some (a, op, b)
  | _ -> None

let facts_from_guard (ls : LoopSummary.t) =
  match parse_guard ls.guard with
  | Some (lhs, "lt", rhs) -> [ lhs ^ " < " ^ rhs; lhs ^ " <= " ^ rhs ]
  | Some (lhs, "le", rhs) -> [ lhs ^ " <= " ^ rhs ]
  | Some (lhs, "gt", rhs) -> [ lhs ^ " > " ^ rhs; lhs ^ " >= " ^ rhs ]
  | Some (lhs, "ge", rhs) -> [ lhs ^ " >= " ^ rhs ]
  | Some (lhs, "eq", rhs) -> [ lhs ^ " = " ^ rhs ]
  | Some (lhs, "ne", rhs) -> [ lhs ^ " != " ^ rhs ]
  | _ -> []

let facts_from_transition (tr : LoopSummary.transition) =
  match parse_transition_rhs tr.after with
  | Some (a, "add", "1") when a = tr.var ->
      [ tr.var ^ " is monotone increasing" ]
  | Some (a, "sub", "1") when a = tr.var ->
      [ tr.var ^ " is monotone decreasing" ]
  | Some (a, "add", b) when a = tr.var && b <> tr.var ->
      [ tr.var ^ "' - " ^ tr.var ^ " = " ^ b ]
  | Some (a, "sub", b) when a = tr.var && b <> tr.var ->
      [ tr.var ^ "' - " ^ tr.var ^ " = -" ^ b ]
  | Some (a, "add", b) when a = tr.var ->
      [ tr.var ^ " evolves by self-plus " ^ b ]
  | Some (a, "add", b) when a <> tr.var ->
      [ tr.var ^ "' = " ^ a ^ " + " ^ b ]
  | _ -> []

let imply_nonneg (ls : LoopSummary.t) =
  let increasing =
    ls.LoopSummary.transitions
    |> List.exists (fun (tr : LoopSummary.transition) ->
           match parse_transition_rhs tr.after with
           | Some (a, "add", "1") when a = tr.var -> true
           | _ -> false)
  in
  match parse_guard ls.guard with
  | Some (lhs, "lt", rhs) when increasing ->
      [ lhs ^ " is bounded above by " ^ rhs ]
  | Some (lhs, "le", rhs) when increasing ->
      [ lhs ^ " is bounded above by " ^ rhs ]
  | Some (lhs, "gt", rhs) ->
      [ lhs ^ " is bounded below by " ^ rhs ]
  | Some (lhs, "ge", rhs) ->
      [ lhs ^ " is bounded below by " ^ rhs ]
  | _ -> []

let generate (ls : LoopSummary.t) =
  let facts =
    facts_from_guard ls
    @ List.concat_map facts_from_transition ls.transitions
    @ imply_nonneg ls
    |> sort_uniq
  in
  { header = ls.header; facts }

let render inv =
  match inv.facts with
  | [] -> Printf.sprintf "invariants(header=%s): none" inv.header
  | xs ->
      Printf.sprintf
        "invariants(header=%s): [%s]"
        inv.header
        (String.concat "; " xs)
