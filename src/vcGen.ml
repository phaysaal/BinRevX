type obligation = {
  name : string;
  formula : string;
}

type t = {
  scope : string;
  obligations : obligation list;
}

let mk name formula = { name; formula }

let is_formal_fact s =
  not (String.contains s '\'')
  && not (String.contains s 'i' && String.contains s 's')

let looks_heuristic s =
  String.contains s ';'
  || String.contains s ':'
  || String.contains s '\''
  || String.contains s ','
  || String.contains s '['
  || String.contains s ']'

let has_phrase s phrase =
  let n = String.length s in
  let m = String.length phrase in
  let rec loop i =
    if i + m > n then false
    else if String.sub s i m = phrase then true
    else loop (i + 1)
  in
  loop 0

let conjunction xs =
  match List.filter (fun s -> s <> "") xs with
  | [] -> "true"
  | [ x ] -> x
  | xs -> "(" ^ String.concat " && " xs ^ ")"

let prime_expr s =
  let buf = Buffer.create (String.length s + 8) in
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let rec scan i =
    if i >= String.length s then ()
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          let j = ref i in
          while !j < String.length s && is_ident_char s.[!j] do
            incr j
          done;
          let tok = String.sub s i (!j - i) in
          Buffer.add_string buf (tok ^ "'");
          scan !j
      | c ->
          Buffer.add_char buf c;
          scan (i + 1)
  in
  scan 0;
  Buffer.contents buf

let formal_facts inv =
  inv.InvariantGen.facts
  |> List.filter (fun s ->
         not (has_phrase s " is ")
         && not (looks_heuristic s))

let loop_vcs (ls : LoopSummary.t) (inv : InvariantGen.t) =
  let inv_facts = formal_facts inv in
  let inv_c = conjunction inv_facts in
  let tr_c =
    ls.LoopSummary.transitions
    |> List.map (fun tr -> tr.LoopSummary.var ^ "' = " ^ tr.after)
    |> conjunction
  in
  let obligations =
    [
      mk
        "init"
        ("header_state(" ^ ls.header ^ ") => " ^ inv_c);
      mk
        "preservation"
        (conjunction [ inv_c; ls.guard; tr_c ] ^ " => " ^ prime_expr inv_c);
      mk
        "exit"
        (conjunction [ inv_c; "!(" ^ ls.guard ^ ")" ] ^ " => exit_state(" ^ ls.header ^ ")");
    ]
  in
  { scope = ls.header; obligations }

let render_obligation ob =
  ob.name ^ ": " ^ ob.formula

let render vcs =
  let body =
    vcs.obligations
    |> List.map render_obligation
    |> String.concat " | "
  in
  Printf.sprintf "vcs(scope=%s): %s" vcs.scope body
