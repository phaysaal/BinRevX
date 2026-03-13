exception Parse_error of string

let trim = String.trim

let split_words s =
  s |> trim |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let starts_with s prefix =
  let ns = String.length s in
  let np = String.length prefix in
  ns >= np && String.sub s 0 np = prefix

let strip_comment s =
  match String.index_opt s '#' with
  | Some i -> String.sub s 0 i
  | None -> s

let clean_line s = s |> strip_comment |> trim

let parse_value = function
  | "undef" -> MicroIR.VUndef
  | tok -> (
      try MicroIR.VConst (int_of_string tok) with
      | Failure _ -> MicroIR.VReg tok)

let parse_expr = function
  | [ "const"; n ] -> MicroIR.EVal (MicroIR.VConst (int_of_string n))
  | [ "reg"; v ] -> MicroIR.EVal (MicroIR.VReg v)
  | [ "val"; v ] -> MicroIR.EVal (parse_value v)
  | [ "load"; v ] -> MicroIR.ELoad (parse_value v)
  | [ "addr"; v; off ] -> MicroIR.EAddr (parse_value v, int_of_string off)
  | [ "index"; base; idx; width ] ->
      MicroIR.EIndex (parse_value base, parse_value idx, int_of_string width)
  | [ "field"; base; fld ] ->
      MicroIR.EField (parse_value base, fld)
  | [ "binop"; op; a; b ] -> MicroIR.EBinop (op, parse_value a, parse_value b)
  | [ "cmp"; pred; a; b ] -> MicroIR.ECmp (pred, parse_value a, parse_value b)
  | xs ->
      raise
        (Parse_error
           ("cannot parse expr from tokens: " ^ String.concat " " xs))

let parse_instr line =
  match split_words line with
  | "assign" :: dst :: rest -> MicroIR.IAssign (dst, parse_expr rest)
  | [ "store"; src; dst ] -> MicroIR.IStore (parse_value dst, parse_value src)
  | "assume" :: rest -> MicroIR.IAssume (parse_expr rest)
  | "assert" :: rest -> MicroIR.IAssert (parse_expr rest)
  | "call" :: dst :: callee :: args ->
      let ret = if dst = "-" then None else Some dst in
      MicroIR.ICall (ret, parse_value callee, List.map parse_value args)
  | xs ->
      raise
        (Parse_error
           ("cannot parse instruction from tokens: " ^ String.concat " " xs))

let parse_terminator line =
  match split_words line with
  | [ "term"; "jump"; lbl ] -> MicroIR.TJump lbl
  | [ "term"; "branch"; pred; a; b; t; f ] ->
      MicroIR.TBranch (MicroIR.ECmp (pred, parse_value a, parse_value b), t, f)
  | "term" :: "switch" :: v :: dflt :: ncases :: rest ->
      let n = int_of_string ncases in
      let rec take_cases k xs acc =
        if k = 0 then (List.rev acc, xs)
        else
          match xs with
          | c :: lbl :: tl -> take_cases (k - 1) tl ((int_of_string c, lbl) :: acc)
          | _ -> raise (Parse_error "malformed switch cases")
      in
      let cases, leftover = take_cases n rest [] in
      if leftover <> [] then raise (Parse_error "extra tokens after switch terminator");
      MicroIR.TSwitch (parse_value v, cases, dflt)
  | [ "term"; "ret"; "void" ] -> MicroIR.TReturn None
  | [ "term"; "ret"; v ] -> MicroIR.TReturn (Some (parse_value v))
  | [ "term"; "stop" ] -> MicroIR.TStop
  | xs ->
      raise
        (Parse_error
           ("cannot parse terminator from tokens: " ^ String.concat " " xs))

type block_acc = {
  label : string;
  body_rev : MicroIR.instr list;
}

type func_acc = {
  fname : string;
  entry : string;
  blocks_rev : MicroIR.block list;
  current : block_acc option;
}

let finish_block current term =
  {
    MicroIR.label = current.label;
    body = List.rev current.body_rev;
    term;
  }

let finish_func acc =
  match acc.current with
  | Some _ -> raise (Parse_error ("unterminated block in function " ^ acc.fname))
  | None ->
      {
        MicroIR.fname = acc.fname;
        entry = acc.entry;
        blocks = List.rev acc.blocks_rev;
      }

let parse_func_header = function
  | [ "func"; fname; "entry"; entry ] -> (fname, entry)
  | xs ->
      raise
        (Parse_error
           ("expected 'func <name> entry <label>', got: " ^ String.concat " " xs))

let read_lines path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

let load path =
  let lines = read_lines path in
  let rec loop funcs current = function
    | [] -> (
        match current with
        | Some acc -> List.rev (finish_func acc :: funcs)
        | None -> List.rev funcs)
    | raw :: rest ->
        let line = clean_line raw in
        if line = "" then
          loop funcs current rest
        else if starts_with line "func " then
          let current_funcs =
            match current with
            | Some acc -> finish_func acc :: funcs
            | None -> funcs
          in
          let fname, entry = parse_func_header (split_words line) in
          loop current_funcs (Some { fname; entry; blocks_rev = []; current = None }) rest
        else if line = "endfunc" then
          match current with
          | Some acc -> loop (finish_func acc :: funcs) None rest
          | None -> raise (Parse_error "endfunc without active function")
        else
          match current with
          | None ->
              raise
                (Parse_error
                   ("content outside of function: " ^ line))
          | Some acc ->
              if starts_with line "block " then
                match split_words line with
                | [ "block"; label ] ->
                    if acc.current <> None then
                      raise (Parse_error ("nested block in function " ^ acc.fname));
                    let next = { acc with current = Some { label; body_rev = [] } } in
                    loop funcs (Some next) rest
                | _ -> raise (Parse_error ("bad block header: " ^ line))
              else if line = "endblock" then
                raise (Parse_error "endblock must be replaced by a term line")
              else if starts_with line "term " then
                match acc.current with
                | None ->
                    raise (Parse_error ("terminator outside block in " ^ acc.fname))
                | Some blk ->
                    let term = parse_terminator line in
                    let block = finish_block blk term in
                    let next =
                      {
                        acc with
                        blocks_rev = block :: acc.blocks_rev;
                        current = None;
                      }
                    in
                    loop funcs (Some next) rest
              else
                match acc.current with
                | None ->
                    raise (Parse_error ("instruction outside block in " ^ acc.fname))
                | Some blk ->
                    let instr = parse_instr line in
                    let next_blk = { blk with body_rev = instr :: blk.body_rev } in
                    loop funcs (Some { acc with current = Some next_blk }) rest
  in
  loop [] None lines
