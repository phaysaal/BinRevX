module SM = Map.Make (String)
module SS = Set.Make (String)

exception Import_error of string

type asm_instr = {
  addr : string;
  text : string;
  mnemonic : string;
  operands : string list;
}

let trim = String.trim

let split_words s =
  s |> trim |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let sanitize s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Buffer.add_char buf c
      | _ -> Buffer.add_char buf '_')
    s;
  Buffer.contents buf

let label_of_addr a = "L_" ^ sanitize a

let run_lines cmd =
  let ic = Unix.open_process_in cmd in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        (match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> List.rev acc
        | _ -> raise (Import_error ("command failed: " ^ cmd)))
  in
  loop []

let defined_functions path =
  let cmd = "nm -C --defined-only " ^ Filename.quote path in
  run_lines cmd
  |> List.filter_map (fun line ->
         match split_words line with
         | [ _addr; _kind; name ] -> Some name
         | _ -> None)

let split_tabs s =
  String.split_on_char '\t' s |> List.filter (fun x -> trim x <> "")

let parse_asm text =
  match String.split_on_char ' ' (trim text) |> List.filter (fun x -> x <> "") with
  | [] -> ("", [])
  | mnem :: rest ->
      let ops =
        String.concat " " rest
        |> String.split_on_char ','
        |> List.map trim
        |> List.filter (fun x -> x <> "")
      in
      (mnem, ops)

let parse_instr_line line =
  match String.index_opt line ':' with
  | None -> None
  | Some idx ->
      let lhs = String.sub line 0 idx |> trim in
      if lhs = "" || String.exists (fun c -> not (Char.code c >= Char.code '0' && Char.code c <= Char.code '9'
                                                  || Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f'
                                                  || Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F')) lhs
      then None
      else
        let rhs =
          String.sub line (idx + 1) (String.length line - idx - 1)
          |> split_tabs
        in
        match List.rev rhs with
        | asm :: _ ->
            let mnemonic, operands = parse_asm asm in
            if mnemonic = "" then None
            else Some { addr = lhs; text = asm; mnemonic; operands }
        | [] -> None

let disassemble_function path fname =
  let cmd =
    "objdump -d -M intel --disassemble=" ^ Filename.quote fname ^ " " ^ Filename.quote path
  in
  run_lines cmd |> List.filter_map parse_instr_line

let parse_int_opt tok =
  try
    if String.length tok > 2 && String.sub tok 0 2 = "0x" then
      Some (int_of_string tok)
    else
      Some (int_of_string tok)
  with _ -> None

let drop_prefix pref s =
  let n = String.length pref in
  if String.length s >= n && String.sub s 0 n = pref then
    String.sub s n (String.length s - n)
  else
    s

let clean_mem_operand s =
  s
  |> drop_prefix "BYTE PTR "
  |> drop_prefix "DWORD PTR "
  |> drop_prefix "QWORD PTR "
  |> drop_prefix "WORD PTR "
  |> trim

let parse_value tok =
  let tok = trim tok in
  let tok = clean_mem_operand tok in
  if tok = "" then MicroIR.VUndef
  else if tok.[0] = '[' then
    MicroIR.VReg ("mem_" ^ sanitize tok)
  else
    match parse_int_opt tok with
    | Some n -> MicroIR.VConst n
    | None -> MicroIR.VReg (sanitize tok)

let parse_mem_access tok =
  let tok = clean_mem_operand tok in
  if String.length tok >= 2 && tok.[0] = '[' && tok.[String.length tok - 1] = ']'
  then
    let inner = String.sub tok 1 (String.length tok - 2) in
    MicroIR.VReg (sanitize inner)
  else
    parse_value tok

let jcc_pred = function
  | "je" | "jz" -> Some "eq"
  | "jne" | "jnz" -> Some "ne"
  | "jl" | "jnge" -> Some "lt"
  | "jle" | "jng" -> Some "le"
  | "jg" | "jnle" -> Some "gt"
  | "jge" | "jnl" -> Some "ge"
  | "jb" | "jc" | "jnae" -> Some "lt"
  | "jbe" | "jna" -> Some "le"
  | "ja" | "jnbe" -> Some "gt"
  | "jae" | "jnb" | "jnc" -> Some "ge"
  | _ -> None

let target_of_operand op =
  match split_words op with
  | addr :: _ -> Some addr
  | [] -> None

let pending_cmp_of_instr = function
  | { mnemonic = "cmp"; operands = [ a; b ]; _ } ->
      Some (parse_value a, parse_value b)
  | { mnemonic = "test"; operands = [ a; b ]; _ } when trim a = trim b ->
      Some (parse_value a, MicroIR.VConst 0)
  | _ -> None

let translate_nonterm i =
  match i.mnemonic, i.operands with
  | "mov", [ dst; src ] ->
      Some (MicroIR.IAssign (sanitize dst, MicroIR.EVal (parse_value src)))
  | "lea", [ dst; src ] ->
      Some (MicroIR.IAssign (sanitize dst, MicroIR.EAddr (parse_mem_access src, 0)))
  | ("add" | "sub" as op), [ dst; src ] ->
      let dstv = sanitize dst in
      Some (MicroIR.IAssign (dstv, MicroIR.EBinop (op, MicroIR.VReg dstv, parse_value src)))
  | "xor", [ a; b ] when trim a = trim b ->
      Some (MicroIR.IAssign (sanitize a, MicroIR.EVal (MicroIR.VConst 0)))
  | "movzx", [ dst; src ] ->
      Some (MicroIR.IAssign (sanitize dst, MicroIR.ELoad (parse_mem_access src)))
  | "call", callee :: _ ->
      Some (MicroIR.ICall (None, parse_value callee, []))
  | _ -> None

let build_blocks fname instrs =
  match instrs with
  | [] -> raise (Import_error ("no disassembly for function " ^ fname))
  | _ ->
      let addrs = List.map (fun i -> i.addr) instrs in
      let next_addr =
        let rec pairs acc = function
          | a :: (b :: _ as tl) -> pairs ((a, Some b) :: acc) tl
          | [ a ] -> List.rev ((a, None) :: acc)
          | [] -> List.rev acc
        in
        pairs [] addrs |> List.to_seq |> SM.of_seq
      in
      let leaders = ref (SS.singleton (List.hd addrs)) in
      List.iter
        (fun i ->
          match i.mnemonic with
          | "jmp" -> (
              match i.operands with
              | op :: _ -> (
                  match target_of_operand op with
                  | Some a -> leaders := SS.add a !leaders
                  | None -> ())
              | _ -> ())
          | m when jcc_pred m <> None -> (
              match i.operands with
              | op :: _ ->
                  (match target_of_operand op with
                  | Some a -> leaders := SS.add a !leaders
                  | None -> ());
                  (match SM.find_opt i.addr next_addr with
                  | Some (Some a) -> leaders := SS.add a !leaders
                  | _ -> ())
              | _ -> ())
          | "ret" ->
              (match SM.find_opt i.addr next_addr with
              | Some (Some a) -> leaders := SS.add a !leaders
              | _ -> ())
          | _ -> ())
        instrs;
      let leader_set = !leaders in
      let rec take_block acc = function
        | [] -> (List.rev acc, [])
        | [ i ] -> (List.rev (i :: acc), [])
        | i :: ((nxt :: _) as tl) ->
            if acc <> [] && SS.mem i.addr leader_set then
              (List.rev acc, i :: tl)
            else if i.mnemonic = "jmp" || i.mnemonic = "ret" || jcc_pred i.mnemonic <> None then
              (List.rev (i :: acc), tl)
            else
              take_block (i :: acc) tl
      in
      let rec blocks acc = function
        | [] -> List.rev acc
        | insns ->
            let blk_insns, rest = take_block [] insns in
            blocks (blk_insns :: acc) rest
      in
      let mk_block insns =
        let entry = List.hd insns in
        let label = label_of_addr entry.addr in
        let pending =
          List.fold_left
            (fun cur i ->
              match pending_cmp_of_instr i with
              | Some x -> Some x
              | None -> cur)
            None
            insns
        in
        let body =
          insns
          |> List.rev |> List.tl |> List.rev
          |> List.filter_map translate_nonterm
        in
        let last = List.hd (List.rev insns) in
        let term =
          match last.mnemonic, last.operands with
          | "ret", _ -> MicroIR.TReturn None
          | "jmp", op :: _ -> (
              match target_of_operand op with
              | Some a -> MicroIR.TJump (label_of_addr a)
              | None -> MicroIR.TStop)
          | m, op :: _ when jcc_pred m <> None -> (
              let pred = Option.get (jcc_pred m) in
              let lhs, rhs =
                match pending with
                | Some (a, b) -> (a, b)
                | None -> (MicroIR.VReg "flag", MicroIR.VConst 0)
              in
              let tlabel =
                match target_of_operand op with
                | Some a -> label_of_addr a
                | None -> "unknown_true"
              in
              let flabel =
                match SM.find_opt last.addr next_addr with
                | Some (Some a) -> label_of_addr a
                | _ -> "unknown_false"
              in
              MicroIR.TBranch (MicroIR.ECmp (pred, lhs, rhs), tlabel, flabel))
          | _ -> (
              match SM.find_opt last.addr next_addr with
              | Some (Some a) -> MicroIR.TJump (label_of_addr a)
              | _ -> MicroIR.TStop)
        in
        { MicroIR.label = label; body; term }
      in
      let blocks = blocks [] instrs |> List.map mk_block in
      {
        MicroIR.fname = fname;
        entry = label_of_addr (List.hd instrs).addr;
        blocks;
      }

let import ?(func = "main") path =
  let funcs = defined_functions path in
  if not (List.mem func funcs) then
    let rec take n xs =
      match n, xs with
      | 0, _ | _, [] -> []
      | n, x :: rest -> x :: take (n - 1) rest
    in
    let sample =
      funcs
      |> List.sort_uniq String.compare
      |> List.filter (fun f -> f = "main" || f = "tester_main" || String.length f < 32)
      |> take 12
    in
    raise
      (Import_error
         ("function not found in symbol table: " ^ func ^ ". try one of: "
        ^ String.concat ", " sample))
  else
    let instrs = disassemble_function path func in
    [ build_blocks func instrs ]
