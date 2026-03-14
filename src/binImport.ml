module SM = Map.Make (String)
module SS = Set.Make (String)

exception Import_error of string

type asm_instr = {
  addr : string;
  text : string;
  mnemonic : string;
  operands : string list;
}

type abi =
  | X86_32
  | X86_64
  | ARM_32

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

let rec take n xs =
  match n, xs with
  | 0, _ | _, [] -> []
  | n, x :: rest -> x :: take (n - 1) rest

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
      (String.lowercase_ascii mnem, ops)

let parse_instr_line line =
  match String.index_opt line ':' with
  | None -> None
  | Some idx ->
      let lhs = String.sub line 0 idx |> trim in
      if
        lhs = ""
        || String.exists
             (fun c ->
               not
                 ((Char.code c >= Char.code '0' && Char.code c <= Char.code '9')
                 || (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'f')
                 || (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'F')))
             lhs
      then
        None
      else
        let rhs =
          String.sub line (idx + 1) (String.length line - idx - 1) |> split_tabs
        in
        match List.rev rhs with
        | asm :: _ ->
            let mnemonic, operands = parse_asm asm in
            if mnemonic = "" then None
            else Some { addr = lhs; text = asm; mnemonic; operands }
        | [] -> None

let contains_sub s sub =
  let ns = String.length s in
  let np = String.length sub in
  let rec loop i =
    if i + np > ns then false
    else if String.sub s i np = sub then true
    else loop (i + 1)
  in
  np = 0 || loop 0

let detect_abi path =
  let lines = run_lines ("readelf -h " ^ Filename.quote path) in
  let class_ =
    lines
    |> List.find_map (fun line ->
           if String.starts_with ~prefix:"  Class:" line then
             Some (trim (String.sub line 8 (String.length line - 8)))
           else
             None)
  in
  let machine =
    lines
    |> List.find_map (fun line ->
           if String.starts_with ~prefix:"  Machine:" line then
             Some (trim (String.sub line 10 (String.length line - 10)))
           else
             None)
  in
  match class_, machine with
  | Some cls, Some m ->
      let m = String.lowercase_ascii m in
      if cls = "ELF32" && contains_sub m "arm" then
        ARM_32
      else if cls = "ELF64" && (contains_sub m "x86-64" || contains_sub m "amd") then
        X86_64
      else if cls = "ELF32" && contains_sub m "80386" then
        X86_32
      else
        raise (Import_error ("unsupported machine: " ^ m))
  | _, Some m -> raise (Import_error ("unsupported machine: " ^ m))
  | _ -> raise (Import_error ("unable to detect ABI for " ^ path))

let disassemble_function abi path fname =
  let cmd =
    match abi with
    | X86_32 | X86_64 ->
        "objdump -d -M intel --disassemble=" ^ Filename.quote fname ^ " "
        ^ Filename.quote path
    | ARM_32 ->
        "objdump -d --disassemble=" ^ Filename.quote fname ^ " "
        ^ Filename.quote path
  in
  try run_lines cmd |> List.filter_map parse_instr_line with
  | Import_error _ -> (
      match abi with
      | ARM_32 ->
          raise
            (Import_error
               ("failed to disassemble ARM input with local objdump: " ^ path
              ^ ". This environment appears to lack an ARM-capable disassembler backend."))
      | _ -> raise (Import_error ("command failed: " ^ cmd)))

let normalize_int_token tok =
  let tok = trim tok in
  if tok = "" then tok
  else if tok.[0] = '#' then String.sub tok 1 (String.length tok - 1)
  else tok

let parse_int_opt tok =
  let tok = normalize_int_token tok in
  try
    if String.length tok > 3 && String.sub tok 0 3 = "-0x" then
      Some (-int_of_string ("0x" ^ String.sub tok 3 (String.length tok - 3)))
    else if String.length tok > 3 && String.sub tok 0 3 = "+0x" then
      Some (int_of_string ("0x" ^ String.sub tok 3 (String.length tok - 3)))
    else if String.length tok > 2 && String.sub tok 0 2 = "0x" then
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

let parse_disp s =
  let s = trim s in
  if s = "" then Some 0 else parse_int_opt s

let x86_frame_stack_offset frame_reg tok =
  let tok = clean_mem_operand tok in
  if String.length tok >= 5 && tok.[0] = '[' && tok.[String.length tok - 1] = ']'
  then
    let inner = String.sub tok 1 (String.length tok - 2) in
    if inner = frame_reg then Some 0
    else if String.length inner > String.length frame_reg + 1
            && String.sub inner 0 (String.length frame_reg + 1) = frame_reg ^ "+"
    then
      parse_disp
        (String.sub inner (String.length frame_reg + 1)
           (String.length inner - String.length frame_reg - 1))
    else if String.length inner > String.length frame_reg + 1
            && String.sub inner 0 (String.length frame_reg + 1) = frame_reg ^ "-"
    then
      parse_disp
        ("-"
        ^ String.sub inner (String.length frame_reg + 1)
            (String.length inner - String.length frame_reg - 1))
    else
      None
  else
    None

let canonical_x86_32_reg = function
  | "eax" | "ax" | "ah" | "al" -> Some "eax"
  | "ebx" | "bx" | "bh" | "bl" -> Some "ebx"
  | "ecx" | "cx" | "ch" | "cl" -> Some "ecx"
  | "edx" | "dx" | "dh" | "dl" -> Some "edx"
  | "esi" | "si" -> Some "esi"
  | "edi" | "di" -> Some "edi"
  | "ebp" | "bp" -> Some "ebp"
  | "esp" | "sp" -> Some "esp"
  | "eip" | "ip" -> Some "eip"
  | _ -> None

let canonical_x86_64_reg = function
  | "rax" | "eax" -> Some "rax"
  | "rbx" | "ebx" -> Some "rbx"
  | "rcx" | "ecx" -> Some "rcx"
  | "rdx" | "edx" -> Some "rdx"
  | "rsi" | "esi" -> Some "rsi"
  | "rdi" | "edi" -> Some "rdi"
  | "rbp" | "ebp" -> Some "rbp"
  | "rsp" | "esp" -> Some "rsp"
  | "r8" | "r8d" -> Some "r8"
  | "r9" | "r9d" -> Some "r9"
  | "r10" | "r10d" -> Some "r10"
  | "r11" | "r11d" -> Some "r11"
  | "r12" | "r12d" -> Some "r12"
  | "r13" | "r13d" -> Some "r13"
  | "r14" | "r14d" -> Some "r14"
  | "r15" | "r15d" -> Some "r15"
  | _ -> None

let canonical_arm_reg tok =
  match tok with
  | "r0" | "r1" | "r2" | "r3" | "r4" | "r5" | "r6" | "r7" | "r8" | "r9"
  | "r10" | "r11" | "r12" | "sp" | "lr" | "pc" ->
      Some tok
  | "fp" -> Some "r11"
  | "ip" -> Some "r12"
  | _ -> None

let canonical_reg abi tok =
  let tok = String.lowercase_ascii (normalize_int_token tok) in
  match abi with
  | X86_32 -> canonical_x86_32_reg tok
  | X86_64 -> canonical_x86_64_reg tok
  | ARM_32 -> canonical_arm_reg tok

let abi_param_regs = function
  | X86_32 -> []
  | X86_64 -> [ "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" ]
  | ARM_32 -> [ "r0"; "r1"; "r2"; "r3" ]

let abi_return_reg = function
  | X86_32 -> Some "eax"
  | X86_64 -> Some "rax"
  | ARM_32 -> Some "r0"

let abi_stack_reg = function
  | X86_32 -> "esp"
  | X86_64 -> "rsp"
  | ARM_32 -> "sp"

let abi_frame_reg = function
  | X86_32 -> Some "ebp"
  | X86_64 -> Some "rbp"
  | ARM_32 -> None

let is_memory_operand tok =
  let tok = clean_mem_operand tok in
  String.length tok >= 2 && tok.[0] = '[' && tok.[String.length tok - 1] = ']'

let tokenize_operand tok =
  let buf = Buffer.create (String.length tok) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> Buffer.add_char buf c
      | _ -> Buffer.add_char buf ' ')
    tok;
  Buffer.contents buf |> split_words |> List.map String.lowercase_ascii

let operand_registers abi tok =
  tokenize_operand tok
  |> List.filter_map (canonical_reg abi)
  |> List.sort_uniq String.compare

let frame_stack_offset abi tok =
  match abi_frame_reg abi with
  | Some reg -> x86_frame_stack_offset reg tok
  | None -> None

let extract_symbol_name tok =
  match String.index_opt tok '<', String.index_opt tok '>' with
  | Some i, Some j when i + 1 < j ->
      Some (String.sub tok (i + 1) (j - i - 1) |> String.lowercase_ascii)
  | _ -> None

let infer_x86_stack_params abi word_size instrs =
  let offsets =
    instrs
    |> List.concat_map (fun i -> i.operands)
    |> List.filter_map (frame_stack_offset abi)
    |> List.filter (fun off -> off >= word_size * 2 && off mod word_size = 0)
    |> List.sort_uniq Int.compare
  in
  let params = List.mapi (fun i _ -> Printf.sprintf "arg%d" i) offsets in
  List.combine offsets params

let infer_x86_stack_locals abi instrs =
  instrs
  |> List.concat_map (fun i -> i.operands)
  |> List.filter_map (frame_stack_offset abi)
  |> List.filter (fun off -> off < 0)
  |> List.sort_uniq Int.compare
  |> List.map (fun off -> (off, Printf.sprintf "local_%x" (abs off)))

let defs_of_instr abi i =
  let reg tok =
    match canonical_reg abi tok with
    | Some r -> [ r ]
    | None -> []
  in
  match i.mnemonic, i.operands with
  | ("mov" | "movzx" | "lea" | "ldr" | "ldrb"), dst :: _ when not (is_memory_operand dst) ->
      reg dst
  | ("add" | "sub" | "xor" | "and" | "or"), dst :: _ when not (is_memory_operand dst) ->
      reg dst
  | "pop", regs ->
      regs
      |> List.concat_map (operand_registers abi)
      |> List.sort_uniq String.compare
  | "call", _ | "callq", _ | "bl", _ | "blx", _ -> (
      match abi_return_reg abi with
      | Some r -> [ r ]
      | None -> [])
  | _ -> []

let uses_of_instr abi i =
  let regs tok = operand_registers abi tok in
  match i.mnemonic, i.operands with
  | "mov", [ dst; src ] when is_memory_operand dst -> regs dst @ regs src
  | "mov", [ _dst; src ] -> regs src
  | "lea", [ _dst; src ] -> regs src
  | "movzx", [ _dst; src ] -> regs src
  | ("add" | "sub" | "xor" | "and" | "or"), [ dst; src ] -> regs dst @ regs src
  | ("cmp" | "test"), [ a; b ] -> regs a @ regs b
  | ("ldr" | "ldrb"), [ _dst; src ] -> regs src
  | "str", [ src; dst ] -> regs src @ regs dst
  | "push", [ src ] -> regs src
  | "call", _ | "callq", _ | "bl", _ | "blx", _ -> abi_param_regs abi
  | _ -> List.concat_map regs i.operands

let infer_register_params abi instrs =
  let param_regs = abi_param_regs abi in
  let seen_defs = ref SS.empty in
  let used_params = ref SS.empty in
  List.iter
    (fun i ->
      uses_of_instr abi i
      |> List.iter (fun r ->
             if List.mem r param_regs && not (SS.mem r !seen_defs) then
               used_params := SS.add r !used_params);
      defs_of_instr abi i
      |> List.iter (fun r -> seen_defs := SS.add r !seen_defs))
    instrs;
  param_regs
  |> List.filter (fun r -> SS.mem r !used_params)
  |> List.mapi (fun i r -> (r, Printf.sprintf "arg%d" i))

let param_aliases abi instrs =
  match abi with
  | X86_32 -> infer_x86_stack_params abi 4 instrs |> List.map (fun (o, n) -> (`Stack o, n))
  | X86_64 | ARM_32 -> infer_register_params abi instrs |> List.map (fun (r, n) -> (`Reg r, n))

let param_names aliases = List.map snd aliases

let local_aliases abi instrs =
  match abi with
  | X86_32 | X86_64 ->
      infer_x86_stack_locals abi instrs |> List.map (fun (o, n) -> (`Stack o, n))
  | ARM_32 -> []

let lookup_stack_alias abi aliases tok =
  match frame_stack_offset abi tok with
  | Some off ->
      aliases
      |> List.find_map (function
           | `Stack k, name when k = off -> Some name
           | _ -> None)
  | None -> None

let lookup_reg_param abi aliases tok =
  match canonical_reg abi tok with
  | Some reg ->
      aliases
      |> List.find_map (function
           | `Reg r, name when r = reg -> Some name
           | _ -> None)
  | None -> None

let value_of_operand abi aliases tok =
  let tok = trim tok |> clean_mem_operand in
  if tok = "" then
    MicroIR.VUndef
  else
    match lookup_stack_alias abi aliases tok with
    | Some name -> MicroIR.VReg name
    | None ->
        if is_memory_operand tok then
          MicroIR.VReg ("mem_" ^ sanitize tok)
        else
          match parse_int_opt tok with
          | Some n -> MicroIR.VConst n
          | None -> (
              match canonical_reg abi tok with
              | Some r -> MicroIR.VReg r
              | None -> (
                  match extract_symbol_name tok with
                  | Some name -> MicroIR.VReg name
                  | None -> MicroIR.VReg (sanitize tok)))

let parse_mem_access abi aliases tok =
  let tok = clean_mem_operand tok in
  if is_memory_operand tok then
    let inner = String.sub tok 1 (String.length tok - 2) in
    match lookup_stack_alias abi aliases tok with
    | Some name -> MicroIR.VReg name
    | None -> (
        match canonical_reg abi inner with
        | Some r -> MicroIR.VReg r
        | None -> MicroIR.VReg (sanitize inner))
  else
    value_of_operand abi aliases tok

let callee_value abi aliases tok =
  match extract_symbol_name tok with
  | Some name -> MicroIR.VReg name
  | None -> value_of_operand abi aliases tok

let jcc_pred = function
  | "je" | "jz" | "beq" -> Some "eq"
  | "jne" | "jnz" | "bne" -> Some "ne"
  | "jl" | "jnge" | "jb" | "jc" | "jnae" | "blt" | "blo" -> Some "lt"
  | "jle" | "jng" | "jbe" | "jna" | "ble" | "bls" -> Some "le"
  | "jg" | "jnle" | "ja" | "jnbe" | "bgt" | "bhi" -> Some "gt"
  | "jge" | "jnl" | "jae" | "jnb" | "jnc" | "bge" | "bhs" -> Some "ge"
  | _ -> None

let target_of_operand op =
  match split_words op with
  | addr :: _ -> Some addr
  | [] -> None

let is_call_mnemonic = function
  | "call" | "callq" | "bl" | "blx" -> true
  | _ -> false

let is_return_instr = function
  | { mnemonic = "ret"; _ } -> true
  | { mnemonic = "bx"; operands = [ op ]; _ } ->
      String.lowercase_ascii (trim op) = "lr"
  | { mnemonic = "mov"; operands = [ dst; src ]; _ } ->
      String.lowercase_ascii (trim dst) = "pc"
      && String.lowercase_ascii (trim src) = "lr"
  | { mnemonic = "pop"; operands = ops; _ } ->
      List.exists
        (fun op ->
          let op = String.lowercase_ascii op in
          String.contains op 'p' && String.contains op 'c')
        ops
  | _ -> false

let is_unconditional_branch = function
  | { mnemonic = "jmp"; _ } | { mnemonic = "b"; _ } -> true
  | _ -> false

let pending_cmp_of_instr i =
  match i.mnemonic, i.operands with
  | ("cmp" | "test"), [ a; b ] -> Some (a, b)
  | ("cbz" | "cbnz"), [ a; _ ] -> Some (a, "0")
  | _ -> None

let branch_term abi aliases next_addr pending last =
  match last.mnemonic, last.operands with
  | ("cbz" | "cbnz"), [ reg; op ] ->
      let pred = if last.mnemonic = "cbz" then "eq" else "ne" in
      let lhs = value_of_operand abi aliases reg in
      let rhs = MicroIR.VConst 0 in
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
      Some (MicroIR.TBranch (MicroIR.ECmp (pred, lhs, rhs), tlabel, flabel))
  | m, op :: _ when jcc_pred m <> None ->
      let pred = Option.get (jcc_pred m) in
      let lhs, rhs =
        match pending with
        | Some (a, b) -> (value_of_operand abi aliases a, value_of_operand abi aliases b)
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
      Some (MicroIR.TBranch (MicroIR.ECmp (pred, lhs, rhs), tlabel, flabel))
  | _ -> None

let is_stack_setup abi = function
  | { mnemonic = ("sub" | "add" | "and"); operands = [ dst; _ ]; _ } -> (
      match canonical_reg abi dst with
      | Some r -> r = abi_stack_reg abi
      | None -> false)
  | _ -> false

let initial_param_moves abi param_aliases =
  param_aliases
  |> List.filter_map (function
       | `Reg reg, name ->
           Some (MicroIR.IAssign (reg, MicroIR.EVal (MicroIR.VReg name)))
       | `Stack _, _ -> None)

let translate_block_body abi aliases insns =
  let call_args_from_regs () =
    abi_param_regs abi |> List.map (fun r -> MicroIR.VReg r)
  in
  let rec loop pending_args acc = function
    | [] -> List.rev acc
    | i :: rest -> (
        match i.mnemonic, i.operands with
        | "push", [ src ] when abi = X86_32 ->
            loop (value_of_operand abi aliases src :: pending_args) acc rest
        | m, callee :: _ when is_call_mnemonic m ->
            let args =
              match abi with
              | X86_32 -> List.rev pending_args
              | X86_64 | ARM_32 -> call_args_from_regs ()
            in
            let ins =
              MicroIR.ICall (abi_return_reg abi, callee_value abi aliases callee, args)
            in
            loop [] (ins :: acc) rest
        | "mov", [ dst; src ] ->
            let ins =
              if is_memory_operand dst then
                Some
                  (MicroIR.IStore
                     (parse_mem_access abi aliases dst, value_of_operand abi aliases src))
              else if is_memory_operand src then
                Some
                  (MicroIR.IAssign
                     ( Option.value
                         (canonical_reg abi dst)
                         ~default:(sanitize dst),
                       MicroIR.ELoad (parse_mem_access abi aliases src) ))
              else
                Some
                  (MicroIR.IAssign
                     ( Option.value
                         (canonical_reg abi dst)
                         ~default:(sanitize dst),
                       MicroIR.EVal (value_of_operand abi aliases src) ))
            in
            loop [] (Option.get ins :: acc) rest
        | "lea", [ dst; src ] ->
            let ins =
              MicroIR.IAssign
                ( Option.value (canonical_reg abi dst) ~default:(sanitize dst),
                  MicroIR.EAddr (parse_mem_access abi aliases src, 0) )
            in
            loop [] (ins :: acc) rest
        | ("ldr" | "ldrb"), [ dst; src ] ->
            let ins =
              MicroIR.IAssign
                ( Option.value (canonical_reg abi dst) ~default:(sanitize dst),
                  MicroIR.ELoad (parse_mem_access abi aliases src) )
            in
            loop [] (ins :: acc) rest
        | "str", [ src; dst ] ->
            let ins =
              MicroIR.IStore
                (parse_mem_access abi aliases dst, value_of_operand abi aliases src)
            in
            loop [] (ins :: acc) rest
        | ("add" | "sub" | "and" as op), [ dst; src ] ->
            let dstv = Option.value (canonical_reg abi dst) ~default:(sanitize dst) in
            let ins =
              MicroIR.IAssign
                (dstv, MicroIR.EBinop (op, MicroIR.VReg dstv, value_of_operand abi aliases src))
            in
            if dstv = abi_stack_reg abi && pending_args <> [] then
              loop pending_args acc rest
            else
              loop [] (ins :: acc) rest
        | "xor", [ a; b ] when trim a = trim b ->
            let dst = Option.value (canonical_reg abi a) ~default:(sanitize a) in
            let ins = MicroIR.IAssign (dst, MicroIR.EVal (MicroIR.VConst 0)) in
            loop [] (ins :: acc) rest
        | "movzx", [ dst; src ] ->
            let ins =
              MicroIR.IAssign
                ( Option.value (canonical_reg abi dst) ~default:(sanitize dst),
                  MicroIR.ELoad (parse_mem_access abi aliases src) )
            in
            loop [] (ins :: acc) rest
        | _ ->
            let pending_args =
              if pending_args <> [] && not (is_stack_setup abi i) then [] else pending_args
            in
            loop pending_args acc rest)
  in
  loop [] [] insns

let build_blocks abi fname params ret param_aliases aliases instrs =
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
          if is_unconditional_branch i then
            match i.operands with
            | op :: _ -> (
                match target_of_operand op with
                | Some a -> leaders := SS.add a !leaders
                | None -> ())
            | _ -> ()
          else if jcc_pred i.mnemonic <> None || i.mnemonic = "cbz" || i.mnemonic = "cbnz" then
            match i.operands with
            | op :: _ ->
                let target =
                  match i.mnemonic with
                  | "cbz" | "cbnz" -> List.nth_opt i.operands 1
                  | _ -> Some op
                in
                (match target with
                | Some target_op -> (
                    match target_of_operand target_op with
                    | Some a -> leaders := SS.add a !leaders
                    | None -> ())
                | None -> ());
                (match SM.find_opt i.addr next_addr with
                | Some (Some a) -> leaders := SS.add a !leaders
                | _ -> ())
            | _ -> ()
          else if is_return_instr i then
            ()
          else
            ())
        instrs;
      let leader_set = !leaders in
      let rec take_block acc = function
        | [] -> (List.rev acc, [])
        | [ i ] -> (List.rev (i :: acc), [])
        | i :: (( _nxt :: _) as tl) ->
            if acc <> [] && SS.mem i.addr leader_set then
              (List.rev acc, i :: tl)
            else if
              is_unconditional_branch i || is_return_instr i || jcc_pred i.mnemonic <> None
              || i.mnemonic = "cbz" || i.mnemonic = "cbnz"
            then
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
      let entry_label = label_of_addr (List.hd instrs).addr in
      let mk_block insns =
        let entry = List.hd insns in
        let label = label_of_addr entry.addr in
        let pending =
          List.fold_left
            (fun cur i ->
              match pending_cmp_of_instr i with
              | Some x -> Some x
              | None -> cur)
            None insns
        in
        let body_core =
          insns |> List.rev |> List.tl |> List.rev |> translate_block_body abi aliases
        in
        let body =
          if label = entry_label then
            initial_param_moves abi param_aliases @ body_core
          else
            body_core
        in
        let last = List.hd (List.rev insns) in
        let term =
          if is_return_instr last then
            MicroIR.TReturn
              (Option.map (fun r -> MicroIR.VReg r) ret)
          else
            match last.mnemonic, last.operands with
            | m, _ when is_unconditional_branch last -> (
                match last.operands with
                | op :: _ -> (
                    match target_of_operand op with
                    | Some a -> MicroIR.TJump (label_of_addr a)
                    | None -> MicroIR.TStop)
                | _ -> MicroIR.TStop)
            | _ -> (
                match branch_term abi aliases next_addr pending last with
                | Some t -> t
                | None -> (
                    match SM.find_opt last.addr next_addr with
                    | Some (Some a) -> MicroIR.TJump (label_of_addr a)
                    | _ -> MicroIR.TStop))
        in
        { MicroIR.label = label; body; term }
      in
      let blocks = blocks [] instrs |> List.map mk_block in
      { MicroIR.fname = fname; entry = entry_label; params; ret; blocks }

let import ?(func = "main") path =
  let funcs = defined_functions path in
  if not (List.mem func funcs) then
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
    let abi = detect_abi path in
    let instrs = disassemble_function abi path func in
    let params_aliases = param_aliases abi instrs in
    let aliases = params_aliases @ local_aliases abi instrs in
    let params = param_names params_aliases in
    [ build_blocks abi func params (abi_return_reg abi) params_aliases aliases instrs ]
