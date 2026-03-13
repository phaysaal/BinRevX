type kind =
  | ArrayLoop
  | StringLoop
  | LinkedListLoop
  | UnknownLoop

type evidence = {
  stable_index_access : bool;
  byte_access : bool;
  field_access : bool;
  sentinel_zero_guard : bool;
  null_guard : bool;
  arithmetic_stride : bool;
  pointer_chase : bool;
  cursor : string option;
  index_base : string option;
  element_width : int option;
  field_name : string option;
  guard_var : string option;
}

type t = {
  kind : kind;
  scores : (string * int) list;
  evidence : evidence;
  reasons : string list;
  derived_facts : string list;
}

let empty_evidence =
  {
    stable_index_access = false;
    byte_access = false;
    field_access = false;
    sentinel_zero_guard = false;
    null_guard = false;
    arithmetic_stride = false;
    pointer_chase = false;
    cursor = None;
    index_base = None;
    element_width = None;
    field_name = None;
    guard_var = None;
  }

let kind_name = function
  | ArrayLoop -> "array"
  | StringLoop -> "string"
  | LinkedListLoop -> "linked-list"
  | UnknownLoop -> "unknown"

let is_stride_transition (tr : LoopSummary.transition) =
  tr.after = Printf.sprintf "(%s add 1)" tr.var
  || tr.after = Printf.sprintf "(%s sub 1)" tr.var

let is_pointer_chase_transition (tr : LoopSummary.transition) =
  let prefix1 = "field(" ^ tr.var ^ ",next)" in
  let prefix2 = "field(" ^ tr.var ^ ","
  in
  String.length tr.after >= String.length prefix2
  && (tr.after = prefix1 || String.sub tr.after 0 (String.length prefix2) = prefix2)

let uniq xs = List.sort_uniq String.compare xs

let pp_opt f = function
  | Some x -> f x
  | None -> "?"

let has_phrase s phrase =
  let n = String.length s and m = String.length phrase in
  let rec loop i =
    if i + m > n then false
    else if String.sub s i m = phrase then true
    else loop (i + 1)
  in
  loop 0

let value_name = function
  | MicroIR.VReg v -> Some v
  | _ -> None

let defs_of_instrs instrs =
  List.fold_left
    (fun defs -> function
      | MicroIR.IAssign (dst, expr) -> (dst, expr) :: defs
      | _ -> defs)
    []
    instrs

let rec expr_is_byte_access defs = function
  | MicroIR.EIndex (_, _, 1) -> true
  | MicroIR.EVal (MicroIR.VReg v) -> (
      match List.assoc_opt v defs with
      | Some expr -> expr_is_byte_access defs expr
      | None -> false)
  | _ -> false

let rec expr_is_field_access defs = function
  | MicroIR.EField _ -> true
  | MicroIR.EVal (MicroIR.VReg v) -> (
      match List.assoc_opt v defs with
      | Some expr -> expr_is_field_access defs expr
      | None -> false)
  | _ -> false

let guard_tests_zero defs = function
  | Some (MicroIR.ECmp (("eq" | "ne"), lhs, MicroIR.VConst 0))
  | Some (MicroIR.ECmp (("eq" | "ne"), MicroIR.VConst 0, lhs)) -> (
      match value_name lhs with
      | Some v -> (
          match List.assoc_opt v defs with
          | Some expr -> expr_is_byte_access defs expr, expr_is_field_access defs expr, Some v
          | None -> false, false, Some v)
      | None -> false, false, None)
  | _ -> false, false, None

let first_index_shape instrs =
  instrs
  |> List.find_map (function
       | MicroIR.IAssign (_, MicroIR.EIndex (base, idx, width)) ->
           Some (value_name base, value_name idx, Some width)
       | _ -> None)

let first_field_shape instrs =
  instrs
  |> List.find_map (function
       | MicroIR.IAssign (_, MicroIR.EField (base, fld)) ->
           Some (value_name base, Some fld)
       | _ -> None)

let analyze_loop fn (lp : LoopRecovery.loop_desc) (ls : LoopSummary.t) =
  let bmap = CfgAnalysis.block_map fn in
  let instrs =
    lp.body_nodes
    |> List.filter_map (fun lbl -> CfgAnalysis.SM.find_opt lbl bmap)
    |> List.concat_map (fun blk -> blk.MicroIR.body)
  in
  let defs = defs_of_instrs instrs in
  let index_base, index_cursor, element_width =
    match first_index_shape instrs with
    | Some (base, idx, width) -> (base, idx, width)
    | None -> (None, None, None)
  in
  let field_base, field_name =
    match first_field_shape instrs with
    | Some (base, fld) -> (base, fld)
    | None -> (None, None)
  in
  let index_cursor_vars =
    instrs
    |> List.filter_map (function
         | MicroIR.IAssign (_, MicroIR.EIndex (_, idx, _)) -> value_name idx
         | _ -> None)
    |> List.filter (fun v -> List.mem v lp.carried)
    |> uniq
  in
  let stable_index_access =
    index_cursor_vars <> []
  in
  let byte_access =
    List.exists
      (function
        | MicroIR.IAssign (_, MicroIR.EIndex (_, _, 1)) -> true
        | _ -> false)
      instrs
  in
  let field_access =
    List.exists
      (function
        | MicroIR.IAssign (_, MicroIR.EField _) -> true
        | _ -> false)
      instrs
  in
  let guard_s = ls.guard in
  let guard_byte_zero, guard_field_zero, guard_var = guard_tests_zero defs lp.guard in
  let sentinel_zero_guard =
    byte_access
    && (guard_byte_zero || has_phrase guard_s " eq 0" || has_phrase guard_s " ne 0")
  in
  let null_guard =
    guard_field_zero
    || match guard_var with
       | Some gv ->
           List.exists
             (fun (tr : LoopSummary.transition) -> tr.var = gv && is_pointer_chase_transition tr)
             ls.transitions
       | None -> false
  in
  let index_stride =
    List.exists
      (fun (tr : LoopSummary.transition) ->
        List.mem tr.var index_cursor_vars && is_stride_transition tr)
      ls.transitions
  in
  let arithmetic_stride =
    index_stride
  in
  let pointer_chase =
    match guard_var with
    | Some gv ->
        List.exists
          (fun (tr : LoopSummary.transition) ->
            tr.var = gv && is_pointer_chase_transition tr)
          ls.transitions
    | None -> false
  in
  let evidence =
    {
      stable_index_access;
      byte_access;
      field_access;
      sentinel_zero_guard;
      null_guard;
      arithmetic_stride;
      pointer_chase;
      cursor =
        (match guard_var with
        | Some v -> Some v
        | None ->
            (match index_cursor with
            | Some v when List.mem v lp.carried -> Some v
            | _ ->
                (match field_base with
                | Some v when List.mem v lp.carried -> Some v
                | _ -> None)));
      index_base;
      element_width;
      field_name;
      guard_var;
    }
  in
  let array_score =
    (if stable_index_access then 3 else 0)
    + (if arithmetic_stride then 3 else 0)
    + (if ls.guard <> "loop_guard_unknown" && not sentinel_zero_guard then 2 else 0)
    - (if pointer_chase then 4 else 0)
    - (if sentinel_zero_guard then 3 else 0)
  in
  let string_score =
    (if byte_access then 2 else 0)
    + (if arithmetic_stride then 1 else 0)
    + (if sentinel_zero_guard then 4 else 0)
    + (if stable_index_access then 1 else 0)
    - (if pointer_chase then 3 else 0)
  in
  let list_score =
    (if field_access then 2 else 0)
    + (if pointer_chase then 4 else 0)
    + (if null_guard then 3 else 0)
    - (if stable_index_access then 2 else 0)
  in
  let scores =
    [ ("array", array_score); ("string", string_score); ("linked-list", list_score) ]
  in
  let kind =
    let best_name, best_score =
      List.fold_left
        (fun (bn, bs) (n, s) -> if s > bs then (n, s) else (bn, bs))
        ("unknown", min_int) scores
    in
    if best_score < 3 then UnknownLoop
    else
      match best_name with
      | "array" -> ArrayLoop
      | "string" -> StringLoop
      | "linked-list" -> LinkedListLoop
      | _ -> UnknownLoop
  in
  let chosen_cursor =
    match kind with
    | ArrayLoop | StringLoop -> (
        match index_cursor with
        | Some v -> Some v
        | _ -> None)
    | LinkedListLoop -> (
        match guard_var with
        | Some v -> Some v
        | None ->
            (match field_base with
            | Some v when List.mem v lp.carried -> Some v
            | _ -> None))
    | UnknownLoop -> None
  in
  let evidence =
    { evidence with cursor = chosen_cursor }
  in
  let reasons =
    [
      (stable_index_access, "indexed access through carried cursor");
      (byte_access, "byte-width memory access");
      (field_access, "field access inside loop");
      (sentinel_zero_guard, "loop exits on zero-valued loaded byte");
      (null_guard, "loop exits on null-like pointer guard");
      (arithmetic_stride, "carried cursor advances by arithmetic stride");
      (pointer_chase, "carried cursor advances by field-based pointer chase");
    ]
    |> List.filter_map (fun (b, s) -> if b then Some s else None)
  in
  let derived_facts =
    (match kind with
    | ArrayLoop ->
        [
          (match evidence.cursor with
          | Some c -> Some (Printf.sprintf "%s is the array traversal cursor" c)
          | None -> None);
          (match evidence.index_base with
          | Some b -> Some (Printf.sprintf "%s is the stable array base" b)
          | None -> None);
          (match evidence.element_width with
          | Some w -> Some (Printf.sprintf "array element width = %d" w)
          | None -> None);
          (match ls.guard with
          | g when g <> "loop_guard_unknown" ->
              Some (Printf.sprintf "loop bound is guarded by %s" g)
          | _ -> None);
        ]
    | StringLoop ->
        [
          (match evidence.cursor with
          | Some c -> Some (Printf.sprintf "%s is the string cursor" c)
          | None -> None);
          (match evidence.index_base with
          | Some b -> Some (Printf.sprintf "%s is the string base" b)
          | None -> None);
          Some "termination is driven by a zero byte sentinel";
          Some "string traversal reads one byte per iteration";
        ]
    | LinkedListLoop ->
        [
          (match evidence.cursor with
          | Some c -> Some (Printf.sprintf "%s is the list cursor" c)
          | None -> None);
          (match evidence.field_name with
          | Some fld -> Some (Printf.sprintf "pointer chasing follows field %s" fld)
          | None -> None);
          Some "termination is driven by a null pointer test";
        ]
    | UnknownLoop -> [])
    |> List.filter_map (fun x -> x)
  in
  { kind; scores; evidence; reasons; derived_facts }

let render p =
  let scores =
    p.scores
    |> List.map (fun (k, v) -> k ^ "=" ^ string_of_int v)
    |> String.concat ","
  in
  let ev = p.evidence in
  let evidence_s =
    Printf.sprintf
      "cursor=%s,base=%s,width=%s,field=%s,guard=%s"
      (pp_opt (fun x -> x) ev.cursor)
      (pp_opt (fun x -> x) ev.index_base)
      (pp_opt string_of_int ev.element_width)
      (pp_opt (fun x -> x) ev.field_name)
      (pp_opt (fun x -> x) ev.guard_var)
  in
  Printf.sprintf
    "memory-pattern(kind=%s, scores=[%s], evidence={%s}, reasons=[%s], facts=[%s])"
    (kind_name p.kind)
    scores
    evidence_s
    (String.concat "; " p.reasons)
    (String.concat "; " p.derived_facts)
