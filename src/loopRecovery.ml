type loop_shape =
  | Natural
  | Rotated
  | HeaderSplit
  | AlwaysEnter
  | TwoBlock
  | Residual

type loop_desc = {
  header : string;
  body_nodes : string list;
  exits : string list;
  carried : string list;
  shape : loop_shape;
  guard : MicroIR.expr option;
}

type region =
  | SeqRegion of string list
  | IfRegion of string * region * region
  | LoopRegion of loop_desc
  | ResidualRegion of string list

let shape_name = function
  | Natural -> "natural"
  | Rotated -> "rotated"
  | HeaderSplit -> "header-split"
  | AlwaysEnter -> "always-enter"
  | TwoBlock -> "two-block"
  | Residual -> "residual"

let describe_loop l =
  let guard_s =
    match l.guard with
    | Some g -> MicroIR.string_of_expr g
    | None -> "unknown"
  in
  Printf.sprintf
    "loop(header=%s, guard=%s, shape=%s, body=%d, exits=%d, carried=%d)"
    l.header
    guard_s
    (shape_name l.shape)
    (List.length l.body_nodes)
    (List.length l.exits)
    (List.length l.carried)

let sort_uniq xs =
  List.sort_uniq String.compare xs

let defs_of_instr = function
  | MicroIR.IAssign (v, _) -> [ v ]
  | MicroIR.ICall (Some v, _, _) -> [ v ]
  | _ -> []

let used_value = function
  | MicroIR.VReg v -> [ v ]
  | MicroIR.VConst _ | MicroIR.VUndef -> []

let used_expr = function
  | MicroIR.EVal v -> used_value v
  | MicroIR.EBinop (_, a, b) -> used_value a @ used_value b
  | MicroIR.ELoad v -> used_value v
  | MicroIR.EAddr (v, _) -> used_value v
  | MicroIR.ECmp (_, a, b) -> used_value a @ used_value b

let used_of_instr = function
  | MicroIR.IAssign (_, e) -> used_expr e
  | MicroIR.IStore (a, b) -> used_value a @ used_value b
  | MicroIR.IAssume e | MicroIR.IAssert e -> used_expr e
  | MicroIR.ICall (_, callee, args) -> used_value callee @ List.flatten (List.map used_value args)

let carried_vars fn body_nodes =
  let node_set = List.fold_left (fun acc n -> CfgAnalysis.SS.add n acc) CfgAnalysis.SS.empty body_nodes in
  let defs = ref [] in
  let uses = ref [] in
  List.iter
    (fun blk ->
      if CfgAnalysis.SS.mem blk.MicroIR.label node_set then begin
        List.iter (fun ins ->
          defs := !defs @ defs_of_instr ins;
          uses := !uses @ used_of_instr ins) blk.MicroIR.body
      end)
    fn.MicroIR.blocks;
  let defset = List.fold_left (fun acc v -> CfgAnalysis.SS.add v acc) CfgAnalysis.SS.empty !defs in
  !uses |> sort_uniq |> List.filter (fun v -> CfgAnalysis.SS.mem v defset)

let classify_shape body_nodes =
  match List.length body_nodes with
  | 0 | 1 -> Residual
  | 2 -> TwoBlock
  | _ -> Natural

let recover_loops (fn : MicroIR.func) =
  let bmap = CfgAnalysis.block_map fn in
  CfgAnalysis.back_edges fn
  |> List.map (fun (tail, header) ->
         let body_nodes =
           CfgAnalysis.natural_loop_nodes fn (tail, header)
           |> CfgAnalysis.SS.elements
           |> List.sort String.compare
         in
         let guard =
           match CfgAnalysis.SM.find_opt header bmap with
           | Some blk -> (
               match blk.MicroIR.term with
               | MicroIR.TBranch (e, _, _) -> Some e
               | _ -> None)
           | None -> None
         in
         {
           header;
           body_nodes;
           exits = CfgAnalysis.exit_nodes fn body_nodes |> sort_uniq;
           carried = carried_vars fn body_nodes;
           shape = classify_shape body_nodes;
           guard;
         })
