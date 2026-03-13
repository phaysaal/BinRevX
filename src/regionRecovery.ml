type cond_desc = {
  header : string;
  then_lbl : string;
  else_lbl : string;
  guard_true : string;
  guard_false : string;
}

type t =
  | RLoop of LoopRecovery.loop_desc
  | RCond of cond_desc
  | RResidual of string list

let loop_headers loops =
  List.fold_left
    (fun acc lp -> CfgAnalysis.SS.add lp.LoopRecovery.header acc)
    CfgAnalysis.SS.empty
    loops

let recover_regions (fn : MicroIR.func) =
  let loops = LoopRecovery.recover_loops fn in
  let loop_hdrs = loop_headers loops in
  let conds =
    fn.MicroIR.blocks
    |> List.filter_map (fun blk ->
           match blk.MicroIR.term with
           | MicroIR.TBranch (_, t, f)
             when not (CfgAnalysis.SS.mem blk.MicroIR.label loop_hdrs) ->
               let guard_true, guard_false =
                 match blk.MicroIR.term with
                 | MicroIR.TBranch (e, _, _) ->
                     ( MicroIR.string_of_expr e,
                       MicroIR.string_of_negated_expr e )
                 | _ -> ("unknown", "unknown")
               in
               Some
                 (RCond
                    {
                      header = blk.MicroIR.label;
                      then_lbl = t;
                      else_lbl = f;
                      guard_true;
                      guard_false;
                    })
           | _ -> None)
  in
  let loop_regions = List.map (fun lp -> RLoop lp) loops in
  loop_regions @ conds

let describe = function
  | RLoop lp -> LoopRecovery.describe_loop lp
  | RCond c ->
      Printf.sprintf
        "if(header=%s, guardT=%s, guardF=%s, then=%s, else=%s)"
        c.header
        c.guard_true
        c.guard_false
        c.then_lbl
        c.else_lbl
  | RResidual nodes ->
      Printf.sprintf "residual(nodes=%d)" (List.length nodes)
