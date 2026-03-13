type stage =
  | Lift
  | Normalize
  | Recover
  | Summarize
  | Execute

let stage_name = function
  | Lift -> "lift"
  | Normalize -> "normalize"
  | Recover -> "recover"
  | Summarize -> "summarize"
  | Execute -> "execute"

let stages = [ Lift; Normalize; Recover; Summarize; Execute ]

let rationale =
  [
    (Lift, "Translate ISA-specific instructions into canonical MicroIR.");
    (Normalize, "Build explicit CFG, SSA-like variables, and memory effects.");
    (Recover, "Detect loops, joins, and modular regions before path exploration.");
    (Summarize, "Infer loop summaries and function-level side-effect contracts.");
    (Execute, "Run symbolic execution over structured regions and residual CFGs.");
  ]

let render () =
  List.map
    (fun stage ->
      let desc = List.assoc stage rationale in
      Printf.sprintf "- %s: %s" (stage_name stage) desc)
    stages
