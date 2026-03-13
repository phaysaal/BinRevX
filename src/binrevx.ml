let print_header () =
  print_endline "BinRevX";
  print_endline "Binary symbolic execution via lifted structural recovery";
  print_endline ""

let print_pipeline () =
  print_endline "Pipeline:";
  List.iter print_endline (SymexPlan.render ());
  print_endline ""

let print_reasoning () =
  print_endline "Key design choice:";
  print_endline
    "  Do not symbolically execute the full assembly ISA directly.";
  print_endline
    "  Lift instructions into a canonical MicroIR, then recover loops and summaries.";
  print_endline ""

let load_prog () =
  match Array.to_list Sys.argv with
  | [ _ ] -> ("built-in sample", MicroIR.sample_prog)
  | [ _; path ] -> (path, MicroIRParser.load path)
  | _ ->
      prerr_endline "usage: ./binrevx [program.microir]";
      exit 2

let print_func_loops fn =
  let loops = LoopRecovery.recover_loops fn in
  Printf.printf "Function %s:\n" fn.MicroIR.fname;
  if loops = [] then
    print_endline "  no loop recovered"
  else
    List.iter (fun lp -> print_endline ("  " ^ LoopRecovery.describe_loop lp)) loops

let print_func_regions fn =
  let regions = RegionRecovery.recover_regions fn in
  print_endline "  regions:";
  if regions = [] then
    print_endline "    none"
  else
    List.iter
      (fun r -> print_endline ("    " ^ RegionRecovery.describe r))
      regions

let print_func_summary fn =
  let s = ModSummary.infer fn in
  print_endline ("  " ^ ModSummary.render s)

let print_func_symbolic fn =
  let summary, seed, header_states, states = SymExec.explore_function fn in
  let _ = summary in
  print_endline "  symbolic:";
  print_endline ("    seed: " ^ SymState.render seed);
  if CfgAnalysis.SM.is_empty header_states then
    print_endline "    headers: none"
  else begin
    print_endline "    headers:";
    CfgAnalysis.SM.bindings header_states
    |> List.iter (fun (lbl, st) ->
           print_endline ("      " ^ lbl ^ " => " ^ SymState.render st))
  end;
  if states = [] then
    print_endline "    no region splits"
  else
    List.iter
      (fun bs -> print_endline ("    " ^ SymExec.render_branch_state bs))
      states

let print_loop_summaries fn =
  let _, _, header_states, _ = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  loop-summaries:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match CfgAnalysis.SM.find_opt lp.LoopRecovery.header header_states with
        | Some st ->
            let s = LoopSummary.summarize fn st lp in
            print_endline ("    " ^ LoopSummary.render s)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let print_loop_invariants fn =
  let _, _, header_states, _ = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  invariants:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match CfgAnalysis.SM.find_opt lp.LoopRecovery.header header_states with
        | Some st ->
            let summary = LoopSummary.summarize fn st lp in
            let pat = MemoryPattern.analyze_loop fn lp summary in
            let inv = InvariantGen.generate ~pattern:pat summary in
            print_endline ("    " ^ InvariantGen.render inv)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let print_memory_patterns fn =
  let _, _, header_states, _ = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  memory-patterns:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match CfgAnalysis.SM.find_opt lp.LoopRecovery.header header_states with
        | Some st ->
            let summary = LoopSummary.summarize fn st lp in
            let pat = MemoryPattern.analyze_loop fn lp summary in
            print_endline ("    " ^ MemoryPattern.render pat)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let print_loop_vcs fn =
  let _, _, header_states, _ = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  vcs:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match CfgAnalysis.SM.find_opt lp.LoopRecovery.header header_states with
        | Some st ->
            let summary = LoopSummary.summarize fn st lp in
            let pat = MemoryPattern.analyze_loop fn lp summary in
            let inv = InvariantGen.generate ~pattern:pat summary in
            let vcs = VcGen.loop_vcs ~pattern:pat summary inv in
            print_endline ("    " ^ VcGen.render vcs)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let () =
  let source, prog = load_prog () in
  print_header ();
  print_pipeline ();
  print_reasoning ();
  Printf.printf "Recovered loops from %s:\n" source;
  List.iter
    (fun fn ->
      print_func_loops fn;
      print_func_regions fn;
      print_endline "  summary:";
      print_func_summary fn;
      print_func_symbolic fn;
      print_loop_summaries fn;
      print_memory_patterns fn;
      print_loop_invariants fn;
      print_loop_vcs fn;
      print_endline "")
    prog
