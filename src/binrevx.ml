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

let binary_func = ref "main"
let demand_mode = ref false

let is_elf_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let b0 = input_byte ic in
      let b1 = input_byte ic in
      let b2 = input_byte ic in
      let b3 = input_byte ic in
      b0 = 0x7f && b1 = Char.code 'E' && b2 = Char.code 'L' && b3 = Char.code 'F')

let load_prog () =
  let rec parse path = function
    | [] -> path
    | "--func" :: name :: rest ->
        binary_func := name;
        parse path rest
    | "--demand" :: rest ->
        demand_mode := true;
        parse path rest
    | arg :: rest ->
        if path = None then
          parse (Some arg) rest
        else begin
          prerr_endline ("unexpected argument: " ^ arg);
          exit 2
        end
  in
  let path = parse None (Array.to_list Sys.argv |> List.tl) in
  match path with
  | None -> ("built-in sample", MicroIR.sample_prog)
  | Some path ->
      if is_elf_file path then
        ( path ^ " [func=" ^ !binary_func ^ "]",
          BinImport.import ~func:!binary_func path )
      else
        (path, MicroIRParser.load path)

let print_func_loops fn =
  let loops = LoopRecovery.recover_loops fn in
  Printf.printf "Function %s:\n" fn.MicroIR.fname;
  Printf.printf
    "  signature: params=[%s] ret=%s\n"
    (String.concat "," fn.MicroIR.params)
    (match fn.MicroIR.ret with Some r -> r | None -> "void");
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

let find_loop_state header header_states states =
  match CfgAnalysis.SM.find_opt header header_states with
  | Some st -> Some st
  | None ->
      states
      |> List.find_map (fun (bs : SymExec.branch_state) ->
             if bs.state.SymState.location = header then Some bs.state else None)

let print_loop_summaries fn =
  let _, _, header_states, states = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  loop-summaries:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match find_loop_state lp.LoopRecovery.header header_states states with
        | Some st ->
            let s = LoopSummary.summarize fn st lp in
            print_endline ("    " ^ LoopSummary.render s)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let print_loop_invariants fn =
  let _, _, header_states, states = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  invariants:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match find_loop_state lp.LoopRecovery.header header_states states with
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
  let _, _, header_states, states = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  memory-patterns:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match find_loop_state lp.LoopRecovery.header header_states states with
        | Some st ->
            let summary = LoopSummary.summarize fn st lp in
            let pat = MemoryPattern.analyze_loop fn lp summary in
            print_endline ("    " ^ MemoryPattern.render pat)
        | None ->
            print_endline
              ("    missing header state for loop " ^ lp.LoopRecovery.header))
      loops

let print_loop_vcs fn =
  let _, _, header_states, states = SymExec.explore_function fn in
  let loops = LoopRecovery.recover_loops fn in
  print_endline "  vcs:";
  if loops = [] then
    print_endline "    none"
  else
    List.iter
      (fun lp ->
        match find_loop_state lp.LoopRecovery.header header_states states with
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
  if !demand_mode && String.ends_with ~suffix:(" [func=" ^ !binary_func ^ "]") source then
    let suffix = " [func=" ^ !binary_func ^ "]" in
    let path = String.sub source 0 (String.length source - String.length suffix) in
    let analysis = DemandInterproc.analyze_binary ~path ~entry:!binary_func () in
    print_endline (DemandInterproc.render analysis)
  else begin
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
  end
