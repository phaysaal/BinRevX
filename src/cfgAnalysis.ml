module SS = Set.Make (String)
module SM = Map.Make (String)

let successors (blk : MicroIR.block) =
  match blk.MicroIR.term with
  | MicroIR.TJump l -> [ l ]
  | MicroIR.TBranch (_, t, f) -> [ t; f ]
  | MicroIR.TSwitch (_, cases, dflt) ->
      List.map snd cases @ [ dflt ]
  | MicroIR.TReturn _ | MicroIR.TStop -> []

let block_map (fn : MicroIR.func) =
  List.fold_left (fun acc blk -> SM.add blk.MicroIR.label blk acc) SM.empty fn.blocks

let labels (fn : MicroIR.func) = List.map (fun blk -> blk.MicroIR.label) fn.blocks

let predecessors (fn : MicroIR.func) =
  let init =
    List.fold_left (fun acc lbl -> SM.add lbl SS.empty acc) SM.empty (labels fn)
  in
  List.fold_left
    (fun acc blk ->
      List.fold_left
        (fun acc succ_lbl ->
          let old =
            match SM.find_opt succ_lbl acc with
            | Some s -> s
            | None -> SS.empty
          in
          SM.add succ_lbl (SS.add blk.MicroIR.label old) acc)
        acc
        (successors blk))
    init
    fn.blocks

let all_labels_set fn =
  List.fold_left (fun acc lbl -> SS.add lbl acc) SS.empty (labels fn)

let dominators (fn : MicroIR.func) =
  let all = all_labels_set fn in
  let preds = predecessors fn in
  let init =
    List.fold_left
      (fun acc lbl ->
        let ds = if lbl = fn.MicroIR.entry then SS.singleton lbl else all in
        SM.add lbl ds acc)
      SM.empty
      (labels fn)
  in
  let rec fix doms =
    let changed = ref false in
    let doms' =
      List.fold_left
        (fun acc lbl ->
          if lbl = fn.MicroIR.entry then
            acc
          else
            let pred_set =
              match SM.find_opt lbl preds with
              | Some s -> s
              | None -> SS.empty
            in
            let pred_list = SS.elements pred_set in
            let intersect =
              match pred_list with
              | [] -> SS.empty
              | p :: ps ->
                  List.fold_left
                    (fun s p' ->
                      let ds = Option.get (SM.find_opt p' doms) in
                      SS.inter s ds)
                    (Option.get (SM.find_opt p doms))
                    ps
            in
            let next = SS.add lbl intersect in
            let prev = Option.get (SM.find_opt lbl doms) in
            if not (SS.equal prev next) then changed := true;
            SM.add lbl next acc)
        doms
        (labels fn)
    in
    if !changed then fix doms' else doms'
  in
  fix init

let dominates doms a b =
  match SM.find_opt b doms with
  | Some ds -> SS.mem a ds
  | None -> false

let back_edges (fn : MicroIR.func) =
  let doms = dominators fn in
  List.fold_left
    (fun acc blk ->
      List.fold_left
        (fun acc succ_lbl ->
          if dominates doms succ_lbl blk.MicroIR.label then
            (blk.MicroIR.label, succ_lbl) :: acc
          else
            acc)
        acc
        (successors blk))
    []
    fn.blocks

let natural_loop_nodes (fn : MicroIR.func) (tail, header) =
  let preds = predecessors fn in
  let rec expand work seen =
    match work with
    | [] -> seen
    | x :: xs ->
        let pred_set =
          match SM.find_opt x preds with
          | Some s -> s
          | None -> SS.empty
        in
        let new_preds =
          SS.elements pred_set
          |> List.filter (fun p -> not (SS.mem p seen))
        in
        let seen' = List.fold_left (fun s p -> SS.add p s) seen new_preds in
        expand (new_preds @ xs) seen'
  in
  expand [ tail ] (SS.of_list [ header; tail ])

let exit_nodes (fn : MicroIR.func) nodes =
  let node_set = List.fold_left (fun acc n -> SS.add n acc) SS.empty nodes in
  List.fold_left
    (fun acc blk ->
      if SS.mem blk.MicroIR.label node_set then
        let escapes =
          successors blk |> List.filter (fun s -> not (SS.mem s node_set))
        in
        if escapes = [] then acc else blk.MicroIR.label :: acc
      else
        acc)
    []
    fn.blocks
