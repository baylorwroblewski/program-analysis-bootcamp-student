(** Generic iterative dataflow analysis solver.

    This module implements the classic worklist-based fixpoint algorithm
    that underlies most dataflow analyses (reaching definitions, live
    variables, available expressions, etc.).

    The solver is parameterized by:
    - [direction]: whether information flows forward or backward
    - [init]: initial lattice value for every block
    - [merge]: how to combine values from multiple predecessors/successors
    - [transfer]: how a single basic block transforms a lattice value
    - [equal]: when to stop iterating (fixpoint test)

    The CFG is given as a list of
      (block_label, predecessor_labels, successor_labels)
    triples. The solver returns (block_label, in_value, out_value)
    for every block once a fixpoint is reached.
*)

type direction = Forward | Backward

type 'a analysis = {
  direction : direction;
  init : 'a;
  merge : 'a -> 'a -> 'a;
  transfer : string -> 'a -> 'a;
  equal : 'a -> 'a -> bool;
}

module StringMap = Map.Make (String)

(** [solve analysis cfg] runs the iterative fixpoint algorithm.

    @param analysis  the analysis configuration (direction, transfer, etc.)
    @param cfg       list of (block_label, predecessors, successors)
    @return          list of (block_label, in_value, out_value) at fixpoint

    Algorithm sketch (forward case):
    {v
      1. Initialize IN[B] = OUT[B] = analysis.init for every block B.
      2. Repeat until nothing changes:
         For each block B:
           a. IN[B]  = merge over all predecessors P of B: OUT[P]
           b. OUT[B] = transfer(B, IN[B])
      3. Return the final IN/OUT for each block.
    v}

    For the backward case, swap the roles of IN/OUT and
    predecessors/successors.

    TODO: Implement this function. It currently raises [Failure "TODO"].
*)
let solve (_analysis : 'a analysis)
    (_cfg : (string * string list * string list) list)
    : (string * 'a * 'a) list =
let labels =
    List.map (fun (label, _, _) -> label) _cfg
  in

  let in_map =
    List.fold_left
      (fun acc label -> StringMap.add label _analysis.init acc)
      StringMap.empty
      labels
  in

  let out_map =
    List.fold_left
      (fun acc label -> StringMap.add label _analysis.init acc)
      StringMap.empty
      labels
  in

  let get map label =
    StringMap.find label map
  in

  let merge_labels map labels =
    match labels with
    | [] -> _analysis.init
    | first :: rest ->
        List.fold_left
          (fun acc label -> _analysis.merge acc (get map label))
          (get map first)
          rest
  in

  let rec iterate in_map out_map =
    let changed, new_in_map, new_out_map =
      List.fold_left
        (fun (changed, in_acc, out_acc) (label, preds, succs) ->
          match _analysis.direction with
          | Forward ->
              let old_in = get in_acc label in
              let old_out = get out_acc label in

              let new_in = merge_labels out_acc preds in
              let new_out = _analysis.transfer label new_in in

              let block_changed =
                not (_analysis.equal old_in new_in)
                || not (_analysis.equal old_out new_out)
              in

              let in_acc = StringMap.add label new_in in_acc in
              let out_acc = StringMap.add label new_out out_acc in

              (changed || block_changed, in_acc, out_acc)

          | Backward ->
              let old_in = get in_acc label in
              let old_out = get out_acc label in

              let new_out = merge_labels in_acc succs in
              let new_in = _analysis.transfer label new_out in

              let block_changed =
                not (_analysis.equal old_in new_in)
                || not (_analysis.equal old_out new_out)
              in

              let in_acc = StringMap.add label new_in in_acc in
              let out_acc = StringMap.add label new_out out_acc in

              (changed || block_changed, in_acc, out_acc))
        (false, in_map, out_map)
        _cfg
    in

    if changed then
      iterate new_in_map new_out_map
    else
      (new_in_map, new_out_map)
  in

  let final_in, final_out =
    iterate in_map out_map
  in

  List.map
    (fun label ->
      (label, get final_in label, get final_out label))
    labels