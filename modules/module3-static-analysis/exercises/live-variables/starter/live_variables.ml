(* Live Variables Analysis - Backward May-Analysis
 *
 * A variable is "live" at a program point if there exists some path from
 * that point to a use of the variable that does not pass through a
 * redefinition. This is a BACKWARD analysis because information flows
 * from uses (later in the program) back to earlier points.
 *
 * Transfer function (applied backward):
 *   IN[B] = use[B] U (OUT[B] - def[B])
 *
 * Merge (at control-flow joins in backward direction):
 *   OUT[B] = U IN[S] for all successors S of B
 *
 * This is a "may" analysis with union merge because a variable is live
 * if it MAY be used along ANY path from the current point.
 *
 * Block representation:
 *   (label, defined_vars, used_vars, successor_labels)
 *
 * - label:            unique string identifying the block
 * - defined_vars:     variables assigned/written in this block
 * - used_vars:        variables read in this block (before any local def)
 * - successor_labels: labels of blocks that follow this one in the CFG
 *)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

(* compute_use: Extract the use set for a block.
 *
 * The "use" set contains variables that are read in this block before
 * being defined. These variables must be live on entry to the block
 * regardless of what happens after the block.
 *
 * Block format: (label, defined_vars, used_vars)
 *)
let compute_use ((_label, _defs, uses) : string * string list * string list) : StringSet.t =
  List.fold_left
    (fun acc v -> StringSet.add v acc)
    StringSet.empty
    uses

(* compute_def: Extract the def set for a block.
 *
 * The "def" set contains variables that are assigned/written in this
 * block. A definition "kills" liveness -- if a variable is defined here,
 * it does not need to be live on entry (unless it is also used before
 * the definition, which is captured by the use set).
 *
 * Block format: (label, defined_vars, used_vars)
 *)
let compute_def ((_label, defs, _uses) : string * string list * string list) : StringSet.t =
  List.fold_left
    (fun acc v -> StringSet.add v acc)
    StringSet.empty
    defs

(* analyze: Run the live variables backward iterative analysis.
 *
 * Given a list of blocks (label, defined_vars, used_vars, successor_labels),
 * compute the fixed-point solution for IN[B] at each block.
 *
 * Algorithm:
 *   1. Initialize IN[B] = {} for all blocks
 *   2. Repeat until no IN set changes:
 *      a. For each block B:
 *         - OUT[B] = union of IN[S] for all successors S of B
 *         - IN[B]  = use[B] U (OUT[B] - def[B])
 *   3. Return (label, IN[B], OUT[B]) for each block
 *
 * Returns: list of (label, in_set, out_set) triples
 *)
let analyze
    (blocks : (string * string list * string list * string list) list)
    : (string * StringSet.t * StringSet.t) list =

  let labels =
    List.map (fun (label, _, _, _) -> label) blocks
  in

  let in_map =
    List.fold_left
      (fun acc label -> StringMap.add label StringSet.empty acc)
      StringMap.empty
      labels
  in

  let get_in label map =
    match StringMap.find_opt label map with
    | Some s -> s
    | None -> StringSet.empty
  in

  let union_successors succs in_map =
    List.fold_left
      (fun acc succ ->
        StringSet.union acc (get_in succ in_map))
      StringSet.empty
      succs
  in

  let rec iterate in_map =
    let changed, new_in_map, out_map =
      List.fold_left
        (fun (changed, in_acc, out_acc)
             (label, defs, uses, succs) ->

          let old_in = get_in label in_acc in

          let out_set =
            union_successors succs in_acc
          in

          let use_set =
            compute_use (label, defs, uses)
          in

          let def_set =
            compute_def (label, defs, uses)
          in

          let new_in =
            StringSet.union
              use_set
              (StringSet.diff out_set def_set)
          in

          let changed =
            changed || not (StringSet.equal old_in new_in)
          in

          let in_acc =
            StringMap.add label new_in in_acc
          in

          let out_acc =
            StringMap.add label out_set out_acc
          in

          (changed, in_acc, out_acc))
        (false, in_map, StringMap.empty)
        blocks
    in

    if changed then
      iterate new_in_map
    else
      (new_in_map, out_map)
  in

  let final_in_map, final_out_map =
    iterate in_map
  in

  List.map
    (fun (label, _, _, _) ->
      let in_set =
        get_in label final_in_map
      in

      let out_set =
        match StringMap.find_opt label final_out_map with
        | Some s -> s
        | None -> StringSet.empty
      in

      (label, in_set, out_set))
    blocks