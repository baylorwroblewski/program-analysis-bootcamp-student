(** Control Flow Graph implementation.

    Students: implement the functions marked with TODO below.
    [create_block] is provided as a reference. *)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type basic_block = {
  label : string;
  stmts : Shared_ast.Ast_types.stmt list;
  mutable succs : string list;
  mutable preds : string list;
}

type cfg = {
  entry : string;
  exit_label : string;
  blocks : basic_block StringMap.t;
}

(* --- Provided ----------------------------------------------------------- *)

let create_block (label : string) (stmts : Shared_ast.Ast_types.stmt list) : basic_block =
  { label; stmts; succs = []; preds = [] }

(* --- TODO: implement these ---------------------------------------------- *)

let add_edge (cfg : cfg) (src : string) (dst : string) : cfg =
  (* TODO: Return a new cfg where:
     1. The block named [src] has [dst] appended to its succs list
     2. The block named [dst] has [src] appended to its preds list
     3. All other blocks remain unchanged
     Hint: Look up both blocks in cfg.blocks using StringMap.find,
     create updated copies, and build a new blocks map with StringMap.add. *)
  ignore (cfg, src, dst);
let src_block = StringMap.find src cfg.blocks in
  let dst_block = StringMap.find dst cfg.blocks in

  let updated_src =
    { src_block with succs = src_block.succs @ [dst] }
  in

  let updated_dst =
    { dst_block with preds = dst_block.preds @ [src] }
  in

  let updated_blocks =
    cfg.blocks
    |> StringMap.add src updated_src
    |> StringMap.add dst updated_dst
  in

  { cfg with blocks = updated_blocks }

let predecessors (cfg : cfg) (label : string) : string list =
  (* TODO: Look up the block with the given label in cfg.blocks
     and return its preds list.
     Hint: Use StringMap.find. *)
  ignore (cfg, label);
let block = StringMap.find label cfg.blocks in
  block.preds

let successors (cfg : cfg) (label : string) : string list =
  (* TODO: Look up the block with the given label in cfg.blocks
     and return its succs list.
     Hint: Use StringMap.find. *)
  ignore (cfg, label);
  let block = StringMap.find label cfg.blocks in
  block.succs

let to_string (cfg : cfg) : string =
  (* TODO: Build a human-readable string representation of the CFG.
     For each block, print its label, the number of statements it
     contains, its successors, and its predecessors.  Format example:

       Block: ENTRY (0 stmts)
         succs: [B1]
         preds: []

     Hint: Use StringMap.fold to iterate over cfg.blocks.
     Use String.concat to join lists of labels. *)
  ignore cfg;
StringMap.fold
    (fun _ block acc ->
      let succs_str = String.concat ", " block.succs in
      let preds_str = String.concat ", " block.preds in

      acc ^
      Printf.sprintf
        "Block: %s (%d stmts)\n  succs: [%s]\n  preds: [%s]\n\n"
        block.label
        (List.length block.stmts)
        succs_str
        preds_str)
    cfg.blocks
    ""