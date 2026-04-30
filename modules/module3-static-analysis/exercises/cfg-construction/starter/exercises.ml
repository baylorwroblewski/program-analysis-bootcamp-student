(** CFG Construction Exercises.

    Each function below takes a list of AST statements and returns a CFG
    whose shape matches a specific control-flow pattern.

    Students: implement the functions marked with TODO.

    General approach for each exercise:
    1. Create the basic blocks with [Cfg.create_block].
    2. Put them into a [Cfg.StringMap] keyed by label.
    3. Build the initial [Cfg.cfg] record with entry, exit_label, and blocks.
    4. Use [Cfg.add_edge] to wire up the control flow edges.

    The ENTRY and EXIT blocks are always empty (no statements). *)

open Shared_ast.Ast_types

(** Build a CFG for straight-line (sequential) code.

    Expected shape:

      ENTRY --> B1 --> EXIT

    All statements go into a single block B1.

    Example input:
      [ Assign ("x", IntLit 1);
        Assign ("y", IntLit 2);
        Assign ("z", BinOp (Add, Var "x", Var "y")) ]

    @param stmts  A flat list of statements with no branches or loops. *)
let build_cfg_sequential (stmts : stmt list) : Cfg.cfg =
  (* TODO:
     1. Create three blocks: "ENTRY" (empty), "B1" (all stmts), "EXIT" (empty)
     2. Add them to a StringMap
     3. Build the cfg record (entry = "ENTRY", exit_label = "EXIT")
     4. Add edges: ENTRY -> B1, B1 -> EXIT *)
  ignore stmts;
  let entry_block = Cfg.create_block "ENTRY" [] in
  let b1_block = Cfg.create_block "B1" stmts in
  let exit_block = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry_block
    |> Cfg.StringMap.add "B1" b1_block
    |> Cfg.StringMap.add "EXIT" exit_block
  in

  let cfg =
    {
      Cfg.entry = "ENTRY";
      exit_label = "EXIT";
      blocks;
    }
  in

  let cfg = Cfg.add_edge cfg "ENTRY" "B1" in
  let cfg = Cfg.add_edge cfg "B1" "EXIT" in
  cfg

(** Build a CFG for an if-else branch.

    Expected shape (diamond):

           ENTRY
             |
           B_cond
           /    \
       B_then  B_else
           \    /
           B_join
             |
            EXIT

    The input should contain statements before the if, the if-else
    itself, and statements after the if.

    The condition block B_cond holds any statements that precede the
    If, plus the If statement acts as the branch (but is not placed
    in a block -- only its children are).

    For simplicity, this exercise expects the input to be:
      [ ...pre-if stmts...;
        If (cond, then_stmts, else_stmts);
        ...post-if stmts... ]

    Map them to blocks:
    - B_cond : statements before the If
    - B_then : then_stmts
    - B_else : else_stmts
    - B_join : statements after the If

    @param stmts  Statement list containing exactly one If statement. *)
let build_cfg_ifelse (stmts : stmt list) : Cfg.cfg =
  (* TODO:
     1. Partition [stmts] to find the If and the statements before/after it.
        Hint: use a recursive helper or List.fold to split around the If.
     2. Extract then_stmts and else_stmts from the If node.
     3. Create blocks: ENTRY, B_cond, B_then, B_else, B_join, EXIT
     4. Wire edges:
          ENTRY -> B_cond
          B_cond -> B_then
          B_cond -> B_else
          B_then -> B_join
          B_else -> B_join
          B_join -> EXIT *)
  ignore stmts;
  let rec split_if pre = function
    | [] -> failwith "No If statement found"
    | If (_, then_stmts, else_stmts) :: rest ->
        (List.rev pre, then_stmts, else_stmts, rest)
    | stmt :: rest ->
        split_if (stmt :: pre) rest
  in

  let pre_if, then_stmts, else_stmts, post_if =
    split_if [] stmts
  in

  let entry_block = Cfg.create_block "ENTRY" [] in
  let cond_block = Cfg.create_block "B_cond" pre_if in
  let then_block = Cfg.create_block "B_then" then_stmts in
  let else_block = Cfg.create_block "B_else" else_stmts in
  let join_block = Cfg.create_block "B_join" post_if in
  let exit_block = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry_block
    |> Cfg.StringMap.add "B_cond" cond_block
    |> Cfg.StringMap.add "B_then" then_block
    |> Cfg.StringMap.add "B_else" else_block
    |> Cfg.StringMap.add "B_join" join_block
    |> Cfg.StringMap.add "EXIT" exit_block
  in

  let cfg =
    {
      Cfg.entry = "ENTRY";
      exit_label = "EXIT";
      blocks;
    }
  in

  let cfg = Cfg.add_edge cfg "ENTRY" "B_cond" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_then" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_else" in
  let cfg = Cfg.add_edge cfg "B_then" "B_join" in
  let cfg = Cfg.add_edge cfg "B_else" "B_join" in
  let cfg = Cfg.add_edge cfg "B_join" "EXIT" in
  cfg

(** Build a CFG for a while loop.

    Expected shape:

       ENTRY
         |
       B_pre       (statements before the while)
         |
       B_cond  <---+
       /    \      |
    B_body   \     |
      |       \    |
      +--------+   |
               |
            B_post  (statements after the while)
               |
             EXIT

    More precisely:
      ENTRY -> B_pre -> B_cond -> B_body -> B_cond  (back edge!)
                                  B_cond -> B_post -> EXIT

    @param stmts  Statement list containing exactly one While statement. *)
let build_cfg_while (stmts : stmt list) : Cfg.cfg =
  (* TODO:
     1. Partition [stmts] to find the While and the stmts before/after it.
     2. Extract the loop body from the While node.
     3. Create blocks: ENTRY, B_pre, B_cond, B_body, B_post, EXIT
     4. Wire edges:
          ENTRY  -> B_pre
          B_pre  -> B_cond
          B_cond -> B_body    (loop body)
          B_cond -> B_post    (loop exit)
          B_body -> B_cond    (back edge)
          B_post -> EXIT *)
  ignore stmts;
let rec split_while pre = function
    | [] -> failwith "No While statement found"
    | While (_, body_stmts) :: rest ->
        (List.rev pre, body_stmts, rest)
    | stmt :: rest ->
        split_while (stmt :: pre) rest
  in

  let pre_loop, body_stmts, post_loop =
    split_while [] stmts
  in

  let entry_block = Cfg.create_block "ENTRY" [] in
  let pre_block = Cfg.create_block "B_pre" pre_loop in
  let cond_block = Cfg.create_block "B_cond" [] in
  let body_block = Cfg.create_block "B_body" body_stmts in
  let post_block = Cfg.create_block "B_post" post_loop in
  let exit_block = Cfg.create_block "EXIT" [] in

  let blocks =
    Cfg.StringMap.empty
    |> Cfg.StringMap.add "ENTRY" entry_block
    |> Cfg.StringMap.add "B_pre" pre_block
    |> Cfg.StringMap.add "B_cond" cond_block
    |> Cfg.StringMap.add "B_body" body_block
    |> Cfg.StringMap.add "B_post" post_block
    |> Cfg.StringMap.add "EXIT" exit_block
  in

  let cfg =
    {
      Cfg.entry = "ENTRY";
      exit_label = "EXIT";
      blocks;
    }
  in

  let cfg = Cfg.add_edge cfg "ENTRY" "B_pre" in
  let cfg = Cfg.add_edge cfg "B_pre" "B_cond" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_body" in
  let cfg = Cfg.add_edge cfg "B_cond" "B_post" in
  let cfg = Cfg.add_edge cfg "B_body" "B_cond" in
  let cfg = Cfg.add_edge cfg "B_post" "EXIT" in
  cfg