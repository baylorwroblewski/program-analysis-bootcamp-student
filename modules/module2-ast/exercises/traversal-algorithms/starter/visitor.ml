(* visitor.ml - AST visitor pattern exercises.
   Implement two common visitor-style operations that walk the AST
   and accumulate information. *)

open Shared_ast.Ast_types

(** Count the number of each node type in a statement list.
    Returns an association list like:
      [("Assign", 3); ("IntLit", 5); ("BinOp", 2); ...]
    The keys are constructor names WITHOUT parameters (e.g., "IntLit"
    not "IntLit(3)"). Order does not matter.

    Hint:
      - Write recursive helpers for expr and stmt.
      - Use a mutable Hashtbl or a ref to a Map to accumulate counts,
        or thread an accumulator through the recursion.
      - Don't forget to count the node itself AND recurse into its
        children. *)
let count_nodes (_stmts : stmt list) : (string * int) list =
  (* TODO: walk the AST and count occurrences of each constructor *)
  let tbl = Hashtbl.create 16 in

  let inc key =
    let current =
      match Hashtbl.find_opt tbl key with
      | Some n -> n
      | None -> 0
    in
    Hashtbl.replace tbl key (current + 1)
  in

  let rec visit_expr e =
    match e with
    | IntLit _ ->
        inc "IntLit"
    | BoolLit _ ->
        inc "BoolLit"
    | Var _ ->
        inc "Var"
    | UnaryOp (_, e1) ->
        inc "UnaryOp";
        visit_expr e1
    | BinOp (_, e1, e2) ->
        inc "BinOp";
        visit_expr e1;
        visit_expr e2
    | Call (_, args) ->
        inc "Call";
        List.iter visit_expr args
  in

  let rec visit_stmt s =
    match s with
    | Assign (_, e) ->
        inc "Assign";
        visit_expr e
    | If (cond, then_b, else_b) ->
        inc "If";
        visit_expr cond;
        List.iter visit_stmt then_b;
        List.iter visit_stmt else_b
    | While (cond, body) ->
        inc "While";
        visit_expr cond;
        List.iter visit_stmt body
    | Return None ->
        inc "Return"
    | Return (Some e) ->
        inc "Return";
        visit_expr e
    | Print exprs ->
        inc "Print";
        List.iter visit_expr exprs
    | Block stmts ->
        inc "Block";
        List.iter visit_stmt stmts
  in

  List.iter visit_stmt _stmts;

  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

(** Evaluate a constant expression, returning Some int if the
    expression contains only integer literals and arithmetic operators,
    or None if it contains variables, booleans, calls, or comparison
    operators.

    Supported operators: Add, Sub, Mul, Div (integer division).
    Division by zero should return None.

    Examples:
      evaluate (IntLit 42)                        => Some 42
      evaluate (BinOp (Add, IntLit 1, IntLit 2))  => Some 3
      evaluate (BinOp (Add, IntLit 1, Var "x"))   => None
      evaluate (BoolLit true)                      => None

    Hint: use Option.bind or match on recursive results. *)
let rec evaluate (_e : expr) : int option =
  (* TODO: evaluate constant integer expressions *)
  match _e with
  | IntLit n -> Some n

  | BinOp (op, e1, e2) ->
      (match evaluate e1, evaluate e2 with
       | Some v1, Some v2 ->
           (match op with
            | Add -> Some (v1 + v2)
            | Sub -> Some (v1 - v2)
            | Mul -> Some (v1 * v2)
            | Div ->
                if v2 = 0 then None else Some (v1 / v2)
            | _ -> None)
       | _ -> None)

  | _ -> None