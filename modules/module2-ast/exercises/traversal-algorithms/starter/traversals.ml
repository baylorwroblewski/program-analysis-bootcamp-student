(* traversals.ml - AST traversal algorithms exercise.
   Implement three classic tree traversal strategies on the AST:
   pre-order (depth-first), post-order (depth-first), and
   breadth-first (level-order).

   Each function walks a list of statements and collects a string label
   for every node visited. Labels should look like:
     Statements: "Assign", "If", "While", "Return", "Print", "Block"
     Expressions: "IntLit(3)", "BoolLit(true)", "Var(x)", "BinOp(+)",
                  "UnaryOp(-)", "Call(f)"
*)

open Shared_ast.Ast_types

(** Helper: produce a string label for a single expression node.
    Examples: IntLit(3), BoolLit(true), Var(x), BinOp(+), UnaryOp(-), Call(f) *)
let label_of_expr (_e : expr) : string =
  (* TODO: pattern match on the expression and return its label string *)
  match _e with
  | IntLit n -> Printf.sprintf "IntLit(%d)" n
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | Var s -> Printf.sprintf "Var(%s)" s
  | BinOp (op, _, _) -> Printf.sprintf "BinOp(%s)" (string_of_op op)
  | UnaryOp (op, _) -> Printf.sprintf "UnaryOp(%s)" (string_of_uop op)
  | Call (name, _) -> Printf.sprintf "Call(%s)" name

(** Helper: produce a string label for a single statement node.
    Examples: "Assign", "If", "While", "Return", "Print", "Block" *)
let label_of_stmt (_s : stmt) : string =
  (* TODO: pattern match on the statement and return its label string *)
  match _s with
  | Assign _ -> "Assign"
  | If _ -> "If"
  | While _ -> "While"
  | Return _ -> "Return"
  | Print _ -> "Print"
  | Block _ -> "Block"

(** Pre-order depth-first traversal.
    Visit the current node FIRST, then recurse into its children
    left-to-right.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]

    Hint: write a mutual recursion with helpers for expr and stmt lists. *)
let pre_order (_stmts : stmt list) : string list =
  (* TODO: implement pre-order DFS traversal *)
  (* 1. Emit the label of the current node
     2. Then recurse into children *)
  let rec expr e =
    let current = [label_of_expr e] in
    match e with
    | IntLit _ | BoolLit _ | Var _ -> current
    | UnaryOp (_, e1) -> current @ expr e1
    | BinOp (_, e1, e2) -> current @ expr e1 @ expr e2
    | Call (_, args) -> current @ List.flatten (List.map expr args)
  in
  let rec stmt s =
    let current = [label_of_stmt s] in
    match s with
    | Assign (_, e) -> current @ expr e
    | If (cond, t, e) ->
        current @ expr cond @ stmts t @ stmts e
    | While (cond, body) ->
        current @ expr cond @ stmts body
    | Return None -> current
    | Return (Some e) -> current @ expr e
    | Print exprs ->
        current @ List.flatten (List.map expr exprs)
    | Block stmts_list ->
        current @ stmts stmts_list
  and stmts ss =
    List.flatten (List.map stmt ss)
  in
  stmts _stmts

(** Post-order depth-first traversal.
    Recurse into children FIRST, then visit the current node.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["IntLit(1)"; "IntLit(2)"; "BinOp(+)"; "Assign"]

    Hint: same structure as pre_order but emit the label at the end. *)
let post_order (_stmts : stmt list) : string list =
  (* TODO: implement post-order DFS traversal *)
  (* 1. Recurse into children first
     2. Then emit the label of the current node *)
  let rec expr e =
    let children =
      match e with
      | IntLit _ | BoolLit _ | Var _ -> []
      | UnaryOp (_, e1) -> expr e1
      | BinOp (_, e1, e2) -> expr e1 @ expr e2
      | Call (_, args) -> List.flatten (List.map expr args)
    in
    children @ [label_of_expr e]
  in
  let rec stmt s =
    let children =
      match s with
      | Assign (_, e) -> expr e
      | If (cond, t, e) ->
          expr cond @ stmts t @ stmts e
      | While (cond, body) ->
          expr cond @ stmts body
      | Return None -> []
      | Return (Some e) -> expr e
      | Print exprs ->
          List.flatten (List.map expr exprs)
      | Block stmts_list ->
          stmts stmts_list
    in
    children @ [label_of_stmt s]
  and stmts ss =
    List.flatten (List.map stmt ss)
  in
  stmts _stmts

(** Breadth-first (level-order) traversal.
    Visit all nodes at depth d before any node at depth d+1.

    For example, on [Assign("x", BinOp(Add, IntLit 1, IntLit 2))]:
      ["Assign"; "BinOp(+)"; "IntLit(1)"; "IntLit(2)"]
    (In this small case it happens to match pre-order, but differs on
     deeper trees with multiple siblings.)

    Hint: use the OCaml Queue module.
      1. Seed the queue with all top-level stmts.
      2. Dequeue a node, emit its label, enqueue its children.
      3. Repeat until the queue is empty.
    You will need a sum type or two queues to handle both stmt and expr
    nodes uniformly. *)
let bfs (_stmts : stmt list) : string list =
  (* TODO: implement breadth-first traversal using Queue *)
  type node =
    | S of stmt
    | E of expr
  in
  let q = Queue.create () in
  List.iter (fun s -> Queue.add (S s) q) _stmts;

  let rec loop acc =
    if Queue.is_empty q then List.rev acc
    else
      match Queue.take q with
      | S s ->
          let acc = label_of_stmt s :: acc in
          (match s with
           | Assign (_, e) -> Queue.add (E e) q
           | If (cond, t, e) ->
               Queue.add (E cond) q;
               List.iter (fun s -> Queue.add (S s) q) t;
               List.iter (fun s -> Queue.add (S s) q) e
           | While (cond, body) ->
               Queue.add (E cond) q;
               List.iter (fun s -> Queue.add (S s) q) body
           | Return None -> ()
           | Return (Some e) -> Queue.add (E e) q
           | Print exprs ->
               List.iter (fun e -> Queue.add (E e) q) exprs
           | Block stmts_list ->
               List.iter (fun s -> Queue.add (S s) q) stmts_list);
          loop acc

      | E e ->
          let acc = label_of_expr e :: acc in
          (match e with
           | IntLit _ | BoolLit _ | Var _ -> ()
           | UnaryOp (_, e1) -> Queue.add (E e1) q
           | BinOp (_, e1, e2) ->
               Queue.add (E e1) q;
               Queue.add (E e2) q
           | Call (_, args) ->
               List.iter (fun e -> Queue.add (E e) q) args);
          loop acc
  in
  loop []