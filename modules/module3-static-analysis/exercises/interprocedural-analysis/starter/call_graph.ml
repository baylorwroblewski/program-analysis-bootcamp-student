(** Interprocedural Analysis: Call Graph Construction

    Build a call graph from a program and use it to answer questions
    about function relationships like reachability and recursion. *)

open Shared_ast.Ast_types

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type call_graph = {
  nodes : StringSet.t;               (** All function names in the program *)
  edges : StringSet.t StringMap.t;   (** caller -> set of callees *)
}

(** Extract function names called in an expression.

    Walk the expression tree and collect every function name that
    appears in a [Call(name, args)] node. Don't forget to also
    recurse into the argument expressions -- a call like
    [f(g(x))] should return both "f" and "g".

    Examples:
    - [IntLit 5] -> []
    - [Call("f", [Var "x"])] -> ["f"]
    - [Call("f", [Call("g", [IntLit 1])])] -> ["f"; "g"]
    - [BinOp(Add, Call("a", []), Call("b", []))] -> ["a"; "b"] *)
(* Hint: You will need [let rec] when you implement this. *)
let rec calls_in_expr (expr : expr) : string list =
  match expr with
  | IntLit _ -> []
  | BoolLit _ -> []
  | Var _ -> []

  | BinOp (_, lhs, rhs) ->
      calls_in_expr lhs @ calls_in_expr rhs

  | UnaryOp (_, e) ->
      calls_in_expr e

  | Call (name, args) ->
      let arg_calls =
        List.flatten (List.map calls_in_expr args)
      in
      name :: arg_calls

(** Extract all function names called in a list of statements.

    Walk every statement recursively:
    - [Assign(_, e)] -> calls in e
    - [If(cond, then_branch, else_branch)] -> calls in cond + both branches
    - [While(cond, body)] -> calls in cond + body
    - [Return(Some e)] -> calls in e
    - [Return(None)] -> []
    - [Print(exprs)] -> calls in each expr
    - [Block(stmts)] -> recurse into stmts *)
(* Hint: You will need [let rec ... and ...] for mutual recursion
   between calls_in_stmts and a per-statement helper. *)
and calls_in_stmts (stmts : stmt list) : string list =
  List.flatten (List.map calls_in_stmt stmts)

and calls_in_stmt (stmt : stmt) : string list =
  match stmt with
  | Assign (_, e) ->
      calls_in_expr e

  | If (cond, then_branch, else_branch) ->
      calls_in_expr cond
      @ calls_in_stmts then_branch
      @ calls_in_stmts else_branch

  | While (cond, body) ->
      calls_in_expr cond
      @ calls_in_stmts body

  | Return (Some e) ->
      calls_in_expr e

  | Return None ->
      []

  | Print exprs ->
      List.flatten (List.map calls_in_expr exprs)

  | Block stmts ->
      calls_in_stmts stmts

(** Build a call graph from a program.

    For each function definition in the program:
    1. Add it as a node in the graph
    2. Find all function calls in its body using [calls_in_stmts]
    3. Record these as edges: caller -> {callees}

    The result should have:
    - [nodes]&#58; the set of all function names
    - [edges]&#58; a map from each function name to the set of functions it calls *)
let build_call_graph (program : program) : call_graph =
  let nodes =
    List.fold_left
      (fun acc func ->
        StringSet.add func.name acc)
      StringSet.empty
      program
  in

  let edges =
    List.fold_left
      (fun acc func ->
        let callees =
          calls_in_stmts func.body
          |> List.fold_left
               (fun s name -> StringSet.add name s)
               StringSet.empty
        in
        StringMap.add func.name callees acc)
      StringMap.empty
      program
  in

  { nodes; edges }

(** Find all functions reachable from a given starting function.

    Perform a BFS or DFS traversal following the call graph edges.
    Return the set of all functions that can be reached (directly
    or transitively), NOT including the starting function itself
    (unless it calls itself recursively).

    Example for: main -> process_data -> helper
    - [reachable_from graph "main"] = {"process_data", "helper"}
    - [reachable_from graph "process_data"] = {"helper"}
    - [reachable_from graph "helper"] = {} *)
let reachable_from (cg : call_graph) (start : string) : StringSet.t =
  let rec dfs visited worklist =
    match worklist with
    | [] -> visited

    | current :: rest ->
        if StringSet.mem current visited then
          dfs visited rest
        else
          let visited = StringSet.add current visited in

          let neighbors =
            match StringMap.find_opt current cg.edges with
            | Some s -> StringSet.elements s
            | None -> []
          in

          dfs visited (neighbors @ rest)
  in

  let direct_neighbors =
    match StringMap.find_opt start cg.edges with
    | Some s -> StringSet.elements s
    | None -> []
  in

  dfs StringSet.empty direct_neighbors

(** Detect recursive functions in the call graph.

    A function is recursive if it appears in a cycle in the call graph.
    This includes:
    - Direct recursion: f calls f
    - Mutual recursion: f calls g, g calls f

    Hint: A function f is recursive if f is in [reachable_from graph f].

    Return a sorted list of all recursive function names. *)
let find_recursive (cg : call_graph) : string list =
  StringSet.elements cg.nodes
  |> List.filter (fun func ->
         let reachable = reachable_from cg func in
         StringSet.mem func reachable)