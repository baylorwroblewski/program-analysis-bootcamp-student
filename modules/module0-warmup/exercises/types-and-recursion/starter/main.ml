(* ================================================================
   Exercise 2: Types and Recursion -- "Mini Expression Tree"
   ================================================================

   Theme: You will define and manipulate a tiny expression tree --
   a miniature version of the AST you will work with in Modules 2-6.
   By the end you will be comfortable with algebraic data types,
   pattern matching, recursion, and Option.

   Run with:  dune exec modules/module0-warmup/exercises/types-and-recursion/starter/main.exe
   ================================================================ *)

(* ----------------------------------------------------------------
   Part 1: Defining the Types

   These are COMPLETE -- do not modify. They mirror the types you
   will see in lib/shared_ast/ast_types.ml starting in Module 2.
   ---------------------------------------------------------------- *)

(** Binary operators. *)
type op =
  | Add  (** + *)
  | Sub  (** - *)
  | Mul  (** * *)

(** Expressions -- a tiny AST. *)
type expr =
  | Num of int           (** integer literal, e.g. 42 *)
  | Var of string        (** variable reference, e.g. "x" *)
  | BinOp of op * expr * expr  (** binary operation, e.g. x + 1 *)

(* ----------------------------------------------------------------
   Part 2: Printing

   Converting an AST back to a readable string is essential for
   debugging. Module 2 has a full ast_printer.ml -- here is a
   simplified version.
   ---------------------------------------------------------------- *)

(** [string_of_op op] returns "+", "-", or "*". *)
let string_of_op (_o : op) : string =
  match _o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
[@@warning "-32"]

(** [string_of_expr e] returns a fully parenthesized string.
    Examples:
      Num 3           --> "3"
      Var "x"         --> "x"
      BinOp(Add, Num 1, Var "x")  --> "(1 + x)" *)
let rec string_of_expr (_e : expr) : string =
  (* EXERCISE: pattern match on Num, Var, BinOp
     Hint: this function needs to be recursive -- add [rec] when ready *)
  match _e with
  | Num n -> string_of_int n
  | Var x -> x
  | BinOp (o, l, r) ->
      "(" ^ string_of_expr l ^ " " ^ string_of_op o ^ " " ^ string_of_expr r ^ ")"


(* ----------------------------------------------------------------
   Part 3: Tree Metrics
   ---------------------------------------------------------------- *)

(** [count_nodes e] returns the total number of nodes in the tree.
    Num and Var are 1 node each. BinOp is 1 + left + right. *)
let rec count_nodes (_e : expr) : int =
  (* EXERCISE: recursive pattern match -- add [rec] when ready *)
  match _e with
  | Num _ -> 1
  | Var _ -> 1
  | BinOp (_, l, r) -> 1 + count_nodes l + count_nodes r

(** [depth e] returns the depth of the tree (Num/Var = 1,
    BinOp = 1 + max of children). *)
let rec depth (_e : expr) : int =
  (* EXERCISE: recursive pattern match, use max -- add [rec] when ready *)
  match _e with
  | Num _ -> 1
  | Var _ -> 1
  | BinOp (_, l, r) -> 1 + max (depth l) (depth r)


(* ----------------------------------------------------------------
   Part 4: Evaluation with Option

   Evaluating an expression that contains variables is not always
   possible (we don't know their values). We use [int option]:
   - Some n  when evaluation succeeds
   - None    when we hit an unknown variable.

   This pattern foreshadows Module 4's abstract interpretation,
   where "we might not know the exact value" is the norm.
   ---------------------------------------------------------------- *)

(** [eval e] evaluates [e] if it contains no variables.
    Returns [Some n] on success, [None] if a Var is encountered.

    For BinOp: if either side is None, the whole result is None.

    Hint: use [match eval left, eval right with]
    to evaluate both sides, then pattern match on the pair. *)
let rec eval (_e : expr) : int option =
  (* EXERCISE: handle Num, Var, and BinOp -- add [rec] when ready *)
  match _e with
  | Num n -> Some n
  | Var _ -> None
  | BinOp (o, l, r) ->
      match eval l, eval r with
      | Some x, Some y ->
          (match o with
           | Add -> Some (x + y)
           | Sub -> Some (x - y)
           | Mul -> Some (x * y))
      | _ -> None

(* ----------------------------------------------------------------
   Part 5: Tree Transformations
   ---------------------------------------------------------------- *)

(** [substitute var_name value e] replaces every [Var var_name]
    in [e] with [Num value], leaving other nodes unchanged.

    Example: substitute "x" 5 (BinOp(Add, Var "x", Num 1))
             --> BinOp(Add, Num 5, Num 1) *)
let rec substitute (_var_name : string) (_value : int) (_e : expr) : expr =
  (* EXERCISE: pattern match; for Var, check if name matches
     Hint: add [rec] when ready *)
  match _e with
  | Num _ -> _e
  | Var x -> if x = _var_name then Num _value else _e
  | BinOp (o, l, r) ->
      BinOp (o, substitute _var_name _value l, substitute _var_name _value r)


(** [vars_in e] returns a sorted, deduplicated list of all variable
    names appearing in [e].

    Hint: collect into a list, then use List.sort_uniq. *)
let vars_in (_e : expr) : string list =
  let rec collect (_e : expr) : string list =
    (* EXERCISE: Num -> [], Var name -> [name], BinOp -> left @ right
       Hint: add [rec] to collect when ready *)
    match _e with
    | Num _ -> []
    | Var x -> [x]
    | BinOp (_, l, r) -> collect l @ collect r
  in
  List.sort_uniq String.compare (collect _e)

(** [is_constant e] returns true if [e] contains no Var nodes. *)
let is_constant (_e : expr) : bool =
  (* EXERCISE: use vars_in or write a direct recursive check *)
  vars_in _e = []

(** [simplify e] performs constant folding: if a BinOp has two Num
    children, replace it with the computed Num.
    Apply recursively (simplify children first, then check).

    Example: BinOp(Add, Num 2, Num 3) --> Num 5 *)
let rec simplify (_e : expr) : expr =
  (* EXERCISE: add [rec] when ready.
     Match on Num/Var (return as-is) and BinOp:
       1. Simplify both children first
       2. If both are Num, compute the result
       3. Otherwise return BinOp(o, left', right') *)
  match _e with
  | Num _ -> _e
  | Var _ -> _e
  | BinOp (o, l, r) ->
      let l' = simplify l in
      let r' = simplify r in
      match l', r' with
      | Num x, Num y ->
          (match o with
           | Add -> Num (x + y)
           | Sub -> Num (x - y)
           | Mul -> Num (x * y))
      | _ -> BinOp (o, l', r')

(* ================================================================
   Main -- runs all exercises and prints results.
   ================================================================ *)
let () =
  Printf.printf "=== Exercise 2: Types and Recursion ===\n\n";

  (* Sample expressions *)
  let e1 = BinOp (Add, Num 2, Num 3) in
  let e2 = BinOp (Mul, Var "x", BinOp (Add, Num 1, Var "y")) in
  let e3 = BinOp (Sub, BinOp (Add, Num 10, Num 20), Num 5) in

  (* Part 2: Printing *)
  Printf.printf "string_of_expr e1 = %s\n" (string_of_expr e1);
  Printf.printf "string_of_expr e2 = %s\n" (string_of_expr e2);
  Printf.printf "string_of_expr e3 = %s\n\n" (string_of_expr e3);

  (* Part 3: Metrics *)
  Printf.printf "count_nodes e1 = %d\n" (count_nodes e1);
  Printf.printf "count_nodes e2 = %d\n" (count_nodes e2);
  Printf.printf "depth e1 = %d\n" (depth e1);
  Printf.printf "depth e2 = %d\n\n" (depth e2);

  (* Part 4: Evaluation *)
  let print_eval label e =
    match eval e with
    | Some n -> Printf.printf "%s = Some %d\n" label n
    | None   -> Printf.printf "%s = None\n" label
  in
  print_eval "eval e1" e1;
  print_eval "eval e2" e2;
  print_eval "eval e3" e3;
  Printf.printf "\n";

  (* Part 5: Transformations *)
  let e2_sub = substitute "x" 3 e2 in
  Printf.printf "substitute \"x\" 3 e2 = %s\n" (string_of_expr e2_sub);
  print_eval "eval (substitute \"x\" 3 e2)" e2_sub;

  Printf.printf "vars_in e2 = [%s]\n"
    (String.concat "; " (vars_in e2));
  Printf.printf "is_constant e1 = %b\n" (is_constant e1);
  Printf.printf "is_constant e2 = %b\n" (is_constant e2);

  Printf.printf "simplify e3 = %s\n" (string_of_expr (simplify e3));
  Printf.printf "simplify e2 = %s\n" (string_of_expr (simplify e2));

  Printf.printf "\nDone!\n"
