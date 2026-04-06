and pp_stmt (depth : int) (s : stmt) : string =
  let _pad = indent depth in
  match s with
  | Assign (v, e) ->
      Printf.sprintf "%s%s = %s;" pad v (expr_to_string e)
  | If (cond, then_b, else_b) ->
      if else_b = [] then
        Printf.sprintf "%sif (%s) {\n%s\n%s}"
          pad
          (expr_to_string cond)
          (pp_stmts (depth + 1) then_b)
          pad
      else
        Printf.sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}"
          pad
          (expr_to_string cond)
          (pp_stmts (depth + 1) then_b)
          pad
          (pp_stmts (depth + 1) else_b)
          pad
  | While (cond, body) ->
      Printf.sprintf "%swhile (%s) {\n%s\n%s}"
        pad
        (expr_to_string cond)
        (pp_stmts (depth + 1) body)
        pad
  | Return None ->
      Printf.sprintf "%sreturn;" pad
  | Return (Some e) ->
      Printf.sprintf "%sreturn %s;" pad (expr_to_string e)
  | Print exprs ->
      let es =
        exprs |> List.map expr_to_string |> String.concat ", "
      in
      Printf.sprintf "%sprint(%s);" pad es
  | Block stmts ->
      Printf.sprintf "%s{\n%s\n%s}"
        pad
        (pp_stmts (depth + 1) stmts)
        pad