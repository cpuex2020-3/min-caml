type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None)

let rec str = function
  | Unit -> "()"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Fun(args, body) ->
    "Fun(" ^
    (List.fold_left (fun cur_s arg -> cur_s ^ ", " ^ (str arg)) "" args) ^
    ") -> " ^ (str body)
  | Tuple li -> "(" ^ (List.fold_left (fun cur_s arg -> cur_s ^ ", " ^ (str arg)) "" li) ^ ")"
  | Array arr -> Printf.sprintf "[%s]" (str arr)
  | Var v -> (
      match !v with
      | Some v -> Printf.sprintf "Var(%s)" (str v)
      | None -> Printf.sprintf "()"
    )
