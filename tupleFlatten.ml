open Closure

let rec flatten_tuple tuple_env = function
  | hd :: tl ->
    if List.mem_assoc hd tuple_env then
      (flatten_tuple tuple_env (List.assoc hd tuple_env)) @ (flatten_tuple tuple_env tl)
    else
      hd :: (flatten_tuple tuple_env tl)
  | [] -> []

let rec convert_let_tuple li orig =
  match (li, orig) with
  | hd1 :: tl1, hd2 :: tl2 -> Let(hd1, Var(hd2), convert_let_tuple tl1 tl2)
  | [], [] -> Unit
  | _ -> raise (Failure "length not matched")

let rec g tuple_env = function
  (* NEXT: fix this. just change let(a, b, c) = (1, 2, 3) to let a = 1 in let b = 2 in let c = 3 *)
  | Let((id, ty), t1, t2) ->
    (match t1 with
     | Tuple(ts) ->
       let tu = Tuple(flatten_tuple tuple_env ts) in
       Let((id, ty), tu, g ((id, ts) :: tuple_env) t2)
     | _ ->
       Let((id, ty), g tuple_env t1, g tuple_env t2))
  | LetTuple(li, id, t) ->
    (try
       let orig = List.assoc id tuple_env in
       convert_let_tuple li orig
     with Not_found -> LetTuple(li, id, g tuple_env t))
  | IfEq(id1, id2, t1, t2) -> IfEq(id1, id2, g tuple_env t1, g tuple_env t2)
  | IfLE(id1, id2, t1, t2) -> IfLE(id1, id2, g tuple_env t1, g tuple_env t2)
  | e -> e


let f (Prog(fns, t)) =
  let new_fns = List.map (fun fn -> { name = fn.name; args = fn.args; formal_fv = fn.formal_fv; body = g [] fn.body }) fns in
  Prog(new_fns, g [] t)
