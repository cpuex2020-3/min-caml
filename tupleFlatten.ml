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
  | Let((id, ty), t1, t2) ->
    (match t1 with
     | Tuple(ts) ->
       let tu = Tuple(flatten_tuple tuple_env ts) in
       Let((id, ty), tu, g ((id, ts) :: tuple_env) t2)
     | _ ->
       Let((id, ty), g tuple_env t1, g tuple_env t2))
  | LetTuple(li, id, t) ->
    let orig = List.assoc id tuple_env in
    convert_let_tuple li orig
  | IfEq(id1, id2, t1, t2) -> IfEq(id1, id2, g tuple_env t1, g tuple_env t2)
  | IfLE(id1, id2, t1, t2) -> IfLE(id1, id2, g tuple_env t1, g tuple_env t2)
  | MakeCls(id, cl, t) -> MakeCls(id, cl, g tuple_env t)
  | e -> e


let f prog =
  let Prog(fns, t) = prog in
  let new_fns = List.map (fun fn -> { name = fn.name; args = fn.args; formal_fv = fn.formal_fv; body = g [] fn.body }) fns in
  Prog(new_fns, g [] t)
