open Ir

let rec effect = function
  | Ans(exp) -> effect' exp
  | Let((x, t), exp, e) -> Asm.is_reg x || effect' exp || effect e
and effect' = function
  | Ld _ | LdF _ | CallCls _ | CallDir _ | Save _ | Restore _ -> true
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfGE(_, _, e1, e2) | IfFEq(_, _, e1, e2) | IfFLE(_, _, e1, e2) ->
    effect e1 || effect e2
  | _ -> false

let extend key value env = (key, value) :: env

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t) as xt, exp, e) ->
    let exp' = g' env exp in
    if Asm.is_reg x || effect' exp then
      Let(xt, exp', g env e)
    else
      Let(xt, exp', g (extend exp' xt env) e)
and g' env = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfGE(x, y, e1, e2) -> IfGE(x, y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e ->
    try
      let (x, t) = List.assoc e env in
      (match t with
       | Type.Int | Type.Array _ -> Mov(x)
       | Type.Float -> FMov(x)
       | Type.Unit -> Nop
       | _ -> Format.eprintf "Ignoring due to unhandled type %s\n" (Type.str t); e)
    with Not_found -> e

let rec iter n e =
  if n = 1000 then e
  else
    let e' = g [] e in
    if e = e' then
      (if n > 0 then Format.eprintf "IrCse iterated %d@." n;
       e)
    else iter (n+1) e'

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = iter 0 e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, iter 0 e)
