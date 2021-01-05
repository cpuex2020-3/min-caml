open Ir

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), exp, e) ->
    if Asm.is_reg x || effect' exp then
      Let((x, t), exp, e)
    else
      let exp' = g' env exp in
      let env = (exp', (x, t)) :: env in
      Let((x, t), exp', g env e)
and g' env = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfGE(x, y, e1, e2) -> IfGE(x, y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e ->
    try
      let (x, t) = List.assoc e env in
      if not (Asm.is_reg x) then
        (match t with
         | Type.Float -> FMov(x)
         | _ -> Mov(x))
      else
        e
    with Not_found -> e

let rec iter n e =
  Format.eprintf "IrCse iteration %d@." n;
  if n = 0 then e
  else
    let e' = g [] e in
    if e = e' then e else iter (n-1) e'

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = iter 1000 e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, iter 1000 e)
