open Ir

let rec g = function
  | Ans(exp) -> Ans(exp)
  | Let((x, t), Mov(y), e) | Let((x, t), FMov(y), e) when not (Asm.is_reg y) ->
    Format.eprintf "replacing %s with %s\n" x y;
    g (replace x y e)
  | Let((x, t), exp, e) ->
    if List.mem x (fv e) || effect' exp || Asm.is_reg x then
      Let((x, t), exp, g e)
    else
      (Format.eprintf "eliminating %s.\n" x; g e)

let rec iter n e =
  if n = 0 then e
  else
    let e' = g e in
    if e' = e then e else iter (n-1) e'

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = iter 1000 e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, iter 1000 e)
