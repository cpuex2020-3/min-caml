open Asm
open Ir

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Seti(i), e) ->
    if i = 0 then
      g env (replace x reg_zero e)
    else
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then
        Let((x, t), Seti(i), e')
      else
        e'
  | Let(xt, Mul(y, i), e) when M.mem y env -> (* for array access *)
    if i = 2 then
      g env (Let(xt, Seti((M.find y env) lsl 1), e))
    else if i = 4 then
      g env (Let(xt, Seti((M.find y env) lsl 2), e))
    else
      raise (Failure "unhandled mul.")
  | Let(xt, Div(y, i), e) when M.mem y env -> (* for array access *)
    if i = 2 then
      g env (Let(xt, Seti((M.find y env) lsr 1), e))
    else if i = 4 then
      g env (Let(xt, Seti((M.find y env) lsr 2), e))
    else
      raise (Failure "unhandled div.")
  | Let((x, t), Mov(y), e) when List.mem y const_regs -> g env (replace x y e)
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function
  (* TODO: Itof *)
  | Seti(i) when i = 0 -> Mov(reg_zero)
  | Add(x, V(y)) when M.mem y env -> Add(x, C(M.find y env))
  | Add(x, V(y)) when M.mem x env -> Add(y, C(M.find x env))
  | Add(x, V(y)) when y = reg_zero -> Nop
  | Add(x, C(i)) when i = 0 -> Nop
  | Ld(x, V(y)) when M.mem y env -> Ld(x, C(M.find y env))
  | St(x, y, V(z)) when M.mem z env -> St(x, y, C(M.find z env))
  | LdF(x, V(y)) when M.mem y env -> LdF(x, C(M.find y env))
  | StF(x, y, V(z)) when M.mem z env -> StF(x, y, C(M.find z env))
  | IfEq(x, V(y), e1, e2) when M.mem y env -> IfEq(x, C(M.find y env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem y env -> IfLE(x, C(M.find y env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem y env -> IfGE(x, C(M.find y env), g env e1, g env e2)
  | IfEq(x, V(y), e1, e2) when M.mem x env -> IfEq(y, C(M.find x env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem x env -> IfGE(y, C(M.find x env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem x env -> IfLE(y, C(M.find x env), g env e1, g env e2)
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env e1, g env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env e1, g env e2)
  | IfGE(x, y', e1, e2) -> IfGE(x, y', g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

let f (Prog(float_data, array_data, fundefs, e)) =
  Prog(float_data, array_data, List.map h fundefs, g M.empty e)
