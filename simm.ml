open Asm

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Seti(i), e) ->
    (* Format.eprintf "found simm %s = %d@." x i; *)
    let e' = g (M.add x i env) e in
    if List.mem x (fv e') then
      Let((x, t), Seti(i), e')
    else
      ((* Format.eprintf "erased redundant Set to %s@." x; *)
        e')
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function
  | Add(x, y) when M.mem y env -> AddI(x, M.find y env)
  | Add(x, y) when M.mem x env -> AddI(y, M.find x env)
  | Ld(x, V(y)) when M.mem y env -> Ld(x, C(M.find y env))
  | St(x, y, V(z)) when M.mem z env -> St(x, y, C(M.find z env))
  | LdF(x, V(y)) when M.mem y env -> LdF(x, C(M.find y env))
  | StF(x, y, V(z)) when M.mem z env -> StF(x, y, C(M.find z env))
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env e1, g env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env e1, g env e2)
  (*| IfGE(x, y', e1, e2) -> IfGE(x, y', g env e1, g env e2)*)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } =
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

let f (Prog(data, fundefs, e)) =
  Prog(data, List.map h fundefs, g M.empty e)
