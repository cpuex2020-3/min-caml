open Asm

let rec g env = function (* Ì¿ÎáÎó¤ÎÂ¨ÃÍºÇÅ¬²½ (caml2html: simm13_g) *)
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Set(i), e) ->
    (* Format.eprintf "found simm %s = %d@." x i; *)
    let e' = g (M.add x i env) e in
    if List.mem x (fv e') then Let((x, t), Set(i), e') else
      ((* Format.eprintf "erased redundant Set to %s@." x; *)
        e')
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function (* ³ÆÌ¿Îá¤ÎÂ¨ÃÍºÇÅ¬²½ (caml2html: simm13_gprime) *)
  | Add(x, y, V(z)) when M.mem z env -> Add(x, x, C(M.find z env))
  | Add(x, y, V(z)) when M.mem y env -> Add(y, y, C(M.find y env))
  | Sub(x, y, V(z)) when M.mem z env -> Sub(x, y, C(M.find z env))
  | Ld(x, V(y), i) when M.mem y env -> Ld(x, C(M.find y env), i)
  | St(x, y, V(z), i) when M.mem z env -> St(x, y, C(M.find z env), i)
  | LdDF(x, V(y), i) when M.mem y env -> LdDF(x, C(M.find y env), i)
  | StDF(x, y, V(z), i) when M.mem z env -> StDF(x, y, C(M.find z env), i)
  | IfEq(x, y, e1, e2) when M.mem y env -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) when M.mem y env -> IfLE(x, y, g env e1, g env e2)
  (*| IfGE(x, V(y), e1, e2) when M.mem y env -> IfGE(x, C(M.find y env), g env e1, g env e2)*)
  (*| IfEq(x, V(y), e1, e2) when M.mem x env -> IfEq(y, C(M.find x env), g env e1, g env e2)*)
  (*| IfLE(x, V(y), e1, e2) when M.mem x env -> IfGE(y, C(M.find x env), g env e1, g env e2)*)
  (*| IfGE(x, V(y), e1, e2) when M.mem x env -> IfLE(y, C(M.find x env), g env e1, g env e2)*)
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env e1, g env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env e1, g env e2)
  (*| IfGE(x, y', e1, e2) -> IfGE(x, y', g env e1, g env e2)*)
  (*| IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)*)
  (*| IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)*)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } = (* ¥È¥Ã¥×¥ì¥Ù¥ë´Ø¿ô¤ÎÂ¨ÃÍºÇÅ¬²½ *)
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

let f (Prog(data, fundefs, e)) = (* ¥×¥í¥°¥é¥àÁ´ÂÎ¤ÎÂ¨ÃÍºÇÅ¬²½ *)
  Prog(data, List.map h fundefs, g M.empty e)
