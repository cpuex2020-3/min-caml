open KNormal

let threshold = ref 0

let rec size = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfFIsZero(_, e1, e2) | IfFIsPos(_, e1, e2)
  | Let(_, e1, e2) | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | LetTuple(_, _, e) -> 1 + size e
  | _ -> 1

let rec g env = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfFIsZero(x, e1, e2) -> IfFIsZero(x, g env e1, g env e2)
  | IfFIsPos(x, e1, e2) -> IfFIsPos(x, g env e1, g env e2)
  | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
    let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
    LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | App(x, ys) when M.mem x env ->
    let (zs, e) = M.find x env in
    Format.eprintf "inlining %s@." x;
    let env' =
      List.fold_left2
        (fun env' (z, t) y -> M.add z y env')
        M.empty
        zs
        ys in
    Alpha.g env' e
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | e -> e

let f e = g M.empty e
