(* give names to intermediate values (K-normalization) *)

type t = (* KÀµµ¬²½¸å¤Î¼° (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* Èæ³Ó + Ê¬´ô (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* Èæ³Ó + Ê¬´ô *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_n_tabs n =
  if n = 0 then ()
  else (Printf.printf " "; print_n_tabs (n-1))

let rec print t depth =
  print_n_tabs depth;
  match t with
  | Unit -> Printf.printf "UNIT"
  | Int i -> Printf.printf "INT %d" i
  | Float f -> Printf.printf "FLOAT %f" f
  | Neg id -> Printf.printf "NEG %s" id
  | Add (lhs, rhs) -> Printf.printf "ADD %s %s\n" lhs rhs
  | Sub (lhs, rhs) -> Printf.printf "SUB %s %s\n" lhs rhs
  | FNeg id -> Printf.printf "FNEG %s" id
  | FAdd (lhs, rhs) -> Printf.printf "FADD %s %s\n" lhs rhs
  | FSub (lhs, rhs) -> Printf.printf "FSUB %s %s\n" lhs rhs
  | FMul (lhs, rhs) -> Printf.printf "FMUL %s %s\n" lhs rhs
  | FDiv (lhs, rhs) -> Printf.printf "FDIV %s %s\n" lhs rhs
  | IfEq (lhs, rhs, thn, els) -> (
      Printf.printf "IF %s = %s\n" lhs rhs;
      print thn (depth + 1);
      print_newline();
      print els (depth + 1);
      print_newline();
    )
  | IfLE (lhs, rhs, thn, els) -> (
      Printf.printf "IF %s <= %s\n" lhs rhs;
      print thn (depth + 1);
      print_newline();
      print els (depth + 1);
      print_newline();
    )
  | Let ((id, ty), t1, t2) -> (
      Printf.printf "LET %s TYPE: %s\n" id (Type.str ty);
      print t1 (depth + 1);
      print_newline();
      print t2 (depth + 1);
      print_newline();
    )
  | Var (v) -> Printf.printf "VAR %s" v
  | LetRec (f, ty) -> (
      let (id, t) = f.name in
      let args = f.args in
      let body = f.body in
      Printf.printf "LETREC %s\n" id;
      print_n_tabs (depth + 1);
      Printf.printf "ARGS\n";
      print_n_tabs (depth + 2);
      List.iter (fun arg -> (let (id, ty) = arg in Printf.printf "ID %s, TYPE %s" id (Type.str ty))) args;
      print_newline();
      print_n_tabs (depth + 1);
      Printf.printf "BODY\n";
      print body (depth + 1);
      print_newline();
      print ty (depth + 1);
      print_newline();
    )
  | App (id, li) -> (
      Printf.printf "APP\n";
      print_n_tabs (depth + 1);
      Printf.printf "ID: %s\n" id;
      print_n_tabs (depth + 1);
      Printf.printf "ARGS: [\n";
      List.iter (fun arg -> (print_n_tabs (depth + 2); print_string arg; print_newline();)) li;
      print_n_tabs (depth + 1);
      Printf.printf "]\n";
    )
  | Tuple (li) -> (
      Printf.printf "TUPLE [\n";
      List.iter (fun arg -> (print_n_tabs (depth + 1); print_string arg; print_newline();)) li;
      print_n_tabs depth;
      Printf.printf "]\n";
    )
  | LetTuple (li, id, t) -> (
      Printf.printf "LETTUPLE (\n";
      List.iter (
        fun arg -> (
            let (id, ty) = arg in
            print_n_tabs (depth + 1);
            Printf.printf "ID: %s, TYPE: %s\n" id (Type.str ty);
            print_newline();
          )
      ) li;
      print_n_tabs depth;
      Printf.printf ")\n";
    )
  | Get (t1, t2) -> Printf.printf "GET %s %s\n" t1 t2
  | Put (t1, t2, t3) -> Printf.printf "PUT %s %s %s\n" t1 t2 t3
  | ExtArray (t) -> Printf.printf "EXTARRAY %s\n" t
  | ExtFunApp (id, li) -> (
      Printf.printf "EXTFUNAPP %s [" id;
      List.iter (fun id -> Printf.printf "%s " id) li;
      Printf.printf "]\n";
    )

let rec fv = function (* ¼°¤Ë½Ð¸½¤¹¤ë¡Ê¼«Í³¤Ê¡ËÊÑ¿ô (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
    let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
    S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k = (* let¤òÁÞÆþ¤¹¤ëÊä½õ´Ø¿ô (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
    let x = Id.gentmp t in
    let e', t' = k x in
    Let((x, t), e, e'), t'

let rec g env = function (* KÀµµ¬²½¥ë¡¼¥Á¥óËÜÂÎ (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* ÏÀÍýÃÍtrue, false¤òÀ°¿ô1, 0¤ËÊÑ´¹ (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
    insert_let (g env e)
      (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* Â­¤·»»¤ÎKÀµµ¬²½ (caml2html: knormal_add) *)
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
    insert_let (g env e)
      (fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
    g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* not¤Ë¤è¤ëÊ¬´ô¤òÊÑ´¹ (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', t4 = g env e4 in
             IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', t4 = g env e4 in
             IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* Èæ³Ó¤Î¤Ê¤¤Ê¬´ô¤òÊÑ´¹ (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
    let e1', t1 = g env e1 in
    let e2', t2 = g (M.add x t env) e2 in
    Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> (* ³°ÉôÇÛÎó¤Î»²¾È (caml2html: knormal_extarray) *)
    (match M.find x !Typing.extenv with
     | Type.Array(_) as t -> ExtArray x, t
     | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
    let env' = M.add x t env in
    let e2', t2 = g env' e2 in
    let e1', t1 = g (M.add_list yts env') e1 in
    LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* ³°Éô´Ø¿ô¤Î¸Æ¤Ó½Ð¤· (caml2html: knormal_extfunapp) *)
    (match M.find f !Typing.extenv with
     | Type.Fun(_, t) ->
       let rec bind xs = function (* "xs" are identifiers for the arguments *)
         | [] -> ExtFunApp(f, xs), t
         | e2 :: e2s ->
           insert_let (g env e2)
             (fun x -> bind (xs @ [x]) e2s) in
       bind [] e2s (* left-to-right evaluation *)
     | _ -> assert false)
  | Syntax.App(e1, e2s) ->
    (match g env e1 with
     | _, Type.Fun(_, t) as g_e1 ->
       insert_let g_e1
         (fun f ->
            let rec bind xs = function (* "xs" are identifiers for the arguments *)
              | [] -> App(f, xs), t
              | e2 :: e2s ->
                insert_let (g env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s) (* left-to-right evaluation *)
     | _ -> assert false)
  | Syntax.Tuple(es) ->
    let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
      | [] -> Tuple(xs), Type.Tuple(ts)
      | e :: es ->
        let _, t as g_e = g env e in
        insert_let g_e
          (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
    bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
    insert_let (g env e1)
      (fun y ->
         let e2', t2 = g (M.add_list xts env) e2 in
         LetTuple(xts, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
    insert_let (g env e1)
      (fun x ->
         let _, t2 as g_e2 = g env e2 in
         insert_let g_e2
           (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array" in
              ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
    (match g env e1 with
     |        _, Type.Array(t) as g_e1 ->
       insert_let g_e1
         (fun x -> insert_let (g env e2)
             (fun y -> Get(x, y), t))
     | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> insert_let (g env e3)
              (fun z -> Put(x, y, z), Type.Unit)))

let f e = fst (g M.empty e)