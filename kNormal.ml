open ConstExp
(* give names to intermediate values (K-normalization) *)

type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * int
  | Div of Id.t * int
  | FNeg of Id.t
  | FSqr of Id.t
  | Sqrt of Id.t
  | FAbs of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | IfFIsZero of Id.t * t * t
  | IfFIsPos of Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | GlobalLet of (Id.t * Type.t) * ConstExp.t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
  | Itof of Id.t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Itof(x) | FSqr(x) | Sqrt(x) | FAbs(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | Mul(x, _) | Div(x, _) -> S.of_list [x]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfFIsZero(x, e1, e2) | IfFIsPos(x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | GlobalLet((x, t), e1, e2) -> S.empty (* there should be no free variable in global variable. *)
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
    let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
    S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k =
  match e with
  | Var(x) -> k x
  | _ ->
    let x = Id.gentmp t in
    let e', t' = k x in
    Let((x, t), e, e'), t'

let globenv = ref M.empty

(* [XXX]: Need to handle Add, Sub, etc but just ignore them for now (enough for raytracer). *)
let rec const_exp env = function
  | Syntax.Int(i) -> ConstInt(i), Type.Int
  | Syntax.Float(d) -> ConstFloat(d), Type.Float
  | Syntax.Bool(b) -> ConstBool(b), Type.Bool
  | Syntax.Neg(e) ->
    let (e, t) = const_exp env e in
    (match e with
     | ConstInt(i) -> ConstInt(-i), Type.Int
     | ConstFloat(d) -> ConstFloat(-.d), Type.Float
     | _ -> raise (Failure "Not a constant expression in Neg."))
  | Syntax.Array(Syntax.Int(len), init) ->
    let (e, t) = const_exp env init in
    ConstArray(len, e), Type.Array(t)
  | Syntax.Let((x, t), e1, e2) ->
    let (ex, t) = const_exp env e1 in
    const_exp (M.add x (ex, t) env) e2
  | Syntax.Var(x) ->
    if M.mem x env then
      M.find x env
    else
      M.find x !globenv
  | Syntax.Tuple(ts) ->
    let consts = List.map (fun t -> const_exp env t) ts in
    let es = List.map (fun e -> fst e) consts in
    let ts = List.map (fun e -> snd e) consts in
    (ConstTuple(es), Type.Tuple(ts))
  | Syntax.Get(e, Int(i)) ->
    (match e with
     | Syntax.Var(x) ->
       let (ini, _) = M.find x !globenv in
       let (e, t) = const_exp env e in
       (ini, t) (* [XXX] just assume the initial elements are not changed. *)
     | _ -> raise (Failure "Not a Var."))
  | _ as e -> Syntax.print e 0; raise (Failure "Not a constant expression.")

let rec g env = function
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
    insert_let (g env e)
      (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Sub(x, y), Type.Int))
  | Syntax.Mul(e1, i) ->
    insert_let (g env e1)
      (fun x -> Mul(x, i), Type.Int)
  | Syntax.Div(e1, i) ->
    insert_let (g env e1)
      (fun x -> Div(x, i), Type.Int)
  | Syntax.FNeg(e) ->
    insert_let (g env e)
      (fun x -> FNeg(x), Type.Float)
  | Syntax.FSqr(e) ->
    insert_let (g env e)
      (fun x -> FSqr(x), Type.Float)
  | Syntax.Sqrt(e) ->
    insert_let (g env e)
      (fun x -> Sqrt(x), Type.Float)
  | Syntax.FAbs(e) ->
    insert_let (g env e)
      (fun x -> FAbs(x), Type.Float)
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
  | Syntax.Eq _ | Syntax.LE _ | Syntax.FLess _ | Syntax.FIsZero _ | Syntax.FIsPos _ | Syntax.FIsNeg _ as cmp ->
    g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2))
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
  | Syntax.If(Syntax.FLess(e1, e2), e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', t4 = g env e4 in
             IfLE(y, x, e4', e3'), t3))
  | Syntax.If(Syntax.FIsZero(e1), e3, e4) ->
    insert_let (g env e1)
      (fun x ->
         let e3', t3 = g env e3 in
         let e4', t4 = g env e4 in
         IfFIsZero(x, e3', e4'), t3)
  | Syntax.If(Syntax.FIsPos(e1), e3, e4) ->
    insert_let (g env e1)
      (fun x ->
         let e3', t3 = g env e3 in
         let e4', t4 = g env e4 in
         IfFIsPos(x, e3', e4'), t3)
  | Syntax.If(Syntax.FIsNeg(e1), e3, e4) ->
    insert_let (g env e1)
      (fun x ->
         let e3', t3 = g env e3 in
         let e4', t4 = g env e4 in
         IfFIsPos(x, e4', e3'), t3)
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2))
  | Syntax.Let((x, t), e1, e2) ->
    let (e1', t1) = g env e1 in
    let (e2', t2) = g (M.add x t env) e2 in
    Let((x, t), e1', e2'), t2
  | Syntax.GlobalLet((x, t), e1, e2) ->
    let (const, t1) = const_exp (M.empty) e1 in
    globenv := M.add x (const, t1) !globenv;
    let (e2', t2) = g env e2 in
    GlobalLet((x, t), const, e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) when M.mem x !Typing.globenv -> Var(x), M.find x !Typing.globenv
  | Syntax.Var(x) when M.mem x !Typing.extenv ->
    (match M.find x !Typing.extenv with
     | Type.Array(_) as t -> ExtArray x, t
     | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.Var(x) -> raise (Failure (Printf.sprintf "variable named %s not found." x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
    let env' = M.add x t env in
    let e2', t2 = g env' e2 in
    let e1', t1 = g (M.add_list yts env') e1 in
    LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) ->
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
  | Syntax.Itof(e) ->
    (match g env e with
     | _, Type.Int as g_e ->
       insert_let g_e
         (fun x -> Itof(x), Type.Float)
     | _ -> assert false)
  | Syntax.Get(e1, e2) ->
    (match g env e1 with
     | _, Type.Array(t) as g_e1 ->
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
  | Mul (lhs, rhs) -> Printf.printf "Mul %s %d\n" lhs rhs
  | Div (lhs, rhs) -> Printf.printf "Div %s %d\n" lhs rhs
  | FNeg id -> Printf.printf "FNEG %s\n" id
  | FSqr id -> Printf.printf "FSQR %s\n" id
  | Sqrt id -> Printf.printf "SQRT %s\n" id
  | FAbs id -> Printf.printf "FABS %s\n" id
  | FAdd (lhs, rhs) -> Printf.printf "FADD %s %s\n" lhs rhs
  | FSub (lhs, rhs) -> Printf.printf "FSUB %s %s\n" lhs rhs
  | FMul (lhs, rhs) -> Printf.printf "FMUL %s %s\n" lhs rhs
  | FDiv (lhs, rhs) -> Printf.printf "FDIV %s %s\n" lhs rhs
  | IfEq (lhs, rhs, thn, els) ->
    (Printf.printf "IF %s = %s\n" lhs rhs;
     print thn (depth + 1);
     print_newline();
     print els (depth + 1);
     print_newline())
  | IfLE (lhs, rhs, thn, els) ->
    (Printf.printf "IF %s <= %s\n" lhs rhs;
     print thn (depth + 1);
     print_newline();
     print els (depth + 1);
     print_newline())
  | IfFIsZero(x, thn, els) ->
    (Printf.printf "IFFISZERO %s\n" x;
     print els (depth + 1);
     print_newline())
  | IfFIsPos(x, thn, els) ->
    (Printf.printf "IFFISPOS %s\n" x;
     print els (depth + 1);
     print_newline())
  | Let ((id, ty), t1, t2) ->
    (Printf.printf "LET %s TYPE: %s\n" id (Type.str ty);
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | GlobalLet ((id, ty), t1, t2) ->
    (Printf.printf "GLOBAL_LET %s TYPE: %s\n" id (Type.str ty);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | Var (v) -> Printf.printf "VAR %s" v
  | LetRec (f, ty) ->
    (let (id, t) = f.name in
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
     print_newline())
  | App (id, li) ->
    (Printf.printf "APP\n";
     print_n_tabs (depth + 1);
     Printf.printf "ID: %s\n" id;
     print_n_tabs (depth + 1);
     Printf.printf "ARGS: [\n";
     List.iter (fun arg -> (print_n_tabs (depth + 2); print_string arg; print_newline();)) li;
     print_n_tabs (depth + 1);
     Printf.printf "]\n")
  | Tuple (li) ->
    (Printf.printf "TUPLE [\n";
     List.iter (fun arg -> (print_n_tabs (depth + 1); print_string arg; print_newline();)) li;
     print_n_tabs depth;
     Printf.printf "]\n")
  | LetTuple (li, id, t) ->
    (Printf.printf "LETTUPLE (\n";
     List.iter
       (fun arg ->
          (let (id, ty) = arg in
           print_n_tabs (depth + 1);
           Printf.printf "ID: %s, TYPE: %s\n" id (Type.str ty);
           print_newline()))
       li;
     print_n_tabs depth;
     Printf.printf ")\n")
  | Get (t1, t2) -> Printf.printf "GET %s %s\n" t1 t2
  | Put (t1, t2, t3) -> Printf.printf "PUT %s %s %s\n" t1 t2 t3
  | ExtArray (t) -> Printf.printf "EXTARRAY %s\n" t
  | ExtFunApp (id, li) ->
    (Printf.printf "EXTFUNAPP %s [" id;
     List.iter (fun id -> Printf.printf "%s " id) li;
     Printf.printf "]\n")
  | Itof (e) -> (Printf.printf "ITOF %s\n" e)
