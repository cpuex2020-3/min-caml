type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known = function
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) ->
    let toplevel_backup = !toplevel in
    let env' = M.add x t env in
    let known' = S.add x known in
    let e1' = g (M.add_list yts env') known' e1 in
    let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
    let known', e1' =
      if S.is_empty zs then known', e1' else
        (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
         Format.eprintf "function %s cannot be directly applied in fact@." x;
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
    let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in
    let zts = List.map (fun z -> (z, M.find z env')) zs in
    toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel;
    let e2' = g env' known' e2 in
    if S.mem x (fv e2') then
      MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2')
    else
      (Format.eprintf "eliminating closure(s) %s@." x;
       e2')
  | KNormal.App(x, ys) when S.mem x known ->
    Format.eprintf "directly applying %s@." x;
    AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')

let rec print_n_tabs n =
  if n = 0 then ()
  else (Printf.printf " "; print_n_tabs (n-1))

let rec print_t t depth =
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
      print_t thn (depth + 1);
      print_newline();
      print_t els (depth + 1);
      print_newline();
    )
  | IfLE (lhs, rhs, thn, els) -> (
      Printf.printf "IF %s <= %s\n" lhs rhs;
      print_t thn (depth + 1);
      print_newline();
      print_t els (depth + 1);
      print_newline();
    )
  | Let ((id, ty), t1, t2) -> (
      Printf.printf "LET %s TYPE: %s\n" id (Type.str ty);
      print_t t1 (depth + 1);
      print_newline();
      print_t t2 (depth + 1);
      print_newline();
    )
  | Var (v) -> Printf.printf "VAR %s" v
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
  | ExtArray (t) -> let Id.L(s) = t in Printf.printf "EXTARRAY %s\n" s
  | MakeCls((id, ty), cl, z) -> Printf.printf "MAKECLS %s\n" id
  | AppCls(id, args) ->
    Printf.printf "APPCLS %s\n" id;
    print_n_tabs (depth + 1);
    Printf.printf "args:\n";
    print_n_tabs (depth + 2);
    List.iter (fun arg -> (Printf.printf "%s, " arg)) args;
  | AppDir(l, args) ->
    let Id.L(s) = l in
    Printf.printf "APPDIR %s\n" s;
    print_n_tabs (depth + 1);
    Printf.printf "args:\n";
    print_n_tabs (depth + 2);
    List.iter (fun arg -> (Printf.printf "%s, " arg)) args

let rec print prog =
  let Prog(fns, t) = prog in
  List.iter (fun fn -> print_t fn.body 0) fns;
  print_t t 0
