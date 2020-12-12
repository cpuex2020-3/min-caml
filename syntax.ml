type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * int
  | Div of t * int
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | GlobalLet of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_n_tabs n =
  if n = 0 then ()
  else
    (Printf.printf " ";
     print_n_tabs (n-1))

let rec print t depth =
  print_n_tabs depth;
  match t with
  | Unit -> Printf.printf "UNIT"
  | Bool t -> Printf.printf "BOOL %B" t
  | Int t -> Printf.printf "INT %d" t
  | Float t -> Printf.printf "FLOAT %f" t
  | Not t ->
    (Printf.printf "NOT ";
     print t depth)
  | Neg t ->
    (Printf.printf "NEG ";
     print t depth)
  | Add (t1, t2) ->
    (Printf.printf "ADD\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | Sub (t1, t2) ->
    (Printf.printf "SUB\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | Mul (t1, i) ->
    (Printf.printf "MUL\n";
     print t1 (depth + 1);
     print_newline();
     print_int i;
     print_newline())
  | Div (t1, i) ->
    (Printf.printf "DIV\n";
     print t1 (depth + 1);
     print_newline();
     print_int i;
     print_newline())
  | FNeg t ->
    (Printf.printf "FNEG\n";
     print t depth;
     print_newline())
  | FAdd (t1, t2) ->
    (Printf.printf "FADD\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | FSub (t1, t2) ->
    (Printf.printf "FSUB\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | FMul (t1, t2) ->
    (Printf.printf "FMUL\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | FDiv (t1, t2) ->
    (Printf.printf "FDIV\n";
     print t1 (depth + 1);
     print_newline();
     print t2 (depth + 1);
     print_newline())
  | Eq (t1, t2) ->
    (Printf.printf "EQ\n";
     print_n_tabs (depth + 1);
     print t1 depth;
     print t2 depth;
     print_newline())
  | LE (t1, t2) ->
    (Printf.printf "LE\n";
     print_n_tabs (depth + 1);
     print t1 depth;
     print t2 depth;
     print_newline())
  | If (cond, thn, els) ->
    (Printf.printf "IF\n";
     print cond (depth + 1);
     Printf.printf "\n";
     print_n_tabs depth;
     Printf.printf "THEN\n";
     print thn (depth + 1);
     Printf.printf "\n";
     print_n_tabs depth;
     Printf.printf "ELSE\n";
     print els (depth + 1))
  | Let ((id, ty), t1, t2) ->
    (Printf.printf "LET %s TYPE %s\n" id (Type.str ty);
     print t1 depth;
     print_newline();
     print t2 depth;
     print_newline())
  | GlobalLet ((id, ty), t1, t2) ->
    (Printf.printf "GLOBAL_LET %s TYPE %s\n" id (Type.str ty);
     print t1 depth;
     print_newline();
     print t2 depth;
     print_newline())
  | Var id -> Printf.printf "VAR %s" id
  | LetRec (f, ty) ->
    (let (id, t) = f.name in
     let args = f.args in
     let body = f.body in
     Printf.printf "LETREC %s\n" id;
     print_n_tabs (depth + 1);
     Printf.printf "ARGS\n";
     print_n_tabs (depth + 2);
     List.iter (fun arg -> (let (id, ty) = arg in Printf.printf "ID %s, TYPE %s" id (Type.str ty))) args;
     Printf.printf "\n";
     print_n_tabs (depth + 1);
     Printf.printf "BODY\n";
     print body (depth + 1);
     print_newline();
     print ty (depth + 1))
  | App (f, args) ->
    (Printf.printf "APP\n";
     print_n_tabs (depth + 1);
     Printf.printf "FUN(\n";
     print f (depth + 2);
     print_newline();
     print_n_tabs (depth + 1);
     Printf.printf ")\n";
     List.iter (fun arg -> print arg (depth + 1)) args)
  | Tuple li ->
    (Printf.printf "Tuple(\n";
     List.iter (fun e -> print e depth; Printf.printf ",\n") li;
     Printf.printf ")\n")
  | LetTuple (vars, t1 , t2) ->
    (List.iter (
        fun arg -> (
            let (id, ty) = arg in
            Printf.printf "ID %s, TYPE %s" id (Type.str ty)
          )
      ) vars;
     print t1 (depth + 1);
     print t2 (depth + 1))
  | Array (t1, t2) ->
    (Printf.printf "Array(\n";
     print t1 (depth + 1);
     Printf.printf ",\n";
     print t2 (depth + 1);
     Printf.printf ")\n")
  | Get (t1, t2) ->
    (Printf.printf "Get(\n";
     print t1 (depth + 1);
     Printf.printf ",\n";
     print t2 (depth + 1);
     Printf.printf ")\n")
  | Put (t1, t2, t3) ->
    (Printf.printf "Put(\n";
     print t1 (depth + 1);
     Printf.printf ",\n";
     print t2 (depth + 1);
     Printf.printf ",\n";
     print t3 (depth + 1);
     Printf.printf ",\n";
     Printf.printf ")\n")
