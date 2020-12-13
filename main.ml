let limit = ref 1000

let rec iter n e =
  Format.eprintf "iteration %d@." n;
  if n = 0 then e
  else
    let e = Cse.f e in
    (*print_string "** After cse **\n";*)
    (*KNormal.print cse 0;*)
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let lexbuf outchan l =
  Id.counter := 0;
  let syntax = Parser.toplevel Lexer.token l in
  (*print_string "** Output of Syntax.t **\n";*)
  (*Syntax.print syntax 0;*)
  let kNormal = KNormal.f (Typing.f syntax) in
  (*print_string "\n** Output of KNormal.t **\n";*)
  (*KNormal.print kNormal 0;*)
  let closure = Closure.f (iter !limit (Alpha.f kNormal)) in
  (*let flatten = TupleFlatten.f closure in*)
  let flatten = closure in
  (*print_string "\n** Output of Closure.t after tuple flattening **\n";*)
  (*Closure.print flatten;*)
  Emit.f outchan
    (Gen.f
       (RegAlloc.f
          (Simm.f
             (Virtual.f flatten))))

let string s = lexbuf stdout (Lexing.from_string s)
let globals_path = ref "./raytracer/globals.ml"

let gfile g =
  Typing.extenv := M.empty;
  let _ = Virtual.f (Closure.f (KNormal.f (Typing.f (Parser.toplevel Lexer.token g)))) in
  ()

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-globals", Arg.String(fun s -> globals_path := s), "path to the global file.") ]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-globlas s] ...filenames without \".ml\"..." Sys.argv.(0));
  let gchan = open_in !globals_path in
  gfile (Lexing.from_channel gchan);
  List.iter
    (fun f -> ignore (file f))
    !files
