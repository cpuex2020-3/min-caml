let frontend_limit = ref 1000
let backend_limit = ref 100
let out_file = ref None

let rec iter_frontend n e =
  Format.eprintf "iteration %d@." n;
  if n = 0 then e
  else
    let e = Cse.f e in
    (*print_string "** After cse **\n";*)
    (*KNormal.print cse 0;*)
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter_frontend (n - 1) e'

let rec iter_backend n e =
  Format.eprintf "iteration backend %d@." n;
  if n = 0 then e
  else
    let e' = Peephole.f e in
    if e = e' then e else
      iter_backend (n - 1) e'

let lexbuf outchan l =
  Id.counter := 0;
  let syntax = Parser.toplevel Lexer.token l in
  (*print_string "** Output of Syntax.t **\n";*)
  (*Syntax.print syntax 0;*)
  let kNormal = KNormal.f (Typing.f syntax) in
  (*print_string "\n** Output of KNormal.t **\n";*)
  (*KNormal.print kNormal 0;*)
  let closure = Closure.f (iter_frontend !frontend_limit (Alpha.f kNormal)) in
  (*let flatten = TupleFlatten.f closure in*)
  let flatten = closure in
  (*print_string "\n** Output of Closure.t after tuple flattening **\n";*)
  (*Closure.print flatten;*)
  Emit.f outchan
    (iter_backend !backend_limit
       (Gen.f
          (RegAlloc.f
             (Simm.f
                (IrElim.f
                   (Virtual.f flatten))))))

let string s = lexbuf stdout (Lexing.from_string s)
let globals_path = ref "./raytracer/globals.ml"

let gfile g =
  Typing.extenv := M.empty;
  let _ = Virtual.f (Closure.f (KNormal.f (Typing.f (Parser.toplevel Lexer.token g)))) in
  ()

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outfile = match !out_file with
    | Some(f) -> f
    | None -> f ^ ".s"
  in
  let outchan = open_out outfile in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  (* TODO: fix this optioning *)
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-globals", Arg.String(fun s -> globals_path := s), "path to the global file.");
     ("-core", Arg.Unit(fun _ -> Asm.is_core := true), "for core or not.");
     ("-addressing", Arg.String(function
          | "byte" ->
            Asm.is_word_addressing := false;
            Asm.inc := 4;
            Asm.data_top_default := 21 * 4;
          | "word" ->
            Asm.is_word_addressing := true;
            Asm.inc := 1;
            Asm.data_top_default := 21;
          | _ -> raise (Failure "Unsupported addressing mode.")
        ), "word addressing. default is byte addressing.");
     ("-o", Arg.String(fun s -> out_file := Some(s)), "assembly file name.")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-globlas s] ...filenames without \".ml\"..." Sys.argv.(0));
  let gchan = open_in !globals_path in
  gfile (Lexing.from_channel gchan);
  List.iter
    (fun f -> ignore (file f))
    !files
