let limit = ref 1000

let rec iter n e = (* ºÇÅ¬²½½èÍý¤ò¤¯¤ê¤«¤¨¤¹ (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let lexbuf outchan l = (* ¥Ð¥Ã¥Õ¥¡¤ò¥³¥ó¥Ñ¥¤¥ë¤·¤Æ¥Á¥ã¥ó¥Í¥ë¤Ø½ÐÎÏ¤¹¤ë (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  let syntax = Parser.exp Lexer.token l in
  Syntax.print syntax 0;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
          (Virtual.f
             (Closure.f
                (iter !limit
                   (Alpha.f
                      (KNormal.f
                         (Typing.f syntax))))))))

let string s = lexbuf stdout (Lexing.from_string s) (* Ê¸»úÎó¤ò¥³¥ó¥Ñ¥¤¥ë¤·¤ÆÉ¸½à½ÐÎÏ¤ËÉ½¼¨¤¹¤ë (caml2html: main_string) *)

let file f = (* ¥Õ¥¡¥¤¥ë¤ò¥³¥ó¥Ñ¥¤¥ë¤·¤Æ¥Õ¥¡¥¤¥ë¤Ë½ÐÎÏ¤¹¤ë (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ¤³¤³¤«¤é¥³¥ó¥Ñ¥¤¥é¤Î¼Â¹Ô¤¬³«»Ï¤µ¤ì¤ë (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
