let limit = ref 1000
let debug_level = ref Debug.Emit
let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'
let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  (* 変更を加えた　*)
  let parsed = Parser.exp Lexer.token l in
  let typed = Typing.f parsed in
  if (!debug_level = Debug.Parser) then Debug.print_parser outchan typed;
  let knormalized = KNormal.f typed in
  if (!debug_level = Debug.KNormal) then (
      print_string "-------------------------\n";
      print_string "Normal KNormal\n";
      print_string "-------------------------\n";
      KNormal.print_knormal "  " knormalized;
      let list = Hashtbl.create 0 in
      let result = KNormal.delete_duplication list knormalized in
      print_string "-------------------------\n";
      print_string "Delete Duplication\n";
      print_string "-------------------------\n";
      KNormal.print_knormal "  " result;
        );
  let alphaed = Alpha.f knormalized in
  let itered = iter !limit alphaed in
  let closured = Closure.f itered in
  let virutualized = Virtual.f closured in
  let simmed = Simm.f virutualized in
  let regalloced = RegAlloc.f simmed in
  Emit.f outchan regalloced


let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-debug", Arg.String(fun s -> debug_level := Debug.string_to_debug_level s), "debug level")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
