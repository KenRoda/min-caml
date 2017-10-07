open Syntax
open Format

type debug_level =
  | Parser
  | Typing
  | KNormal
  | Alpha
  | Closure
  | Virtual
  | Simm
  | RegAlloc
  | Emit

 let string_to_debug_level (string:string) :debug_level =
  match string with
    | "Parser"   -> Parser
    | "Typing"   -> Typing
    | "KNormal"  -> KNormal
    | "Alpha"    -> Alpha
    | "Closure"  -> Closure
    | "Virtual"  -> Virtual
    | "Simm"     -> Simm
    | "RegAlloc" -> RegAlloc
    | _          -> Emit

let print_id (id:Id.t) :unit =
  print_string id

let rec print_type = function
     Type.Unit     -> print_string "u"
   | Type.Bool     -> print_string "b"
   | Type.Int      -> print_string "i"
   | Type.Float    -> print_string "d"
   | Type.Fun _    -> print_string "f"
   | Type.Tuple _  -> print_string "t"
   | Type.Array _  -> print_string "a"
   | Type.Var rt   -> (print_string "v{";
                      (match !rt with
                        | None   -> print_string "None"
                        | Some t -> print_type t);
                          print_string "}")
let emit_parser (out_channel:out_channel) (syntax:Syntax.t) :Syntax.t =
  let rec iter (syntax:Syntax.t) :unit =
    (match syntax with
      | Unit ->
        print_string "Unit";
      | Bool b ->
        open_hbox ();
        print_string "Bool";
        print_space ();
        print_bool b;
      | Int i ->
        open_hbox ();
        print_string("Int");
        print_space ();
        print_int i;
      | Float f ->
        open_hbox ();
        print_string("Float");
        print_space ();
        print_float f;
      | Not s ->
        open_vbox 4;
        print_string("Not");
        print_space ();
        iter s;
      | Neg s ->
        open_vbox 4;
        print_string("Neg");
        print_space ();
        iter s;
      | Add (s0, s1)->
        open_vbox 4;
        print_string("Add");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Sub (s0, s1)->
        open_vbox 4;
        print_string("Sub");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | FNeg s ->
        open_vbox 4;
        print_string("FNeg");
        print_space ();
        iter s;
      | FAdd (s0, s1)->
        open_vbox 4;
        print_string("FAdd");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | FSub (s0, s1)->
        open_vbox 4;
        print_string("FSub");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | FMul (s0, s1)->
        open_vbox 4;
        print_string("FMul");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | FDiv (s0, s1)->
        open_vbox 4;
        print_string("FDiv");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Eq (s0, s1)->
        open_vbox 4;
        print_string("Eq");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | LE (s0, s1)->
        open_vbox 4;
        print_string("LE");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | If (s0, s1, s2)->
        open_vbox 4;
        print_string("If");
        print_space ();
        iter s0;
        print_space ();
        iter s1;
        print_space ();
        iter s2;
      | Let ((id, t), s0, s1)->
        open_vbox 4;
        print_string("Let");
        print_space ();
        open_hbox ();
        print_id id;
        print_space ();
        print_type t;
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Var id ->
        open_hbox ();
        print_string "Var";
        print_space ();
        print_id id;
      | LetRec (fundef, s0) ->
        open_vbox 4;
        print_string "LetRec";
        print_space ();
        print_fundef fundef;
        print_space ();
        iter s0;
      | App (s0, s_list) ->
        open_vbox 4;
        print_string "App";
        print_space ();
        iter s0;
        print_space ();
        print_syntax_list s_list;
      | Tuple (s_list) ->
        open_vbox 4;
        print_string "Tuple";
        print_space ();
        print_syntax_list s_list;
      | LetTuple (id_t_list, s0, s1) ->
        open_vbox 4;
        print_string "LetTuple";
        print_space ();
        print_id_type_list id_t_list;
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Array (s0, s1) ->
        open_vbox 4;
        print_string "Array";
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Get (s0, s1) ->
        open_vbox 4;
        print_string "Get";
        print_space ();
        iter s0;
        print_space ();
        iter s1;
      | Put (s0, s1, s2) ->
        open_vbox 4;
        print_string "Put";
        print_space ();
        iter s0;
        print_space ();
        iter s1;
        print_space ();
        iter s2;);
        close_box ();
    and print_fundef ({ name = (id, t); args = args; body = s }:Syntax.fundef) :unit =
      open_vbox 4;
      print_string "fundef";
      print_space ();
      open_hbox ();
      print_string "name:";
      print_space ();
      print_id id;
      close_box ();
      print_space ();
      open_hbox ();
      print_string "type:";
      print_space ();
      print_type t;
      close_box ();
      print_space ();
      open_hbox ();
      print_string "args";
      print_space ();
      print_id_type_list args;
      close_box ();
      print_space ();
      iter s;
      close_box ();
    and print_syntax_list (s_list:Syntax.t list) :unit =
      match s_list with
      | [] -> ()
      | x::xs ->
        iter x;
        print_space ();
        print_syntax_list xs
    and print_id_type_list (lst:(Id.t * Type.t) list) :unit =
      open_hbox ();
      (match lst with
      | [] -> ()
      | [(id,t)] ->
        print_id id;
        print_space ();
        print_type t;
      | (id,t)::xs ->
        print_id id;
        print_space ();
        print_type t;
        print_string ",";
        print_space ();
        print_id_type_list xs);
        close_box ();
    in iter syntax;
        syntax
