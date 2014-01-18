open Lang
open Proprules
module Aux = Utils;;

(*module type Parser_type =
sig
  type token
  type result
  val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> result
  val rule : Lexing.lexbuf -> token
end

exception Error of exn * (int * int * string * string)

module Make(T : Parser_type) =
struct
  let parse_buf_exn lexbuf =
  try
    T.input T.rule lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      (*let tail = My_lexer.ruleTail "" lexbuf in*)
      let tail = "" in
        raise (Error (exn,(line,cnum,tok,tail)))
end

let parse_buf lexbuf = try Some (parse_buf_exn lexbuf) with _ -> None

let parse_stdin () = parse_buf (Lexing.from_channel stdin)
let parse_string str = parse_buf (Lexing.from_string str)

let parse_file filename =
let contents = try Std.input_file filename with _ -> "" in
parse_string contents

end*)
    
let print_help () =       
  print_endline "help is outdated"
(*
  print_endline "Autoprover `Murlock' ";
  print_endline "Options";
  print_endline "-dot on              enable image generation (default)";
  print_endline "-dot off             disable image generation";
  print_endline "-o filename          set outfile name (default `fip.dot')";
  print_endline "-u n m               Unify sequence. Next n lines contains sequences";
  print_endline "                     And m lines  - forbidden substitutions.";
  print_endline "<-p> formula         prove formula";
  print_endline "-t fip               extract all variables from fip";
  print_endline "#something           this is `comments'. Murlock skips these lines";
  print_endline "Examples:";
  print_endline "-u 2 1";
  print_endline "P(__m1) # P(__m2) --> P(z1) # P(z2)";
  print_endline "P(__m1) # P(__m2) --> P(z3) # P(z4)";
  print_endline "__m1 => z1 # z2 # z3";
  print_endline "-p (!a&!b)=>!q";
  print_endline "   (!a&!b)=>!q";
  print_endline "ENTER a command" *)
;;

let parse_str x = 
  let buf  =  Lexing.from_string (if (Str.last_chars x 2 != ";;") then x^";;" else x) in
  try
		Some (Parser.parse_fip Lexer.token buf)
  with exn -> begin
    let cur = buf.Lexing.lex_curr_p in
    let n = cur.Lexing.pos_cnum - cur.Lexing.pos_bol in
    print_endline x;
    print_endline ((String.make n ' ')^"^");
    None
  end
(*  if (Str.last_chars x 2 != ";;") then
    Parser.parse_fip Lexer.token (Lexing.from_string (x^";;"))
  else 
    Parser.parse_fip Lexer.token (Lexing.from_string x) *)
;;
let parse_term x = 
  let buf  =  Lexing.from_string (if (Str.last_chars x 2 != ";;") then x^";;" else x) in
  try
       Some (Parser.parse_term Lexer.token buf)
  with exn -> begin
    let cur = buf.Lexing.lex_curr_p in
    let n = cur.Lexing.pos_cnum - cur.Lexing.pos_bol in
    print_endline x;
    print_endline ((String.make n ' ')^"^");
    None
  end
(*
  if (Str.last_chars s 2 != ";;") then
    Parser.parse_term Lexer.token (Lexing.from_string (s^";;"))
  else 
    Parser.parse_term Lexer.token (Lexing.from_string s) *)
;;

exception BadInput of string
open Config;;
let config = Config.default ();;
(*
let get_temp_unif_func () = match !temp_unif_type with
  | Never -> Prover2.unif_never
  | EveryMinus -> Prover2.unif_every_minus
;;
*)
(*
let deriv_printer () = if !light_deriv = true then 
    Prover2.print_deriv_light
  else Prover2.print_deriv_full;;
*)
(*
let doUnify str = 
  let counts = Str.split (Str.regexp " ") str in
  if List.length counts <> 2 then  
    print_endline "specify 2 numbers"
  else begin
    let seq_n  = int_of_string (Aux.trim (List.nth counts 0)) 
    and forb_n = int_of_string (Aux.trim (List.nth counts 1))
    in
     let seqs = ref []
     and forbs = ref UniAns.empty in
     let rec parse_n_strs n func = 
       if n>0 then (
        let s = input_line stdin in
          func s;
          parse_n_strs (n-1) func
        )
     and read_seq s =
       (*print_endline ("argument of read_seq is "^s);*)
       let preseq = Str.split (Str.regexp "-->") s in
       (*print_endline ("List.length preseq = "^ (string_of_int (List.length preseq))); 
       print_endline ("List.nth preseq 0 = "^(List.nth preseq 0));*)
       if List.length preseq <> 2 then raise (BadInput ("No '-->' in the string "^ s));
       let anti_lst = Str.split (Str.regexp "#") (List.nth preseq 0)
       and suc_lst  = Str.split (Str.regexp "#") (List.nth preseq 1)
       and seq : Proprules.sequence = { anti=[]; suc=[] }

       in
         List.iter ( fun x -> (match parse_str x with
                               | Some a -> seq.anti <- a::seq.anti
                               | None   -> print_endline ("string `"^x^"' skipped")
         )) anti_lst;
         List.iter ( fun x -> (match parse_str x with
                               | Some a -> seq.suc <- a::seq.suc
                               | None   -> print_endline ("string `"^x^"' skipped")
         )) suc_lst;
         (*List.iter ( fun x -> (seq.suc  <- (parse_str x)::seq.suc ))  suc_lst;*)
         seqs:= seq::!seqs
     and read_forb s = begin
       let preseq = Str.split (Str.regexp "=>") s in 
       if List.length preseq <> 2 then raise (BadInput ("No `=>' in the string "^ s));
       let left_str = Aux.trim (List.nth preseq 0) in
       
       match parse_term left_str with
       | Some left -> begin
         let right_targets = Str.split (Str.regexp ",") (List.nth preseq 1) in
         let right_ans = ref [] in
         List.iter (fun x ->
           match parse_term x with                       
           | None -> print_endline ("string `"^x^" skipped")
           | Some ( z) -> print_endline ("add "^left_str^" => "^(Lang.term_to_pretty_string z));
                          right_ans := z :: !right_ans
         ) right_targets;
         forbs:= UniAns.add (Lang.MetaVarM.MetaVar left_str) !right_ans !forbs
           (*let right = List.map (parse_term) right_targets in  
             forbs := UniAns.add (Lang.MetaVarM.MetaVar (Aux.trim (List.nth preseq 0))) right !forbs*)
         end
       
       | None -> print_endline ("can't parse left side of string `"^s^"'. string is skipped")
     end
    in 
      parse_n_strs seq_n read_seq; (* parse sequences *) 
      parse_n_strs forb_n read_forb; (* parse forbidden substitutions *)
      print_endline "sequences are:";
      List.iter (fun x -> print_sequence x) !seqs;
      print_endline "forbs are:";
(*      Lang.UniAns.iter (fun k v -> print_endline ((match k with Lang.MetaVarM.MetaVar m-> m)^" --> [ "^(Aux.char_separated_list ';' (Lang.term_to_pretty_string) v)^" ]" ) ) !forbs; *)
      print_endline ("Seqs length = "^string_of_int(List.length !seqs));
      if (UniAns.is_empty !forbs) then
        print_endline ("forbs ares empty")
      else begin
       UniAns.iter (fun k v -> print_endline ((match k with Lang.MetaVarM.MetaVar m -> m)^" => "^(Aux.char_separated_list '#' (term_to_pretty_string) v)) ) !forbs;
      end;
      (*print_endline ("forb length = "^string_of_int(List.length !forbs));*) 
       
      let res = Prover2.unifyByNormalMethod !seqs !forbs in
      match res with
        | (false,_) -> print_endline "Fails\n"
        | (true,subs) -> print_endline "Yeah";   
             List.iter (fun (x,y) -> 
               print_endline ((term_to_pretty_string x)^" in "^(term_to_pretty_string y)) 
             ) subs;   
  end;
  print_endline ""
;;

let doUnify_lib str = 
  begin
    let seq_n  = int_of_string (Aux.trim (str)) in
     let seqs = ref [] in
     let rec parse_n_strs n func = 
       if n>0 then (
        let s = input_line stdin in
          func s;
          parse_n_strs (n-1) func
        )
     and read_seq s =
       let preseq = Str.split (Str.regexp "-->") s in
       if List.length preseq <> 2 then 
         raise (BadInput ("No '-->' in the string "^ s));
       let anti_lst = Str.split (Str.regexp "#") (List.nth preseq 0)
       and suc_lst  = Str.split (Str.regexp "#") (List.nth preseq 1)
       and seq : Proprules.sequence = { anti=[]; suc=[] } in
         List.iter ( fun x -> (match parse_str x with
                               | Some a -> seq.anti <- a::seq.anti
                               | None   -> print_endline ("string `"^x^"' skipped")
         )) anti_lst;
         List.iter ( fun x -> (match parse_str x with
                               | Some a -> seq.suc <- a::seq.suc
                               | None   -> print_endline ("string `"^x^"' skipped")
         )) suc_lst;
         seqs:= seq::!seqs
    in 
      parse_n_strs seq_n read_seq; (* parse sequences *) 
      print_endline "sequences are:";
      List.iter (fun x -> print_sequence x) !seqs;
      print_endline ("Seqs length = "^string_of_int(List.length !seqs));
       
      let res = Unif.unifLiberalized !seqs in
      match res with
        | (false,_) -> print_endline "Fails\n"
        | (true,subs) -> print_endline "Yeah";   
             List.iter (fun (x,y) -> 
               print_endline ((term_to_pretty_string x)^" in "^(term_to_pretty_string y)) 
             ) subs;   
  end;
  print_endline ""
;;
*)
let proveAbstr func temp_f = 
  let formula = temp_f^ (if (Str.last_chars temp_f 2 <> ";;") then ";;" else "")
  in 
  try
    print_endline  ("Input string: "^formula);
    let lexbuf = Lexing.from_string formula in
    let result = Parser.parse_fip Lexer.token lexbuf in
    print_endline ("out formula : "^(Lang.fip_to_pretty_string result));
    let deriv,res,dotfile = func result  in
    if config.graphviz_on then
      match Unix.system ("dot -Tgif <"^ dotfile ^" >"^ dotfile ^".gif") with
      | Unix.WEXITED 127 -> 
	print_endline "dot not found. You can disable graphviz using `-dot off'"
      | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ();

    match res with 
      | Prover2.DepthLimit -> output_string stderr 
          ("Proving of `"^(Lang.fip_to_pretty_string result)^"` failed\n")
      | _ -> print_endline "Proving succesful";
    print_endline ""; 
    flush stdout
  with Parsing.Parse_error ->  print_endline "Parsing.Parse_error"
     | Failure str when str = "lexing: empty token"
                          ->  print_endline ("Failure("^str^")")
     | Lexer.Eof -> (print_endline "Lexer.Eof"; flush stdout )  
;;

let proveNormal s =
  let cutstr = if (Str.string_before s 3 = "-p ") then
                 Str.string_after s 3
               else s
  in 
    proveAbstr (Prover2.prove_classic config) cutstr
;;

let proveKonev s= 
  let cutstr = if (Str.string_before s 3 = "-p2 ") then
                 Str.string_after s 3
               else s
  in 
  proveAbstr (Prover2.prove_liberalised config) cutstr
;;

Log.init "log";;
(* autoclose log on exit *)
at_exit (fun () -> Log.flush; Log.close);;
let prove_by_config s =
  let cutstr = if (Str.string_before s 3 = "-p2 ") then
                 Str.string_after s 3
               else s
  in 
proveAbstr ( (match config.prove_type with
             | Classic -> Prover2.prove_classic 
             | Liberalised -> Prover2.prove_liberalised) config) cutstr
;;

exception BadConfigStr of string * int * string;;
let update_config c str = 
  let s = Utils.trim str in
  if 3 <> String.length s then  raise (BadConfigStr(s,0,"bad string length"))
  else 
    (match s.[0],s.[1],s.[2] with
      | '0',_,_ -> c.prove_type <- Classic;
      | '2',_,_ -> c.prove_type <- Liberalised;
      | _,_,_ 
	  -> raise (BadConfigStr(s,0,"0 or 2 are available")) );
    (match s.[0],s.[1],s.[2] with
      | _,'0',_ -> c.euristics <- false 
      | _,'1',_ -> c.euristics <- true
      | _,_,_ 
	  -> raise (BadConfigStr(s,1,"0 or 1 are available")) );
    match s.[0],s.[1],s.[2] with
      | _,_,'0' -> c.need_temp_unif <- false 
      | _,_,'1' -> c.need_temp_unif <- true
      | _,_,_
	  -> raise (BadConfigStr(s,2,"0 or 1 are available"))
;;

exception Continue;;
let main () =  
  print_endline "Autoprover `Murlock'. type `-h' for help";
  while true do
    try
        let instr = Aux.trim (input_line stdin) in
	if instr = "" then raise Continue;
        if (Str.string_before instr 1 = "#") then raise Continue;
        match instr with
        | ""        ->   raise (BadInput "empty string")
	| "-q" | "-quit" | "#quit;;" | "exit"    -> exit 0;
        | "-h"      -> ( print_help (); raise Continue )
        | "-dot on" -> ( print_endline "graphviz enabled";
                         config.graphviz_on <- true;  raise Continue )
        | "-dot off"-> ( print_endline "graphviz disabled";
                         config.graphviz_on <- false; raise Continue )
        | "-pt 0"
        | "-pt normal"      -> (print_endline "setting normal proving";
                                config.prove_type <- Classic; raise Continue )
        | "-pt 1"
        | "-pt liberalized" -> (print_endline "setting liberalized proving";
                                config.prove_type <- Liberalised; raise Continue )
        | "-dt light"  -> (config.tree_format <- Brief; 
                           print_endline "derivation type: light")
	| "-dt normal" -> (config.tree_format <- Full; 
			   print_endline "derivation type: normal")
	| "-tu 1" | "-tu minus"  -> config.need_temp_unif <- true
	| "-tu 0" | "-tu never"  -> config.need_temp_unif <- false
	| "-ug 1"                -> config.unif_graphs <- true
	| "-ug 0"                -> config.unif_graphs <- false
	| "-ue true"  | "-ue 1"  -> config.euristics <- true
	| "-ue false" | "-ue 0"  -> config.euristics <- false
        | _         -> ();

        let input =
          if (String.length instr < 3)  then "-p " ^ instr 
          else instr
        in
        let input_tail = Aux.trim (Str.string_after input 3) in
        
        match Str.string_before input 3  with
        | "-o " -> config.file_prefix <- Aux.trim input_tail
        | "-uc" -> config.max_uni_count <- int_of_string (Aux.trim input_tail)

	| "-op" -> begin
	  try update_config config input_tail;
	      print_endline "Configs updated.";
	      Config.print_config config
	  with  BadConfigStr (str,n,msg) -> 
	    print_endline "Wrong config!";
	    print_endline str;
	    if n>0 then print_string (String.make (n-1) ' ');
	    print_endline "^";
	    print_endline (msg ^ "\n" )
	  end
        | "-p0"  -> (config.prove_type <- Classic; 
		     prove_by_config input_tail )
        | "-p4"  -> (config.prove_type <- Liberalised;
                     prove_by_config input_tail )       
        | "-p "  -> prove_by_config input_tail       
        | _      -> prove_by_config input 
      with End_of_file  -> (exit 0)
         | BadInput s -> print_endline ("Bad input. " ^ s)
         | Continue -> ()
    done
;;


try main ()
with exc ->
  Printf.eprintf "EXCEPTION!!!";
  print_endline (  Printexc.to_string exc);
(*  print_endline (Printexc.get_backtrace ())   *)
  Printexc.print_backtrace stdout
