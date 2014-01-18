%{
	(* Murlock parser *)
	open Lang;;
        module Aux = Utils;;
%}
%token <string> VAR
%token <string> SUBJCONST
%token <string> METAVAR
%token <string> PRED
%token AND OR IMPL EQV NOT SHEFFER PIRCE XOR Q_ALL Q_EXZ TRUE FALSE
%token LPAREN RPAREN COMMA
%token EOL

%left IMPL EQV SHEFFER PIRCE
%left XOR AND OR 
%nonassoc NOT
%start parse_fip
%start parse_term
%type <Lang.fip> parse_fip
%type <Lang.term> parse_term
%type <Lang.fip> fip
%type <Lang.atomFormula> atformula
%type <Lang.term list> termlist
%type <Lang.term> term
%%
parse_fip:
	fip EOL                     { $1 }
;
term:
	| VAR                     { Lang.SubjVar (Aux.trim $1) }
	| SUBJCONST               { Lang.SubjConst (Aux.trim $1) }
	| METAVAR                 { Lang.MetaVar (Aux.trim $1) }
        | PRED LPAREN RPAREN      { Lang.FuncSymbol(Aux.trim $1,[]) }
	| PRED LPAREN termlist RPAREN { Lang.FuncSymbol ((Aux.trim $1), $3) } 
;
termlist:
	| term                     { [$1] }
	| termlist COMMA term      { List.append $1 [$3] }
;
atformula:
	| PRED LPAREN          RPAREN { Lang.AtomFormula (Aux.trim $1, []) }
	| PRED LPAREN termlist RPAREN { Lang.AtomFormula (Aux.trim $1, $3) }
;

fip:
	| TRUE                    { Lang.LogConst ( Lang.True ) }
	| FALSE                   { Lang.LogConst ( Lang.False) }
	| VAR                     { Lang.LogVar (Aux.trim $1) }
	| atformula               { Formula ( $1 ) }

	| Q_ALL VAR fip           { Lang.QALL ($2, $3 ) }
	| Q_EXZ VAR fip           { Lang.QEXZ ($2, $3 ) }

	| LPAREN fip RPAREN       { $2 }
	| fip AND     fip         { Lang.BinOp ("AND", $1, $3)	}
	| fip XOR     fip         { Lang.BinOp ("XOR", $1, $3)  }
	| fip SHEFFER fip         { Lang.BinOp ("SHEFFER",$1, $3) }
	| fip PIRCE   fip         { Lang.BinOp ("BROSNAN", $1, $3) (* =) *) }
	| fip OR  fip             { Lang.BinOp ("OR", $1, $3)	}
	| fip IMPL  fip           { Lang.BinOp ("IMPL", $1, $3)	}
	| fip EQV  fip            { Lang.BinOp ("EQV", $1, $3)	}
	| NOT fip                 { Lang.UnOp  ("NOT", $2) }
;
parse_term:
	term EOL 	{ $1 }
%%
(*
  TODO: compile this functions
let str2fip s = 
  parse_fip Lexer.token (Lexing.from_string 
       (if (Str.last_chars s 2 != ";;") then s^";;" else s));;

let str2term s = 
  parse_term Lexer.token (Lexing.from_string 
       (if (Str.last_chars s 2 != ";;") then s^";;" else s));;
*)
