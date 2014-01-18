(* Murlock lexer *)
{
	open Parser;;
	exception Eof
}
rule token = parse
	| [' ' '\t']           { token lexbuf }     (* empty space is skipped *)
	| ['\n' ]
	| ";;"                 { EOL }
    | "True" 
    | "true" 
	| "TRUE"	       { TRUE }
    | "False"| "false"
	| "FALSE"              { FALSE }
	| "EXISTS"| "EXIST"
 	| "EXZ"                { Q_EXZ } 
	| "ALL"| "ANY" | "All" { Q_ALL }
	| "OR"                 { OR }  
	| '&' | "AND"          { AND }  
	| "XOR"                { XOR }
	| "!" | "NOT"          { NOT }
	| "=>"                 { IMPL }  
	| "==" | "<=>"         { EQV }
	| '|'                  { SHEFFER }
	| "->"                 { PIRCE }
	| ['a'-'z'] ['a'-'z' '0'-'9']* as var    { VAR ( var ) }
	| ['A'-'Z'] ['A'-'Z' '0'-'9']*
	             as pr     { PRED ( pr ) }
  | ['_'] ['a'-'z' '0'-'9']* as c { SUBJCONST (c) }
  | ['_']['_']['a'-'z']* ['a'-'z' '0'-'9']* as m
                         { METAVAR (m) }
	| ','                  { COMMA }
	| '(' | '['            { LPAREN }
	| ')' | ']'            { RPAREN }
	(*| eof                  { raise Eof }*)  
{
	
}	
