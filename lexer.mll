{
  exception Eof
}
rule token = parse
	 [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)

  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | "function"      { FUNCTION }
  | "->"            { FN_ARROW }
  | '_'             { WILDCARD }
  | '|'             { PIPE }
  | '['             { LBRACK }
  | ']'             { RBRACK }
  | ';'             { SEMICOLON }
  | "::"            { CONS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "rec"           { REC }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "="             { EQ }
  | "in"            { IN }
  | ':'             { COLON }
  | '>'             { GT }
  

  | ";;"            { EOD }
  | eof             { raise Eof }

  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | "true"|"false" as lxm { BOOL(bool_of_string lxm) }

  | ['a'-'z'] (['a'-'z' 'A'-'Z' '0'-'9']*) as name { VAR(name) }
