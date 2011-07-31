
{
  open Parser

  exception Error of string

  let incr_linenum lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in 
      lexbuf.Lexing.lex_curr_p <- { pos with 
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1; 
      Lexing.pos_bol = pos.Lexing.pos_cnum; 
    }     
  
  let get_pos lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
     (pos.Lexing.pos_lnum, pos.Lexing.pos_cnum-pos.Lexing.pos_bol)
    
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| [ '\n' ] { incr_linenum lexbuf; token lexbuf }
| ['#']([^'\n']*) { token lexbuf } (* skip comments *)
| ['0'-'9']|['1'-'9']['0'-'9']+ as i { INT((int_of_string i)) }
| ['0'-'9']'.'['0'-'9']+|['1'-'9']['0'-'9']+'.'['0'-'9']+ as f { FLOAT((float_of_string f)) }
| ['"'](([^'"' '\n']*) as str)['"'] { STRING(str) }
| '+' { PLUS }
| '-' { MINUS }
| '*' { MUL }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LSQUARE }
| ']' { RSQUARE }
| '{' { LCURLY }
| '}' { RCURLY }
| '^' { CONCAT }
| '$' { ARRLNG }
| ';' { SCOLON }
| '&' { AND }
| '|' { OR }
| '!' { NOT }
| '=' { EQ }
| '<' { LTHAN }
| '>' { GTHAN }
| ',' { COMMA }
| "<=" { LOETHAN }
| ">=" { GOETHAN }
| "<>" { NEQ }
| "tt" { TRUE }
| "ff" { FALSE }
| "true" { TRUE }
| "false" { FALSE }
| "if" { IF }
| "else" { ELSE }
| "++" { INCR }
| "--" { DECR }
| "i2f" { I2F }
| "while" { WHILE }
| "return" { RETURN }
| "print" { PRINT }
| "int" { TINT }
| "bool" { TBOOL }
| "float" { TFLOAT }
| "string" { TSTRING }
| "unit" { TUNIT }
| (['a'-'z']|['A'-'Z']|['_'])(['0'-'9']|['a'-'z']|['A'-'Z']|['_'])* as s { ID(s) }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }