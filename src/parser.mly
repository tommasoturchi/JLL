%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token TRUE FALSE TINT TBOOL TFLOAT TSTRING TUNIT
%token PLUS MINUS MUL DIV EQ NEQ GTHAN LTHAN GOETHAN LOETHAN AND OR NOT SCOLON COMMA CONCAT ARRLNG INCR DECR I2F
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token IF ELSE RETURN WHILE PRINT
%token EOF

%start <Structs.prg> main

%%

main:
  p=list(fndecl) EOF { p }

fndecl:
	t=rtype fname=ID LPAREN fps=separated_list(COMMA,par) RPAREN fbody=stmt { ( fname , t , fps , fbody ) }

rtype:
| a=atype { a }
| TUNIT { Structs.TUNIT }

par:
  tp=atype varid=ID { ( varid , tp ) }

atype:
| b=bstp { b }
| b=bstp LSQUARE RSQUARE { Structs.TARR( b ) }

bstp:
| TINT { Structs.TINT }
| TBOOL { Structs.TBOOL }
| TFLOAT { Structs.TFLOAT }
| TSTRING { Structs.TSTRING }

block:
| LCURLY d=decl s=stmts RCURLY { { Structs.decl = d ; Structs.cmds = Some( s ) } }
| LCURLY d=decl RCURLY { { Structs.decl = d ; Structs.cmds = None } }

decl:
| { [] }
| tp=bstp varid=ID SCOLON ds=decl { ( varid , tp , None )::ds }
| tp=bstp LSQUARE RSQUARE varid=ID LSQUARE e=expr RSQUARE SCOLON ds=decl { ( varid , Structs.TARR( tp ) , Some(e) )::ds } 

stmts:
| s=stmt { Structs.CMD( s ) }
| s1=stmts s2=stmt { Structs.SEQ( s1 , s2 ) }

stmt:
| s=matched_stmt { s }
| s=open_stmt { s }

matched_stmt:
| IF e=expr s1=matched_stmt ELSE s2=matched_stmt { Structs.IFELSE( e , s1 , s2 ) }
| x=ID EQ e=expr SCOLON { Structs.SET( x , e ) }
| x=ID LSQUARE i=expr RSQUARE EQ e=expr SCOLON { Structs.SETARR( x , i , e ) }
| x=ID LPAREN e=separated_list(COMMA,expr) RPAREN SCOLON { Structs.PROC( x , e ) }
| x=ID INCR SCOLON { Structs.INCR( x ) }
| x=ID DECR SCOLON { Structs.DECR( x ) }
| x=ID LSQUARE i=expr RSQUARE INCR SCOLON { Structs.INCRA( x , i ) }
| x=ID LSQUARE i=expr RSQUARE DECR SCOLON { Structs.DECRA( x , i ) }
| RETURN e=expr SCOLON { Structs.RETURN( Some( e ) ) }
| RETURN SCOLON { Structs.RETURN( None ) }
| PRINT e=expr SCOLON { Structs.PRINT( e ) }
| b=block { Structs.BLOCK( b , { Structs.current = [] ; Structs.prec = None } ) }
| WHILE e=expr s=matched_stmt { Structs.WHILE( e , s ) }

open_stmt:
| IF e=expr s=stmt { Structs.IF( e , s ) }
| IF e=expr s1=matched_stmt ELSE s2=open_stmt { Structs.IFELSE( e , s1 , s2 ) }
| WHILE e=expr s=open_stmt { Structs.WHILE( e , s ) }

expr:
	e=or_expr { e }

uop:
| NOT { Structs.NOT }
| MINUS { Structs.INV }
| ARRLNG { Structs.ARRLNG }
| I2F { Structs.ITF }

relop:
| LOETHAN { Structs.LOETHAN }
| LTHAN { Structs.LTHAN }
| GOETHAN { Structs.GOETHAN }
| GTHAN { Structs.GTHAN }
| EQ { Structs.EQ }
| NEQ { Structs.NEQ }

or_expr:
| e1=and_expr OR e2=or_expr { Structs.BOP( Structs.OR , e1 , e2 , ref Structs.TANY) }
| e=and_expr { e }

and_expr:
| e1=rel_expr AND e2=and_expr { Structs.BOP( Structs.AND , e1 , e2 , ref Structs.TANY) }
| e=rel_expr { e } 

rel_expr:
| e1=sum_expr o=relop e2=rel_expr { Structs.BOP( o , e1 , e2 , ref Structs.TANY) }
| e=sum_expr { e }

sum_expr:
| e1=diff_expr PLUS e2=sum_expr { Structs.BOP( Structs.PLUS , e1 , e2 , ref Structs.TANY) }
| e1=diff_expr CONCAT e2=sum_expr { Structs.BOP( Structs.CONCAT , e1 , e2 , ref Structs.TANY) }
| e=diff_expr { e }

diff_expr:
| e1=mul_expr MINUS e2=diff_expr { Structs.BOP( Structs.MINUS , e1 , e2 , ref Structs.TANY) }
| e=mul_expr { e }

mul_expr:
| e1=div_expr MUL e2=mul_expr { Structs.BOP( Structs.TIMES , e1 , e2 , ref Structs.TANY) }
| e=div_expr { e }

div_expr:
| e1=base_expr DIV e2=div_expr { Structs.BOP( Structs.DIVIDE , e1 , e2 , ref Structs.TANY) }
| e=base_expr { e }

base_expr:
| TRUE { Structs.BOOL( true , ref Structs.TBOOL ) }
| FALSE { Structs.BOOL( false , ref Structs.TBOOL ) }
| i=INT { Structs.INT( i , ref Structs.TINT ) }
| f=FLOAT { Structs.FLOAT( f , ref Structs.TFLOAT ) }
| s=STRING { Structs.STRING( s , ref Structs.TSTRING ) }
| o=uop e=base_expr { Structs.UOP( o , e , ref Structs.TANY ) }
| LPAREN e=expr RPAREN { e }
| x=ID { Structs.ID( x , ref Structs.TANY ) }
| x=ID LPAREN e=separated_list(COMMA,expr) RPAREN { Structs.CALL( x , e , ref Structs.TANY ) }
| x=ID LSQUARE e=expr RSQUARE { Structs.ARRELM( x , e , ref Structs.TANY ) }
| LCURLY e=separated_list(COMMA,expr) RCURLY { Structs.ARR( e , ref Structs.TANY ) }