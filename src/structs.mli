(* Abstract Syntax Tree *)

(* JLL's Data Structures *)
type atype =
	| TINT
	| TBOOL
	| TFLOAT
	| TSTRING
	| TARR of atype
	| TUNIT
	| TANY
type binop =
	| PLUS (* Sum *)
	| MINUS (* Diff *)
	| TIMES (* Mul *)
	| DIVIDE (* Div *)
	| EQ (* Equality *)
	| NEQ (* Inequality *)
	| GOETHAN (* Greater or equal than *)
	| GTHAN (* Greater than *)
	| LOETHAN (* Lower or equal than *)
	| LTHAN (* Lower than *)
	| AND (* Logic and *)
	| OR (* Logic or *)
	| CONCAT (* String concatenation *)
type unop =
	| NOT (* Logic negation *)
	| INV (* Opposite *)
	| ARRLNG (* Array length *)
	| ITF (* Integer to float *)
type par = string*atype

(* Environments *)
type idrec = { varid : string ; vartype : atype ; reg : int }
type blstb = idrec list
type blenv = { mutable current : blstb ; mutable prec : blenv option }

(* Expression's Abstract Syntax Tree *)
type expr =
	| INT of int*(atype ref)
	| BOOL of bool*(atype ref)
	| FLOAT of float*(atype ref)
	| STRING of string*(atype ref)
	| ID of string*(atype ref)
	| ARRELM of string*expr*(atype ref)
	| ARR of (expr list)*(atype ref)
	| BOP of binop*expr*expr*(atype ref)
	| UOP of unop*expr*(atype ref)
	| CALL of string*expr list*(atype ref)

(* Statements' Abstract Syntax Tree *)
type decl = string*atype*(expr option)
type cmd =
	| PROC of string*expr list
	| IF of expr*cmd
	| IFELSE of expr*cmd*cmd
	| WHILE of expr*cmd
	| SET of string*expr
	| SETARR of string*expr*expr
	| PRINT of expr
	| RETURN of expr option
	| BLOCK of block*blenv
	| INCR of string
	| INCRA of string*expr
	| DECR of string
	| DECRA of string*expr
and cmds =
	| SEQ of cmds*cmd
	| CMD of cmd
and block = { decl : decl list ; cmds : cmds option }

(* Functions' Abstract Syntax Tree *)
type fn = string*atype*(par list)*cmd

(* Final Abstract Syntax Tree *)
type prg = fn list

(* 3AC generation *)
type tacad =
	| CLAB of int
	| CIDN of string
	| CTMP of int
	| CINT of int
	| CBOL of int
	| CFLT of float
	| CSTR of string
	| CAID of tacad*tacad
	| CBLC of tac4ple list
	| CENV of blenv ref
	| CTYP of atype
and tacop =
	| IPS (* Integer Plus *)
	| IMN (* Integer Minus *)
	| ITM (* Integer Times *)
	| IDV (* Integer Divide *)
	| IEQ (* Integer Equals *)
	| INQ (* Integer Not equals *)
	| IGE (* Integer Greater or equal than *)
	| IGR (* Integer Greater than *)
	| ILE (* Integer Less or equal than *)
	| ILS (* Integer Less than *)
	| IIV (* Integer Opposite *)
	| I2F (* Int->Float *)
	| FPS (* Float Plus *)
	| FMN (* Float Minus *)
	| FTM (* Float Times *)
	| FDV (* Float Divide *)
	| FEQ (* Float Equals *)
	| FNQ (* Float Not equals *)
	| FGE (* Float Greater or equal than *)
	| FGR (* Float Greater than *)
	| FLE (* Float Less or equal than *)
	| FLS (* Float Less than *)
	| FIV (* Float Opposite *)
	| BNT (* Boolean Not *)
	| BND (* Boolean And *)
	| BOR (* Boolean Or *)
	| SCT (* String Concat *)
	| STQ (* String Equals *)
	| STN (* String Not equals *)
	| ALN (* Array lenght *)
	| IPR (* Integer param *)
	| FPR (* Float param *)
	| BPR (* Boolean param *)
	| SPR (* String param *)
	| APR (* Array param *)
	| CLP (* Call procedure *)
	| CLF (* Call function *)
	| RTI (* Return integer *)
	| RTB (* Return boolean *)
	| RTF (* Return float *)
	| RTS (* Return string *)
	| RTA (* Return array *)
	| RTU (* Return unit *)
	| INI (* Init integer *)
	| INB (* Init boolean *)
	| INF (* Init float *)
	| INS (* Init string *)
	| INA (* Init array *)
	| PRI (* Print integer *)
	| PRB (* Print boolean *)
	| PRF (* Print float *)
	| PRS (* Print string *)
	| CPI (* Copy integer *)
	| CPB (* Copy boolean *)
	| CPF (* Copy float *)
	| CPS (* Copy string *)
	| CPA (* Copy array *)
	| GOT (* Goto *)
	| IFF (* If-false goto *)
	| LAB (* Label *)
	| BLC (* Block *)
	| ERR (* Error code *)
and tac4ple = { mutable tacop : tacop ; mutable arg1 : tacad option ; mutable arg2 : tacad option ; mutable res : tacad option }

(* Global Environment *)
type fnrec = { fnid : string ; fnrtype : atype ; fnpars : par list ; mutable tac : tac4ple list ; mutable usedregs : int ; mutable maxstack : int }
type prgenv = fnrec list