open Structs
open Option

let lstfclb = ref 0
let fnenv = ref []
let classname = ref ""

let printBool =
"PrintBool:
astore 1
ifeq PrintFalse
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc \"true\"
goto PrintBoolRes
PrintFalse:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc \"false\"
PrintBoolRes:
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
ret 1
.end method\n"

let initfc =
".super java/lang/Object
.method public <init>()V
.limit stack 1
.limit locals 1
aload_0
invokespecial java/lang/Object/<init>()V
return
.end method\n"

let rec genFCtp tp =
	match tp with
		| TINT
		| TBOOL -> "I"
		| TFLOAT -> "F"
		| TSTRING -> "Ljava/lang/String;"
		| TARR( t1 ) -> "[" ^ (genFCtp t1)
		| TUNIT -> "V"
		| _ -> ""

let inarr tp =
	match tp with
		| CTYP( TARR( TBOOL ) )
		| CTYP( TARR( TINT ) ) -> "int"
		| CTYP( TARR( TFLOAT ) ) -> "float"
		| CTYP( TARR( TSTRING ) ) -> "Ljava/lang/String;"
		| _ -> ""

let inarrtp tp =
	match tp with
		| CTYP( t ) -> t
		| _ -> TANY

let rec getVreg env id =
	try let i = List.find (fun x -> x.varid = id) env.current in i.reg
	with Not_found ->
		begin
			match env.prec with
				| None -> (-1)
				| Some( prenv ) -> getVreg prenv id
		end

let rec getArg arg env tp =
	match arg with
		| CIDN( x ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> "iload " ^ string_of_int (getVreg env x)
					| TFLOAT -> "fload " ^ string_of_int (getVreg env x)
					| TSTRING
					| TARR( _ ) -> "aload " ^ string_of_int (getVreg env x)
					| _ -> ""
			end
		| CTMP( i ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> "iload " ^ string_of_int i
					| TFLOAT -> "fload " ^ string_of_int i
					| TSTRING
					| TARR( _ ) -> "aload " ^ string_of_int i
					| _ -> ""
			end
		| CBOL( i )
		| CINT( i ) -> "ldc " ^ string_of_int i
		| CFLT( f ) -> "ldc " ^ string_of_float f
		| CSTR( s ) -> "ldc \"" ^ s ^ "\""
		| CAID( x , y ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> (getArg x env (TARR( tp ))) ^ "\n" ^ (getArg y env TINT) ^ "\niaload"
					| TFLOAT -> (getArg x env (TARR( tp ))) ^ "\n" ^ (getArg y env TINT) ^ "\nfaload"
					| TSTRING
					| TARR( _ ) -> (getArg x env (TARR( tp ))) ^ "\n" ^ (getArg y env TINT) ^ "\naaload"
					| _ -> ""
			end
		| CLAB( l ) -> "l" ^ string_of_int l
		| _ -> ""

let setRes res env tp =
	match res with
		| CIDN( x ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> "istore " ^ string_of_int (getVreg env x)
					| TFLOAT -> "fstore " ^ string_of_int (getVreg env x)
					| TSTRING
					| TARR( _ ) -> "astore " ^ string_of_int (getVreg env x)
					| _ -> ""
			end
		| CTMP( i ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> "istore " ^ string_of_int i
					| TFLOAT -> "fstore " ^ string_of_int i
					| TSTRING
					| TARR( _ ) -> "astore " ^ string_of_int i
					| _ -> ""
			end
		| CAID( x , y ) ->
			begin
				match tp with
					| TINT
					| TBOOL -> (getArg x env (TARR( tp ))) ^ "\nswap\n" ^ (getArg y env TINT) ^ "\nswap\niastore"
					| TFLOAT -> (getArg x env (TARR( tp ))) ^ "\nswap\n" ^ (getArg y env TINT) ^ "\nswap\nfastore"
					| TSTRING
					| TARR( _ ) -> (getArg x env (TARR( tp ))) ^ "\nswap\n" ^ (getArg y env TINT) ^ "\nswap\nastore"
					| _ -> ""
			end
		| _ -> ""

let getFnTps fname = let x = (List.find (fun x -> x.fnid = fname) !fnenv) in "(" ^ (List.fold_left (fun x (_,y) -> x ^ (genFCtp y)) "" x.fnpars) ^ ")" ^ (genFCtp x.fnrtype)

let getFnResTps fname =	let x = (List.find (fun x -> x.fnid = fname) !fnenv) in x.fnrtype

let rec genFCF tc4pll env = 
	match tc4pll with
		| [] -> []
		| h::t ->
			begin
				match h.tacop with
					| GOT -> ("goto " ^ (getArg (Option.get h.res) env TANY) ^ "\n")::(genFCF t env)
					| LAB -> ((getArg (Option.get h.res) env TANY) ^ ":\n")::(genFCF t env)
					| PRI -> ("getstatic java/lang/System/out Ljava/io/PrintStream;\n" ^ (getArg (Option.get h.arg1) env TINT) ^ "\ninvokevirtual java/io/PrintStream/print(I)V\n")::(genFCF t env)
					| PRB -> ((getArg (Option.get h.arg1) env TBOOL) ^ "\njsr PrintBool\n")::(genFCF t env)
					| PRF -> ("getstatic java/lang/System/out Ljava/io/PrintStream;\n" ^ (getArg (Option.get h.arg1) env TFLOAT) ^ "\ninvokevirtual java/io/PrintStream/print(F)V\n")::(genFCF t env)
					| PRS -> ("getstatic java/lang/System/out Ljava/io/PrintStream;\n" ^ (getArg (Option.get h.arg1) env TSTRING) ^ "\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n")::(genFCF t env)
					| BLC ->
						begin
							match Option.get h.arg1 , Option.get h.arg2 with
								| CBLC( bl ) , CENV( erf ) -> (genFCF bl !(erf))@(genFCF t env)
								| _ -> (genFCF t env)
						end
					| IPS -> ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\niadd\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| IMN -> ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\nisub\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| ITM -> ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\nimul\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| IDV -> ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\nidiv\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| IEQ ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmpeq fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| INQ ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmpne fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| IGE ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmpge fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| IGR ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmpgt fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| ILE ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmple fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| ILS ->
						let c = ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (getArg (Option.get h.arg2) env TINT) ^ "\n" ^
						"if_icmplt fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| IIV -> ((getArg (Option.get h.arg1) env TINT) ^ "\nineg\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| I2F -> ((getArg (Option.get h.arg1) env TINT) ^ "\ni2f\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| FPS -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\nfadd\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| FMN -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\nfsub\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| FTM -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\nfmul\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| FDV -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\nfdiv\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| FEQ ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\nifeq fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FNQ ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\nifeq fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 1\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 0\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FGE ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\nifge fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FGR ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\nifgt fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FLE ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\nifle fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FLS ->
						let c = ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (getArg (Option.get h.arg2) env TFLOAT) ^ "\n" ^
						"fcmpl\niflt fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| FIV -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\nfneg\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| BNT ->
						let c = ((getArg (Option.get h.arg1) env TBOOL) ^ "\nifeq fcl" ^
						string_of_int ((!lstfclb)+1) ^ "\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| BND -> ((getArg (Option.get h.arg1) env TBOOL) ^ "\n" ^ (getArg (Option.get h.arg2) env TBOOL) ^ "\niand\n" ^ (setRes (Option.get h.res) env TBOOL) ^ "\n")::(genFCF t env)
					| BOR -> ((getArg (Option.get h.arg1) env TBOOL) ^ "\n" ^ (getArg (Option.get h.arg2) env TBOOL) ^ "\nor\n" ^ (setRes (Option.get h.res) env TBOOL) ^ "\n")::(genFCF t env)
					| SCT -> ((getArg (Option.get h.arg1) env TSTRING) ^ "\n" ^ (getArg (Option.get h.arg2) env TSTRING) ^ "\ninvokevirtual java/lang/String.concat(Ljava/lang/String;)Ljava/lang/String;\n" ^ (setRes (Option.get h.res) env TSTRING) ^ "\n")::(genFCF t env)
					| STQ ->
						let c = ((getArg (Option.get h.arg1) env TSTRING) ^ "\n" ^ (getArg (Option.get h.arg2) env TSTRING) ^ "\n" ^
						"if_acmpeq fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| STN ->
						let c = ((getArg (Option.get h.arg1) env TSTRING) ^ "\n" ^ (getArg (Option.get h.arg2) env TSTRING) ^ "\n" ^
						"if_acmpne fcl" ^ string_of_int ((!lstfclb)+1) ^
						"\nldc 0\ngoto fcl" ^ string_of_int !lstfclb ^
						"\nfcl" ^ string_of_int ((!lstfclb)+1) ^ ":\nldc 1\nfcl" ^ string_of_int !lstfclb ^ ":\n" ^
						(setRes (Option.get h.res) env TBOOL) ^ "\n") in
						begin
							lstfclb := (!lstfclb)+2;
							let t = genFCF t env in c::t
						end
					| ALN -> ((getArg (Option.get h.arg1) env (TARR(TANY))) ^ "\narraylength\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| IPR -> ((getArg (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| FPR -> ((getArg (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| BPR -> ((getArg (Option.get h.res) env TBOOL) ^ "\n")::(genFCF t env)
					| SPR -> ((getArg (Option.get h.res) env TSTRING) ^ "\n")::(genFCF t env)
					| APR -> ((getArg (Option.get h.res) env (TARR(TANY))) ^ "\n")::(genFCF t env)
					| CLP ->
						begin
							match Option.get h.arg1 with
								| CIDN( x ) -> ("invokestatic " ^ !classname ^ "/" ^ x ^ (getFnTps x) ^ "\n")::(genFCF t env)
								| _ -> (genFCF t env)
						end
					| CLF ->
						begin
							match Option.get h.arg1 with
								| CIDN( x ) -> ("invokestatic " ^ !classname ^ "/" ^ x ^ (getFnTps x) ^ "\n" ^ (setRes (Option.get h.res) env (getFnResTps x)) ^ "\n")::(genFCF t env)
								| _ -> (genFCF t env)
						end
					| RTI
					| RTB -> ((getArg (Option.get h.arg1) env TINT) ^ "\nireturn\n")::(genFCF t env)
					| RTF -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\nfreturn\n")::(genFCF t env)
					| RTS
					| RTA -> ((getArg (Option.get h.arg1) env TSTRING) ^ "\nareturn\n")::(genFCF t env)
					| RTU -> ("return\n")::(genFCF t env)
					| INI
					| INB -> ("ldc 0\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| INF -> ("ldc 0.0\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| INS -> ("ldc \"\"\n" ^ (setRes (Option.get h.res) env TSTRING) ^ "\n")::(genFCF t env)
					| INA -> ((getArg (Option.get h.arg1) env TINT) ^ "\nnewarray " ^ (inarr (Option.get h.arg2)) ^ "\n" ^ (setRes (Option.get h.res) env (inarrtp (Option.get h.arg2))) ^ "\n")::(genFCF t env)
					| CPI -> ((getArg (Option.get h.arg1) env TINT) ^ "\n" ^ (setRes (Option.get h.res) env TINT) ^ "\n")::(genFCF t env)
					| CPB -> ((getArg (Option.get h.arg1) env TBOOL) ^ "\n" ^ (setRes (Option.get h.res) env TBOOL) ^ "\n")::(genFCF t env)
					| CPF -> ((getArg (Option.get h.arg1) env TFLOAT) ^ "\n" ^ (setRes (Option.get h.res) env TFLOAT) ^ "\n")::(genFCF t env)
					| CPS -> ((getArg (Option.get h.arg1) env TSTRING) ^ "\n" ^ (setRes (Option.get h.res) env TSTRING) ^ "\n")::(genFCF t env)
					| CPA -> ((getArg (Option.get h.arg1) env (TARR(TANY))) ^ "\n" ^ (setRes (Option.get h.res) env (TARR(TANY))) ^ "\n")::(genFCF t env)
					| IFF -> ((getArg (Option.get h.arg1) env TBOOL) ^ "\nifeq " ^ (getArg (Option.get h.res) env TANY) ^ "\n")::(genFCF t env)
					| ERR -> (genFCF t env)
			end

(* Program's final code generation *)
let genFCP prgenv output =
	let parcnt = new Typechecker.cnt in
	fnenv := prgenv;
	classname := !output;
	(".class public " ^ !output ^ "\n" ^ initfc)::
	(List.flatten (List.map
	(fun x -> (".method public static " ^ x.fnid ^ (* Function name *)
	(getFnTps x.fnid) ^ "\n" ^ ".limit stack "^ string_of_int (x.maxstack + 3) ^ "\n" ^ ".limit locals " ^ string_of_int x.usedregs ^ "\n"):: (* Return type and local/stack settings *)
	(genFCF x.tac { current = (List.map (fun (x,y)->{ varid = x ; vartype = y ; reg = parcnt#get }) x.fnpars) ; prec = None })@[printBool]) prgenv))