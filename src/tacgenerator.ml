open Structs
open Option

(* Initial label and register values *)
let lstlb = ref (-1)
let lstrg = ref (-1)
let maxstck = ref 0

let uopf2tac op =
	match op with
		| NOT -> BNT
		| INV -> FIV 
		| _ -> ERR
let bopf2tac op =
	match op with
		| PLUS -> FPS
		| MINUS -> FMN
		| TIMES -> FTM
		| DIVIDE -> FDV
		| EQ -> FEQ
		| NEQ -> FNQ
		| GOETHAN -> FGE
		| GTHAN -> FGR
		| LOETHAN -> FLE
		| LTHAN -> FLS
		| _ -> ERR
let uop2tac op =
	match op with
		| NOT -> BNT
		| INV -> IIV
		| ARRLNG -> ALN
		| ITF -> I2F
let bop2tac op =
	match op with
		| PLUS -> IPS
		| MINUS -> IMN
		| TIMES -> ITM
		| DIVIDE -> IDV
		| EQ -> IEQ
		| NEQ -> INQ
		| GOETHAN -> IGE
		| GTHAN -> IGR
		| LOETHAN -> ILE
		| LTHAN -> ILS
		| AND -> BND
		| OR -> BOR
		| _ -> ERR
let bops2tac op =
	match op with
		| EQ -> STQ
		| NEQ -> STN
		| CONCAT -> SCT
		| _ -> ERR
let par2tac tp =
	match tp with
		| TINT -> IPR
		| TFLOAT -> FPR
		| TBOOL -> BPR
		| TSTRING -> SPR
		| TARR( _ ) -> APR
		| _ -> ERR
let ret2tac tp =
	match tp with
		| TINT -> RTI
		| TFLOAT -> RTF
		| TBOOL -> RTB
		| TSTRING -> RTS
		| _ -> ERR
let arrcp2tac tp =
	match tp with
		| TARR( TINT ) -> CPI
		| TARR( TFLOAT ) -> CPF
		| TARR( TBOOL ) -> CPB
		| TARR( TSTRING ) -> CPS
		| _ -> ERR

(* Expression's 3AC *)
let rec gen3aEx expr =
	match expr with
		| INT( x , _ ) -> CINT( x ) , []
		| BOOL( x , _ ) -> CBOL( if x then 1 else 0 ) , []
		| FLOAT( x , _ ) -> CFLT( x ) , []
		| STRING( x , _ ) -> CSTR( x ) , []
		| BOP( o , e1 , e2 , itstype ) ->
			let (re1 , tace1) , te1 = gen3aEx e1 , Typechecker.exprTp e1 in
			let (re2 , tace2) , te2 = gen3aEx e2 , Typechecker.exprTp e2 in
			begin
				match !(itstype) with
					| TFLOAT when te1=TFLOAT && te2=TINT ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace1@ (* e1's 3AC *)
						tace2@ (* e2's 3AC *)
						[ { tacop=I2F ; arg1=Some(re2) ; arg2=None ; res=Some(CTMP(!(lstrg))) } ;
						{ tacop=bopf2tac o ; arg1=Some(CTMP(!(lstrg))) ; arg2=Some(re1) ; res=Some(CTMP(!(lstrg))) } ]
					| TFLOAT when te1=TINT && te2=TFLOAT ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace1@ (* e1's 3AC *)
						tace2@ (* e2's 3AC *)
						[ { tacop=I2F ; arg1=Some(re1) ; arg2=None ; res=Some(CTMP(!(lstrg))) } ;
						{ tacop=bopf2tac o ; arg1=Some(CTMP(!(lstrg))) ; arg2=Some(re2) ; res=Some(CTMP(!(lstrg))) } ]
					| _ when te1=TFLOAT && te1=TFLOAT ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace1@ (* e1's 3AC *)
						tace2@ (* e2's 3AC *)
						[ { tacop=bopf2tac o ; arg1=Some(re1) ; arg2=Some(re2) ; res=Some(CTMP(!(lstrg))) } ]
					| _ when te1=TSTRING && te2=TSTRING ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace1@ (* e1's 3AC *)
						tace2@ (* e2's 3AC *)
						[ { tacop=bops2tac o ; arg1=Some(re1) ; arg2=Some(re2) ; res=Some(CTMP(!(lstrg))) } ]
					| _ ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace1@ (* e1's 3AC *)
						tace2@ (* e2's 3AC *)
						[ { tacop=bop2tac o ; arg1=Some(re1) ; arg2=Some(re2) ; res=Some(CTMP(!(lstrg))) } ]
			end
		| UOP( o , expr , _ ) ->
			let (re , tace) , te = gen3aEx expr , Typechecker.exprTp expr in
			begin
				match te with
					| TFLOAT ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace@ (* expr's 3AC *)
						[ { tacop=uopf2tac o ; arg1=Some(re) ; arg2=None ; res=Some(CTMP(!(lstrg))) } ]
					| _ ->
						lstrg := !(lstrg)+1;
						CTMP( !(lstrg) ),
						tace@ (* expr's 3AC *)
						[ { tacop=uop2tac o ; arg1=Some(re) ; arg2=None ; res=Some(CTMP(!(lstrg))) } ]
			end
		| ID( x , _ ) -> CIDN( x ) , []
		| ARRELM( x , pos , _ ) -> let rpos , tacpos = gen3aEx pos in CAID( CIDN( x ) , rpos ) , tacpos
		| CALL( x , exlst , _ ) ->
			let (rprs , tacprs) , tprs = List.split (List.map gen3aEx exlst) , List.map Typechecker.exprTp exlst in
			begin
				if (!maxstck < List.length exlst) then maxstck := List.length exlst;
				lstrg := !(lstrg)+1;
				CTMP( !(lstrg) ),
				(List.flatten tacprs)@ (* 3AC for each parameter *)
				(List.map2 (fun x y -> { tacop=par2tac y ; arg1=None ; arg2=None ; res=Some(x) } ) rprs tprs)@ (* 3AC for each param line *)
				[ { tacop=CLF ; arg1=Some(CIDN(x)) ; arg2=None ; res=Some(CTMP(!(lstrg))) } ]
			end
		| ARR( exlst , itstype ) ->
			let (rarel , tacarel) , tmcnt = List.split (List.map gen3aEx exlst) , new Typechecker.cnt in
			begin
				if (!maxstck < List.length exlst) then maxstck := List.length exlst;
				lstrg := !(lstrg)+1;
				CTMP( !(lstrg) ),
				(List.flatten tacarel)@ (* 3AC for each array element *)
				[{ tacop=INA ; arg1=Some(CINT(List.length exlst)) ; arg2=Some(CTYP(!itstype)) ; res=Some(CTMP( !(lstrg) )) }]@
				(List.map (fun x -> { tacop=arrcp2tac !(itstype) ; arg1=Some(x) ; arg2=None ; res=Some(CAID( CTMP( !(lstrg) ) , CINT(tmcnt#get) )) } ) rarel) (* 3AC for array population *)
			end
			

(* Command's 3AC *)
let rec gen3aS cmd env =
	match cmd with
		| IFELSE( expr , st1 , st2 ) ->
			let (re , tace) , truelb = gen3aEx expr , !(lstlb)+1 in
			begin
				lstlb := !(lstlb)+2;
				tace@ (* expr's 3AC *)
				[ { tacop=IFF ; arg1=Some(re) ; arg2=None ; res=Some(CLAB(truelb)) } ]@ (* Jump if false *)
				(gen3aS st1 env)@ (* true branch *)
				[ { tacop=GOT ; arg1=None ; arg2=None ; res=Some(CLAB(truelb+1)) } ]@ (* Goto if's end *)
				[ { tacop=LAB ; arg1=None ; arg2=None ; res=Some(CLAB(truelb)) } ]@ (* False's label *)
				(gen3aS st2 env)@ (* false branch *)
				[ { tacop=LAB ; arg1=None ; arg2=None ; res=Some(CLAB(truelb+1)) } ] (* If's end *)
			end
		| IF( expr , st ) ->
			let (re , tace) , truelb = gen3aEx expr , !(lstlb)+1 in
			begin
				lstlb := !(lstlb)+1;
				tace@ (* expr's 3AC *)
				[ { tacop=IFF ; arg1=Some(re) ; arg2=None ; res=Some(CLAB(truelb)) } ]@ (* Jump if false *)
				(gen3aS st env)@ (* true branch *)
				[ { tacop=LAB ; arg1=None ; arg2=None ; res=Some(CLAB(truelb)) } ] (* If's end *)
			end
		| WHILE( expr , st ) ->
			let (re , tace) , initlb = gen3aEx expr , !(lstlb)+1 in
			begin
				lstlb := !(lstlb)+2;
				{ tacop=LAB ; arg1=None ; arg2=None ; res=Some(CLAB(initlb)) }:: (* Initial label *)
				tace@ (* expr's 3AC *)
				[ { tacop=IFF ; arg1=Some(re) ; arg2=None ; res=Some(CLAB(initlb+1)) } ]@ (* Jump if false *)
				(gen3aS st env)@ (* cmd's 3AC *)
				[ { tacop=GOT ; arg1=None ; arg2=None ; res=Some(CLAB(initlb)) } ]@ (* Goto while's beginning *)
				[ { tacop=LAB ; arg1=None ; arg2=None ; res=Some(CLAB(initlb+1)) } ] (* While's end *)
			end
		| SET( x , expr ) ->
			let (re , tace) , te = gen3aEx expr , Typechecker.exprTp expr in
			begin
				match te with
					| TINT -> tace@[ { tacop=CPI ; arg1=Some(re) ; arg2=None ; res=Some(CIDN(x)) } ]
					| TBOOL -> tace@[ { tacop=CPB ; arg1=Some(re) ; arg2=None ; res=Some(CIDN(x)) } ]
					| TFLOAT -> tace@[ { tacop=CPF ; arg1=Some(re) ; arg2=None ; res=Some(CIDN(x)) } ]
					| TSTRING -> tace@[ { tacop=CPS ; arg1=Some(re) ; arg2=None ; res=Some(CIDN(x)) } ]
					| TARR( _ ) -> tace@[ { tacop=CPA ; arg1=Some(re) ; arg2=None ; res=Some(CIDN(x)) } ]
					| _ -> tace@[ { tacop=ERR ; arg1=None ; arg2=None ; res=None } ]
			end
		| SETARR( x , d , expr ) ->
			let rpos , tacpos = gen3aEx d in
			let (re , tace) , te = gen3aEx expr , Typechecker.exprTp expr in
			begin
				match te with
					| TINT -> tacpos@tace@[ { tacop=CPI ; arg1=Some(re) ; arg2=None ; res=Some(CAID( CIDN( x ) , rpos )) } ]
					| TBOOL -> tacpos@tace@[ { tacop=CPB ; arg1=Some(re) ; arg2=None ; res=Some(CAID( CIDN( x ) , rpos )) } ]
					| TFLOAT -> tacpos@tace@[ { tacop=CPF ; arg1=Some(re) ; arg2=None ; res=Some(CAID( CIDN( x ) , rpos )) } ]
					| TSTRING -> tacpos@tace@[ { tacop=CPS ; arg1=Some(re) ; arg2=None ; res=Some(CAID( CIDN( x ) , rpos )) } ]
					| _ -> tacpos@tace@[ { tacop=ERR ; arg1=None ; arg2=None ; res=None } ]
			end
		| INCR( x ) -> [ { tacop=IPS ; arg1=Some(CIDN( x )) ; arg2=Some(CINT(1)) ; res=Some(CIDN( x )) } ]
		| DECR( x ) -> [ { tacop=IMN ; arg1=Some(CIDN( x )) ; arg2=Some(CINT(1)) ; res=Some(CIDN( x )) } ]
		| INCRA( x , ex ) -> let re , tace = gen3aEx ex in tace@[ { tacop=IPS ; arg1=Some(CAID( CIDN( x ) , re )) ; arg2=Some(CINT(1)) ; res=Some(CAID( CIDN( x ) , re )) } ]
		| DECRA( x , ex ) -> let re , tace = gen3aEx ex in tace@[ { tacop=IMN ; arg1=Some(CAID( CIDN( x ) , re )) ; arg2=Some(CINT(1)) ; res=Some(CAID( CIDN( x ) , re )) } ]
		| PROC( x , exlst ) ->
			let (rprs , tacprs) , tprs = List.split (List.map gen3aEx exlst) , List.map Typechecker.exprTp exlst in
			begin
				if (!maxstck < List.length exlst) then maxstck := List.length exlst;
				(List.flatten tacprs)@ (* 3AC for each parameter *)
				(List.map2 (fun x y -> { tacop=par2tac y ; arg1=None ; arg2=None ; res=Some(x) } ) rprs tprs)@ (* 3AC for each param line *)
				[ { tacop=CLP ; arg1=Some(CIDN(x)) ; arg2=None ; res=None } ]
			end
		| PRINT( expr ) ->
			let (re , tace) , te = gen3aEx expr , Typechecker.exprTp expr in
			begin
				match te with
					| TINT -> tace@[ { tacop=PRI ; arg1=Some(re) ; arg2=None ; res=None } ]
					| TBOOL -> tace@[ { tacop=PRB ; arg1=Some(re) ; arg2=None ; res=None } ]
					| TFLOAT -> tace@[ { tacop=PRF ; arg1=Some(re) ; arg2=None ; res=None } ]
					| TSTRING -> tace@[ { tacop=PRS ; arg1=Some(re) ; arg2=None ; res=None } ]
					| _ -> tace@[ { tacop=ERR ; arg1=None ; arg2=None ; res=None } ]
			end
		| BLOCK( bl , e ) ->
			begin
				let tacd = gen3aD bl.decl in
					match bl.cmds with
						| None -> [ { tacop=BLC ; arg1=Some(CBLC(tacd)) ; arg2=Some(CENV(ref e)) ; res=None } ]
						| Some ( ss ) -> [ { tacop=BLC ; arg1=Some(CBLC(tacd@(gen3aSS ss e))) ; arg2=Some(CENV(ref e)) ; res=None } ]
			end
		| RETURN( expr ) ->
			begin
				try let (re , tace) , te = gen3aEx (Option.get expr) , Typechecker.exprTp (Option.get expr) in
					begin
						match te with
							| TINT -> tace@[ { tacop=RTI ; arg1=Some(re) ; arg2=None ; res=None } ]
							| TBOOL -> tace@[ { tacop=RTB ; arg1=Some(re) ; arg2=None ; res=None } ]
							| TFLOAT -> tace@[ { tacop=RTF ; arg1=Some(re) ; arg2=None ; res=None } ]
							| TSTRING -> tace@[ { tacop=RTS ; arg1=Some(re) ; arg2=None ; res=None } ]
							| TARR( _ ) -> tace@[ { tacop=RTA ; arg1=Some(re) ; arg2=None ; res=None } ]
							| _ -> tace@[ { tacop=ERR ; arg1=None ; arg2=None ; res=None } ]
					end
				with No_value -> [ { tacop=RTU ; arg1=None ; arg2=None ; res=None } ]
			end

(* Commands' 3AC *)
and gen3aSS cmds env =
	match cmds with
		| CMD( s ) -> gen3aS s env
		| SEQ( s1 , s2 ) -> (gen3aSS s1 env)@(gen3aS s2 env)

(* Declarations' 3AC *)
and gen3aD decl =
	match decl with
		| [] -> []
		| h::t ->
			let ( x , tp , e ) = h in
			begin
				match tp with
					| TINT -> lstrg := !(lstrg)+1; { tacop=INI ; arg1=None ; arg2=None ; res=Some(CIDN(x)) }::(gen3aD t)
					| TBOOL -> lstrg := !(lstrg)+1; { tacop=INB ; arg1=None ; arg2=None ; res=Some(CIDN(x)) }::(gen3aD t)
					| TFLOAT -> lstrg := !(lstrg)+1; { tacop=INF ; arg1=None ; arg2=None ; res=Some(CIDN(x)) }::(gen3aD t)
					| TSTRING -> lstrg := !(lstrg)+1; { tacop=INS ; arg1=None ; arg2=None ; res=Some(CIDN(x)) }::(gen3aD t)
					| TARR( _ ) -> let re , tace = gen3aEx (Option.get e) in lstrg := !(lstrg)+1; tace@({ tacop=INA ; arg1=Some(re) ; arg2=Some(CTYP(tp)) ; res=Some(CIDN(x)) }::(gen3aD t))
					| _ -> { tacop=ERR ; arg1=None ; arg2=None ; res=Some(CIDN(x)) }::(gen3aD t)
			end

(* Program's 3AC *)
let rec gen3aP prg =
	match prg with
		| [] -> ()
		| h::t ->
			let (fname,rtype,pars,cmd) = h in
			let frec , parcnt = List.find (fun s -> (s.fnid=fname)) !(Typechecker.prgenv) , new Typechecker.cnt in
			begin
				lstlb := (-1);
				lstrg := (List.length pars)-1;
				frec.tac <- gen3aS cmd { current = (List.map (fun (x,y)->{ varid = x ; vartype = y ; reg = parcnt#get }) pars) ; prec = None }; (* cmd's 3AC *)
				frec.usedregs <- frec.usedregs+(!lstrg)+2; (* Used registers := parameters + (registers + 2 (PrintBool needed)) *)
				frec.maxstack <- (!maxstck)+3;
				maxstck := 0;
				gen3aP t; (* Recursive call *)
			end

(* 3AC to string *)
let optoString op =
	match op with
		| IPS -> "ips"
		| IMN -> "imn"
		| ITM -> "itm"
		| IDV -> "idv"
		| IEQ -> "ieq"
		| INQ -> "inq"
		| IGE -> "ige"
		| IGR -> "igr"
		| ILE -> "ile"
		| ILS -> "ils"
		| IIV -> "iiv"
		| I2F -> "i2f"
		| FPS -> "fps"
		| FMN -> "fmn"
		| FTM -> "ftm"
		| FDV -> "fdv"
		| FEQ -> "feq"
		| FNQ -> "fnq"
		| FGE -> "fge"
		| FGR -> "fgr"
		| FLE -> "fle"
		| FLS -> "fls"
		| FIV -> "fiv"
		| BNT -> "bnt"
		| BND -> "bnd"
		| BOR -> "bor"
		| SCT -> "sct"
		| STQ -> "stq"
		| STN -> "stn"
		| ALN -> "aln"
		| IPR -> "ipr"
		| FPR -> "fpr"
		| BPR -> "bpr"
		| SPR -> "spr"
		| APR -> "apr"
		| CLP -> "clp"
		| CLF -> "clf"
		| RTI -> "rti"
		| RTB -> "rtb"
		| RTF -> "rtf"
		| RTS -> "rts"
		| RTA -> "rta"
		| RTU -> "rtu"
		| INI -> "ini"
		| INB -> "inb"
		| INF -> "inf"
		| INS -> "ins"
		| INA -> "ina"
		| PRI -> "pri"
		| PRB -> "prb"
		| PRF -> "prf"
		| PRS -> "prs"
		| CPI -> "cpi"
		| CPB -> "cpb"
		| CPF -> "cpf"
		| CPS -> "cps"
		| CPA -> "cpa"
		| GOT -> "got"
		| IFF -> "ifg"
		| LAB -> "lab"
		| BLC -> "blc"
		| ERR -> "err"
let rec adtoString arg =
	match Option.get arg with
		| CLAB( i ) -> "L" ^ string_of_int i
		| CTMP( i ) -> "tmp" ^ string_of_int i
		| CINT( i )
		| CBOL( i ) -> string_of_int i
		| CFLT( f ) -> string_of_float f
		| CIDN( x )
		| CSTR( x ) -> x
		| CAID( t1 , t2 ) -> adtoString (Some(t1)) ^ "[" ^ adtoString (Some(t2)) ^ "]"
		| CBLC( tl ) -> List.fold_left (fun x y -> x ^ (toString y) ) "" tl
		| CENV( e ) -> "env"
		| CTYP( t ) ->
			begin
				match t with
					| TINT -> "int"
					| TBOOL -> "bool"
					| TFLOAT -> "float"
					| TSTRING -> "string"
					| TARR( tar ) -> "arr[" ^ (adtoString (Some(CTYP(tar)))) ^ "]"
					| _ -> ""
			end
and toString tacl =
	match tacl.tacop with
		| BLC -> adtoString tacl.arg1
		| LAB -> (adtoString tacl.res) ^ ": "
		| _ ->
			(optoString tacl.tacop) ^
			(if Option.is_some(tacl.arg1) then " " ^ (adtoString tacl.arg1) else "") ^
			(if Option.is_some(tacl.arg2) then " " ^ (adtoString tacl.arg2) else "") ^
			(if Option.is_some(tacl.res) then " " ^ (adtoString tacl.res) else "") ^
			"\n"