open Structs

exception TypeError
exception MissingReturn of string
exception AlreadyDeclared of string
exception SymbolUnknown of string
exception WrongParameter of string
exception WrongMainSyntax

(* Global function's symbol table *)
let prgenv = ref []

(* Registers allocation *)
class cnt =
	object
		val mutable x = (-1)
		method get = x<-x+1; x
	end

(* Typechecks expressions *)

(* Combining numerical expressions *)
let numTp t1 t2 =
	match t1,t2 with
		| TINT,TINT -> TINT
		| TFLOAT,TFLOAT
		| TFLOAT,TINT
		| TINT,TFLOAT -> TFLOAT
		| _,_ -> raise (Invalid_argument("Type error!!!"))

(* Combining boolean expressions *)
let boolTp t1 t2 =
	match t1,t2 with
		| TBOOL,TBOOL -> TBOOL
		| _,_ -> raise (Invalid_argument("Type error!!!"))

(* Comparing expressions *)
let relTp t1 t2 =
	match t1,t2 with
		| TSTRING,TSTRING
		| TINT,TINT
		| TFLOAT,TFLOAT
		| TFLOAT,TINT
		| TINT,TFLOAT -> TBOOL
		| _,_ -> raise (Invalid_argument("Type error!!!"))

(* Combining strings *)
let strTp t1 t2 =
	match t1,t2 with
		| TSTRING,TSTRING -> TSTRING
		| _,_ -> raise (Invalid_argument("Type error!!!"))

(* Binary operators resulting types *)
let bopTp op t1 t2 =
	match op with
	| PLUS
	| MINUS
	| TIMES
	| DIVIDE -> numTp t1 t2
	| AND
	| OR -> boolTp t1 t2
	| LOETHAN
	| LTHAN
	| EQ
	| GOETHAN
	| GTHAN
	| NEQ -> relTp t1 t2
	| CONCAT -> strTp t1 t2

(* Unary operators resulting types *)
let uopTp op t =
	match op,t with
	| INV,TINT
	| INV,TFLOAT
	| NOT,TBOOL -> t
	| ARRLNG,TARR( _ ) -> TINT
	| ITF,TINT -> TFLOAT
	| _ -> raise TypeError

(* Check subtypes *)
let rec isSubTp t1 t2 =
	match t1,t2 with
		| _,TANY -> true
		| TINT,TFLOAT -> true
		| _,_ -> (t1=t2)

(* Get type for variables *)
let rec varTp env id =
	try let v = List.find (fun x -> x.varid = id) env.current in v.vartype
	with Not_found ->
		begin
			match env.prec with
				| None -> raise (SymbolUnknown id)
				| Some( prenv ) -> varTp prenv id
		end

(* Get type for functions *)
let fnTp id =
	try let v = List.find (fun x -> x.fnid = id) !prgenv in v.fnpars , v.fnrtype
	with Not_found -> raise (SymbolUnknown id)

(* Typechecks an expression *)
let rec exprchkTp env expr =
	match expr with
		| INT( _ , itstype )
		| BOOL( _ , itstype )
		| FLOAT( _ , itstype )
		| STRING( _ , itstype ) -> !itstype
		| BOP( o , e1 , e2 , itstype ) ->
			itstype := bopTp o (exprchkTp env e1) (exprchkTp env e2);
			!itstype
		| UOP( o , e , itstype ) ->
			itstype := uopTp o (exprchkTp env e);
			!itstype
		| ID( x , itstype ) ->
			begin
				try let _ = fnTp x in raise TypeError
				with
					| (SymbolUnknown s) -> itstype := varTp env x; !itstype
			end
		| ARRELM( x , pos , itstype ) ->
			begin
				try let _ = fnTp x in raise TypeError
				with
					| (SymbolUnknown s) ->
						begin
							match varTp env x , exprchkTp env pos with
								| TARR( t ) , TINT -> itstype := t; !itstype
								| _ -> raise TypeError
						end
			end
		| CALL( x , exlst , itstype ) ->
			let pars,t = fnTp x in
			begin
				try if (List.for_all2 (isSubTp) (List.map (exprchkTp env) exlst) (List.map (fun (_,t) -> t) pars)) && t!=TUNIT then begin itstype := t; !itstype end else raise TypeError
				with
					| Invalid_argument(_) -> raise (WrongParameter x)
			end
		| ARR( exlst , itstype ) ->
			begin
				try let th = exprchkTp env (List.hd exlst) in if (List.for_all (fun x -> th=(exprchkTp env x)) exlst) then begin itstype := TARR( th ); !itstype end else raise TypeError
				with
					| _ -> raise TypeError
			end

(* Typechecks declarations *)
let rec tpchkD decl env lreg =
	match decl with
		| [] -> [] , lreg
		| h::t ->
			let ( x , tp , e ) = h in
			begin
				try let _ = fnTp x in let l , r = (tpchkD t env (lreg+1)) in [AlreadyDeclared(x)]@l , r
				with
					| (SymbolUnknown s) ->
						begin
							try let _ = varTp env s in let l , r = (tpchkD t env (lreg+1)) in [AlreadyDeclared(s)]@l , r
							with
								| (SymbolUnknown s) ->
									begin
										if (Option.is_some(e)) then
											match exprchkTp env (Option.get e) with
												| TINT -> env.current <- env.current@[{ varid = s ; vartype = tp ; reg = (lreg+1) }]; tpchkD t env (lreg+1)
												| _ -> let l , r = (tpchkD t env (lreg+1)) in [TypeError]@l , r
										else
											begin
												env.current <- env.current@[{ varid = s ; vartype = tp ; reg = (lreg+1) }];
												tpchkD t env (lreg+1)
											end
									end
						end
			end

(* Typechecks a command *)
and tpchkS cmd ftype env lreg =
	match cmd with
		| IFELSE( expr , st1 , st2 ) ->
			begin
				let ex1 , rt1 = tpchkS st1 ftype env lreg and ex2 , rt2 = tpchkS st2 ftype env lreg in
					try let tp = exprchkTp env expr in
						match tp with 
							| TBOOL -> ex1@ex2 , rt1&&rt2
							| _ -> [TypeError]@ex1@ex2 , rt1&&rt2
				with e -> [e]@ex1@ex2 , rt1&&rt2
			end
		| IF( expr , st )
		| WHILE( expr , st ) ->
			begin
				let exs , rts = tpchkS st ftype env lreg in
					try let tp = exprchkTp env expr in
						match tp with
							| TBOOL -> exs , rts
							| _ -> [TypeError]@exs , rts
				with e -> [e]@exs , rts
			end
		| SET( x , expr ) ->
			begin
				try let _ = fnTp x in [TypeError] , false
				with
					| (SymbolUnknown s) ->
						begin try let tx = varTp env x and te = exprchkTp env expr in if isSubTp te tx then [] , false else [TypeError] , false
						with e -> [e] , false
						end
			end
		| SETARR( x , d , expr ) ->
			begin
				try let _ = fnTp x in [TypeError] , false
				with
					| (SymbolUnknown s) ->
						begin 
							try let tx = varTp env x and te = exprchkTp env expr in
							match tx , exprchkTp env d with
								| TARR( ta ) , TINT -> if isSubTp te ta then [] , false else [TypeError] , false
								| _ -> [TypeError] , false
							with e -> [e] , false
						end
			end
		| INCR( x )
		| DECR( x ) ->
			begin
				try let _ = fnTp x in [TypeError] , false
				with
					| (SymbolUnknown s) ->
						begin try let tx = varTp env x in if TINT=tx then [] , false else [TypeError] , false
						with e -> [e] , false
						end
			end
		| INCRA( x , ex )
		| DECRA( x , ex ) ->
			begin
				try let _ = fnTp x in [TypeError] , false
				with
					| (SymbolUnknown s) ->
						begin
							match varTp env x , exprchkTp env ex with
								| TARR( TINT ) , TINT -> [] , false
								| _ -> [TypeError] , false
						end
			end
		| PROC( x , exlst ) ->
			begin
				try let pars,t = fnTp x in
					begin
						try if (List.for_all2 (isSubTp) (List.map (exprchkTp env) exlst) (List.map (fun (_,t) -> t) pars)) && (t=TUNIT) then [] , false else [TypeError] , false
						with Invalid_argument(_) -> [WrongParameter x] , false
					end
				with e -> [e] , false
			end
		| PRINT( expr ) ->
			begin
				try let te = exprchkTp env expr in
				match te with
					| TARR( _ ) -> [TypeError] , false
					| _ -> [] , false
				with e -> [e] , false
			end
		| BLOCK( bl , blenv ) ->
			begin
				blenv.prec <- Some( env );
				let exl , nreg = tpchkD bl.decl blenv lreg in
					match bl.cmds with
						| None -> exl , false
						| Some ( ss ) -> let exb , rtb = tpchkSS ss ftype blenv nreg in exl@exb , rtb
			end
		| RETURN( expr ) ->
			begin
				match expr , ftype with
					| None , TUNIT -> [] , true
					| None , _ -> [TypeError] , true
					| Some( rex ) , _ ->
						begin
							try let tp = exprchkTp env rex in if (tp=ftype) then [] , true else [TypeError] , true
							with e -> [e] , true
						end
			end

(* Typechecks commands *)
and tpchkSS cmds ftype env lreg =
	match cmds with
		| CMD( s ) -> tpchkS s ftype env lreg
		| SEQ( s1 , s2 ) -> let ex1 , rt1 = tpchkSS s1 ftype env lreg and ex2 , rt2 = tpchkS s2 ftype env lreg in ex1@ex2 , rt1||rt2

(* Typechecks programs *)
let rec tpchkP prg =
	match prg with
		| [] -> []
		| h::t ->
			let (fname,rtype,pars,cmd) = h in
			try let _ = fnTp fname in [AlreadyDeclared(fname)]@(tpchkP t)
			with (SymbolUnknown s) ->
				let chkdefmnsig , tmpcnt =
					(if (s="main") &&
					((rtype!=TUNIT) || (* main's type has to be "unit" *)
					(List.length pars<>1) || (* main's parameters have to be exactly 1 *)
					(((fun (x,y) -> y) (List.hd pars))<>TARR(TSTRING))) (* main's parameter has to be args array *)
					then [WrongMainSyntax]
					else []) , new cnt
				in
				begin
					prgenv := !(prgenv)@[{ fnid = fname ; fnrtype = rtype ; fnpars = pars ; tac = [] ; usedregs = if (List.length pars)>0 then List.length pars else 1 ; maxstack = 0 }];
					let ex , rt = tpchkS cmd rtype { current = (List.map (fun (x,y)->{ varid = x ; vartype = y ; reg = tmpcnt#get }) pars) ; prec = None } ((List.length pars)-1) in (fun (x,y) -> if x then y else (MissingReturn fname)::y) ( rt , chkdefmnsig@ex@(tpchkP t) )
				end

(* Returns an expression's type *)
let rec exprTp expr =
	match expr with
		| INT( _ , itstype )
		| BOOL( _ , itstype )
		| FLOAT( _ , itstype )
		| STRING( _ , itstype )
		| BOP( _ , _ , _ , itstype )
		| UOP( _ , _ , itstype )
		| ID( _ , itstype )
		| ARRELM( _ , _ , itstype )
		| CALL( _ , _ , itstype )
		| ARR( _ , itstype ) -> !itstype