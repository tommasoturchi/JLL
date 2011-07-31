open Structs
open Option

(* Algebraic simplifications *)
let rec algsmp tac4ple =
	match tac4ple.tacop with
		| IPS ->
			begin
				match Option.get tac4ple.arg1 , Option.get tac4ple.arg2 with
					| CINT( 0 ) , _ -> { tacop = CPI ; arg1 = tac4ple.arg2 ; arg2 = None ; res = tac4ple.res }
					| _ , CINT( 0 ) -> { tacop = CPI ; arg1 = tac4ple.arg1 ; arg2 = None ; res = tac4ple.res }
					| _ , _ -> tac4ple
			end
		| IMN ->
			begin
				match Option.get tac4ple.arg1 , Option.get tac4ple.arg2 with
					| _ , CINT( 0 ) -> { tacop = CPI ; arg1 = tac4ple.arg1 ; arg2 = None ; res = tac4ple.res }
					| _ , _ -> tac4ple
			end
		| ITM ->
			begin
				match Option.get tac4ple.arg1 , Option.get tac4ple.arg2 with
					| CINT( 1 ) , _ -> { tacop = CPI ; arg1 = tac4ple.arg2 ; arg2 = None ; res = tac4ple.res }
					| _ , CINT( 1 ) -> { tacop = CPI ; arg1 = tac4ple.arg1 ; arg2 = None ; res = tac4ple.res }
					| _ , _ -> tac4ple
			end
		| IDV ->
			begin
				match Option.get tac4ple.arg1 , Option.get tac4ple.arg2 with
					| _ , CINT( 1 ) -> { tacop = CPI ; arg1 = tac4ple.arg1 ; arg2 = None ; res = tac4ple.res }
					| _ , _ -> tac4ple
			end
		| BLC ->
			begin
				match Option.get tac4ple.arg1 with
					| CBLC( tacb ) -> { tacop = BLC ; arg1 = Some( CBLC( List.map algsmp tacb ) ) ; arg2 = tac4ple.arg2 ; res = tac4ple.res }
					| _ -> tac4ple
			end
		| _ -> tac4ple

(* Redundant assigments *)
let rec rdnass tacf =
	match tacf with
		| h1::h2::t ->
			begin
				match h1.tacop , h2.tacop with
					| BLC , _ ->
						begin
							match Option.get h1.arg1 with
								| CBLC( x ) -> { tacop = BLC ; arg1 = Some( CBLC( rdnass x ) ) ; arg2 = h1.arg2 ; res = h1.res }::(rdnass (h2::t))
								| _ -> []							
						end
					| _ , BLC ->
						begin
							match Option.get h2.arg1 with
								| CBLC( x ) -> h1::{ tacop = BLC ; arg1 = Some( CBLC( rdnass x ) ) ; arg2 = h2.arg2 ; res = h2.res }::(rdnass t)
								| _ -> []
						end
					| _ , CPI
					| _ , CPB
					| _ , CPF
					| _ , CPS
					| _ , CPA when Option.is_some h1.res ->
						if (Option.get h1.res = Option.get h2.arg1) then ({ tacop = h1.tacop ; arg1 = h1.arg1 ; arg2 = h1.arg2 ; res = h2.res })::(rdnass t)
						else h1::(rdnass (h2::t))
					| _ -> h1::(rdnass (h2::t))
			end
		| h::[] ->
			begin
				match h.tacop with
					| BLC ->
						begin
							match Option.get h.arg1 with
								| CBLC( x ) -> [{ tacop = BLC ; arg1 = Some( CBLC( rdnass x ) ) ; arg2 = h.arg2 ; res = h.res }]
								| _ -> []							
						end
					| _ -> [h]
			end
		| [] -> []

(* Function's tac optimization *)
let rec opt3aF tacf =
	(* Further improvements should be done here *)
	rdnass (List.map algsmp tacf);;

(* Program's tac optimization *)
let rec opt3aP prgenv =
	(* Code optimization for each function *)
	List.map (fun x -> x.tac <- opt3aF x.tac; x) prgenv;;