open Structs

class cnt :
	object
		val mutable x : int
		method get : int
	end

(* Returns the global symbol table *)
val prgenv: prgenv ref

(* Typechecks a program *)
val tpchkP: prg -> exn list

(* Returns an expression's type *)
val exprTp: expr -> atype

(* Returns a variable's type *)
val varTp: blenv -> string -> atype