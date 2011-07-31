open Structs

(* Default values *)
let jasjar = ref "../jasmin.jar"
let tacout = ref false
let tacopt = ref true
let filename = ref ""
let output = ref ""

(* Specification *)
let usage = "Usage: " ^ Sys.argv.(0) ^ " [-o] [-t] [-j jarpath] [-f filename] output"
let speclist = [
	("-o" , Arg.Clear tacopt , "Disable tree address code optimization");
	("-t" , Arg.Set tacout , " Write tree address code");
	("-j" , Arg.Set_string jasjar , " Specify an alternate jasmin's jar file");
	("-f" , Arg.Set_string filename , "Specify a file to compile (default = output.jll)");
	]
let anonfun x =
	begin
		if (not (Sys.file_exists !jasjar)) then raise (Arg.Bad ("File not found: " ^ !jasjar));
		if (!filename = "") then
			begin
				if (Sys.file_exists (x ^ ".jll")) then filename := x ^ ".jll"
				else raise (Arg.Bad ("File not found: " ^ x ^ ".jll"))
			end
		else if (not (Sys.file_exists !filename)) then raise (Arg.Bad ("File not found: " ^ !filename));
		output := x;
	end;;

(* Lexical and Syntax Analyses *)
let loadCommand _ =
	let inbuf = Lexing.from_channel (open_in !filename) in
  try Some (Parser.main Lexer.token inbuf)
  with
  | Parser.Error ->
      let p = Lexing.lexeme_end_p inbuf in
			let c = (p.Lexing.pos_cnum - p.Lexing.pos_bol) in
			begin
				Printf.fprintf stderr "Syntax error at line %d character %d \n%!" p.Lexing.pos_lnum c;
        Printf.fprintf stderr "Last read: %s \n%!" (String.sub inbuf.Lexing.lex_buffer p.Lexing.pos_bol c);
        None
      end;;

(* Semantic Analysis *)
let run c =
	Printf.printf "Program has been successfully loaded...\n";
	Printf.printf "Type-checking... ";
	let ex = Typechecker.tpchkP c in
	match ex with
		| [] ->
			begin
				try
					Printf.printf "OK\n";
					Printf.printf "Run... ";
					Tacgenerator.gen3aP c;
					(* 3AC optimization *)
					if (!tacopt) then	Typechecker.prgenv := Tacoptimizer.opt3aP !(Typechecker.prgenv);
					(* 3AC printing *)
					begin
						if (!tacout) then let oc = open_out (!filename ^ "tac") in
						List.iter (fun x -> Printf.fprintf oc "%s" (String.concat "" (("function " ^ x.fnid ^ "\n")::(List.map Tacgenerator.toString x.tac)))) !(Typechecker.prgenv);
						close_out oc;
					end;
					(* Final code optimization/generation *)
					let oc = open_out(!filename ^ "ex") in
					begin
						List.iter (Printf.fprintf oc "%s") (Fcgenerator.genFCP !(Typechecker.prgenv) output);
						close_out oc;
						begin
							match Unix.system ("java -jar " ^ !jasjar ^ " -d " ^ (Filename.dirname !filename) ^ " " ^ !filename ^ "ex &>/dev/null") with
								| Unix.WEXITED( 0 ) -> Printf.printf "OK\n";
								| _ -> Printf.printf "KO\nERROR: jasmin returned an unknown error!\n";
						end
					end
				with 
					| Sys_error fname -> Printf.printf "KO\nERROR: couldn't write to %s!\n" fname;
					| _ -> Printf.printf "KO\nERROR: something, somewhere went terribly wrong!\n";
			end
		| _ ->
			Printf.printf "KO\n";
			List.iter (fun x -> Printf.printf "ERROR: %s\n" (Printexc.to_string x)) ex;;

(* A long time ago, in a galaxy far far away... *)
let () =
	try
		Arg.parse speclist anonfun usage;
		let c = loadCommand () in
		match c	with
			| None -> ()
			| Some x ->	run x
	with _ -> Printf.printf "%s\n" usage;;