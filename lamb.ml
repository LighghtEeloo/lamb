open! Core

let dynamic = 
  Ast.Dynamic.by_name
  (* Ast.Dynamic.by_val *)

let exec expr =
  let rec run = function
    | Some e -> 
      Printf.printf "%s\n" @@ Ast.Expr.to_str e;
      run @@ dynamic e
    | None -> None
  in run @@ Some expr

let lexbuf = Lexing.from_channel ~with_positions:true In_channel.stdin 
let _ = 
  let cmds = Parse.main Lex.initial lexbuf in
  List.iteri cmds ~f:(
    fun i e -> 
      Printf.printf "<eval> [%d]\n" i;
      match e with
      | Top e -> match exec e with
        | Some e -> Printf.printf "%s\n" @@ Ast.Expr.to_str e
        | None -> Printf.printf "<halt>\n"
  )
