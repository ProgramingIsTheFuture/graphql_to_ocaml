(* let schema = *)
(*   let open Graphql_lwt.Schema in *)
(*   schema *)
(*     [ *)
(*       field *)
(*         "message" *)
(*         ~doc:"message" *)
(*         ~typ:(string |> non_null) *)
(*         ~args:Arg.[] *)
(*         ~resolve:(fun _ _ -> *)
(*             "Hello World" *)
(*           ); *)
(*     ];; *)

(* let () = *)
(*   Dream.run *)
(*   @@ Dream.logger *)
(*   @@ Dream.router [ *)
(*     Dream.any "/" (Dream.graphql Lwt.return schema); *)
(*     Dream.get "/graphiql" (Dream.graphiql "/") *)
(*   ];; *)

let () =
  let l = Lexing.from_string
      "type Todo {
  id: ID!
}" in

  (* assert (Parser.TYPEKW = (Lexer.tokens l)); *)
  (* assert (Parser.NAME "Todo" = (Lexer.tokens l)); *)
  (* assert (Parser.LBRACK = (Lexer.tokens l)); *)

  (* assert (Parser.NAME "id" = (Lexer.tokens l)); *)
  (* assert (Parser.DOUBLEDOT = (Lexer.tokens l)); *)
  (* assert (Parser.NAME "ID" = (Lexer.tokens l)); *)
  (* assert (Parser.EXCLAMATION "!" = (Lexer.tokens l)); *)

  (* assert (Parser.RBRACK = (Lexer.tokens l)); *)
  (* assert (Parser.EOF = (Lexer.tokens l)); *)

  let result = Parser.schema Lexer.tokens l in
  let open Ast in
  let expt = TypeDecl ("Todo", [Method ("id", [], Typ ("ID", true))] ) :: [] in
  assert (expt = result)|> ignore
