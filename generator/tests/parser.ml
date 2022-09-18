open Generator

let type_schema_id_field_test () =
  (* assert (Parser.TYPEKW = (Lexer.tokens l)); *)
  (* assert (Parser.NAME "Todo" = (Lexer.tokens l)); *)
  (* assert (Parser.LBRACK = (Lexer.tokens l)); *)

  (* assert (Parser.NAME "id" = (Lexer.tokens l)); *)
  (* assert (Parser.DOUBLEDOT = (Lexer.tokens l)); *)
  (* assert (Parser.NAME "ID" = (Lexer.tokens l)); *)
  (* assert (Parser.EXCLAMATION "!" = (Lexer.tokens l)); *)

  (* assert (Parser.RBRACK = (Lexer.tokens l)); *)
  (* assert (Parser.EOF = (Lexer.tokens l)); *)

  let l = Lexing.from_string
      "type Todo {
  id: ID!
}" in

  let result:Ast.schema = Parser.schema Lexer.tokens l in
  let expt: Ast.schema = Ast.TypeDecl ("Todo", [Ast.Method ("id", [], Ast.Typ ("int"))] ) :: [] in

  assert (expt = result)|> ignore;
  Format.printf "\x1b[32mPassed type_schema_id_field_test!\n\x1b[0m@?";;

let type_schema_multiple_fields_test () =
  let l = Lexing.from_string
      "type Todo {
  id: ID!
      username: String
      password: String
}" in

  let open Ast in
  let result:schema = Parser.schema Lexer.tokens l in
  let expt: schema = TypeDecl (
      "Todo",
      [Method ("id", [], Typ ("int"));
       Method ("username", [], Typ ("string option"));
       Method ("password", [], Typ ("string option"))
      ]) :: [] in

  assert (expt = result)|> ignore;
  Format.printf "\x1b[32mPassed type_schema_multiple_fields_test!\n\x1b[0m@?";;

let type_schema_lists_test () =
  let l = Lexing.from_string
      "type Todo {
      id: ID!
      body: [String]
}" in

  let result:Ast.schema = Parser.schema Lexer.tokens l in
  let expt: Ast.schema = Ast.TypeDecl ("Todo", [
      Ast.Method ("id", [], Ast.Typ ("int"));
      Ast.Method ("body", [], Ast.Typ ("string option list option"))] ) :: [] in

  assert (expt = result)|> ignore;
  Format.printf "\x1b[32mPassed type_schema_lists_test!\n\x1b[0m@?";;
;;

let parser_tests () =
  type_schema_id_field_test () |> type_schema_multiple_fields_test |> type_schema_lists_test;;

let () =
  parser_tests ();;
