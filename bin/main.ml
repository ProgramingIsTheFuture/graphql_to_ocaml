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

let test = ref true ;;
let speclist =
  Arg.[("--no-tests", Set test, "Disable the test execution");];;

let () =
  Arg.parse speclist (fun n -> if n = "no-tests" then test := false) "Usage message";

  if !test then Tests.run_tests ()
  else

    let gen = open_out "schema.ml" in

    let s =  "type Todo { id: ID!\n Username: String\n Password: String!\n Age: Int\n Job: [String] }" in(*Tokenizer.read_file "schema.graphql" in*)

    Gen.PrettyPrint.pp gen |>
    Gen.PrettyPrint.generate_code (Tokenizer.get_tokens s);
    close_out gen;
