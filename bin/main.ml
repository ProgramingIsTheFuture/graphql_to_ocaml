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

let test = ref false ;;
let file = ref "schema.grapqhl" ;;
let outfile = ref "schema.ml" ;;
let speclist =
  Arg.[
    ("--no-tests", Set test, "Disable the test execution");
    ("-o", Set_string outfile, "Output file name");
  ];;

let () =
  Arg.parse speclist (fun n -> file := n) "Usage message";

  if !test then Tests.run_tests ()
  else begin

    Format.printf "Out[%s]\n" !outfile;
    let gen = open_out !outfile in

    Format.printf "In[%s]\n" !file;
    let s = Tokenizer.read_file !file in
    Format.printf "File:\n%s\n" s;
    let tokens = Tokenizer.get_tokens s in

    Gen.generate tokens gen;
    close_out gen;
  end;;
