(** @author ProgramingIsTheFuture *)
(** @version 0.1.0 *)

open Generator

let file = ref "schema.graphql" ;;
let outfile = ref "schema.ml" ;;
let speclist =
  Arg.[
    ("-o", Set_string outfile, "Output file name");
  ];;

let () =
  Arg.parse speclist (fun n -> file := n) "Usage message";


  Format.printf "Out[%s]\n" !outfile;
  let gen = open_out !outfile in

  Format.printf "In[%s]\n" !file;
  let s = Tokenizer.read_file !file in
  Format.printf "File:\n%s\n" s;
  let tokens = Tokenizer.get_tokens s in

  Gen.generate tokens gen;
  close_out gen;;
