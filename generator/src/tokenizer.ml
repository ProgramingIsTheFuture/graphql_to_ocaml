(** [read_file fname] takes fname as a file name, opens that file and reads all information inside it
    Finally it will return a big string with all the information inside the file *)
let read_file fname =
  let file = open_in fname in
  let rec read_all ss =
    try
      read_all (ss ^ (input_line file))
    with
      End_of_file -> ss
  in

  read_all "";;


(** [get_tokens s] parses the string s into a list of tokens *)
let get_tokens s =
  Parser.schema Lexer.tokens (Lexing.from_string s)
