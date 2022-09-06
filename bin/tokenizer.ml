let read_file fname =
  let file = open_in fname in
  let rec read_all ss =
    try
      read_all ((input_line file) ^ ss)
    with
      End_of_file -> ss
  in

  read_all "";;


let get_tokens s =

  Parser.schema Lexer.tokens (Lexing.from_string s)
