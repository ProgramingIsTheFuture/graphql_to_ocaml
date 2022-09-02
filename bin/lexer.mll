{
 open Parser


 exception Error of string
}

rule tokens = parse
| [' ' '\n' '\t']
  {tokens lexbuf}
| "type"
  { TYPEKW }
| ['a'-'z' 'A'-'Z']+ as name
  { NAME name }
| '{'
  { LBRACK }
| '}'
  { RBRACK }
| '('
  { LPARENT }
| ')'
  { RPARENT }
| ':'
  { DOUBLEDOT }
| ','
  { COMMA }
| '!'
  { EXCLAMATION "!" }
| eof
  { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
