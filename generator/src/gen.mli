(** [generate t f] generate the code from the tokens t into the file f. *)
val generate: Ast.schema -> out_channel -> unit
