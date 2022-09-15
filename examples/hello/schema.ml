open Graphql_lwt;;

type 'ctx schema = { hello: 'ctx Schema.resolve_info -> unit -> string; };;

let schema schema_from_typ = let open Schema in
  Schema.schema [
    field "hello" ~typ:(non_null string) ~args:Arg.[] 
      ~resolve:schema_from_typ.hello; 
    ];;

