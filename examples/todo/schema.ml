open Graphql_lwt;;

type todo = { id: int; username: string option; password: string; age: int option;
              job: string option list option; };;

let todo: (Dream.request, todo option) Schema.typ = let open Schema in
  obj "Todo"
    ~fields:(fun _info -> [
      field "Id" ~typ:(non_null int) ~args:Arg.[] ~resolve:(fun _ v -> v.id); 
      field "Username" ~typ:(string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.username); 
      field "Password" ~typ:(non_null string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.password); 
      field "Age" ~typ:(int) ~args:Arg.[] ~resolve:(fun _ v -> v.age); 
      field "Job" ~typ:(list string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.job); 
      ]);;

type 'ctx schema = { todos: 'ctx Schema.resolve_info -> unit -> int option ->
                     todo list; };;

let schema schema_from_typ = let open Schema in
  Schema.schema [
    field "Todos" ~typ:(non_null (list (non_null todo))) 
      ~args:Arg.[arg "id" ~typ:(int);] ~resolve:schema_from_typ.todos; 
    ];;

