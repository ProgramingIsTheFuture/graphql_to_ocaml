open Graphql_lwt;;

type todo = { id: int; username: string option; password: string option ->
                                                string;
              age: int option; job: string option list option; };;

let todo_schema = let open Schema in
  obj "todo"
    ~fields:(fun _info -> [
      field "id" ~typ:(int) ~args:Arg.[] ~resolve:(fun _ v -> v.id); 
      field "username" ~typ:(non_null string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.username); 
      field "password" ~typ:(string) 
        ~args:Arg.[arg "username" ~typ:(non_null string);] 
        ~resolve:(fun _ v -> v.password); 
      field "age" ~typ:(non_null int) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.age); 
      field "job" ~typ:(non_null (list (non_null string))) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.job); 
      ]);;
type schema = { todos: int option -> todo list; };;

let schema schema_from_typ = let open Schema in
  Schema.schema [
    field "todos" ~typ:(list todo) ~args:Arg.[arg "id" ~typ:(non_null int);] 
      ~resolve:schema_from_typ.todos; 
    ]

