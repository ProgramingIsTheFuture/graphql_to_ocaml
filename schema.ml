open Graphql_lwt;;

type todo = { id: int; username: string option; password: string; age: int option;
              job: string option list option; };;

let todo_schema = let open Schema in
  obj "todo"
    ~fields:(fun _info -> [
      field "id" ~typ:(int) ~args:Arg.[] ~resolve:(fun _ v -> v.id); 
      field "username" ~typ:(non_null string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.username); 
      field "password" ~typ:(string) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.password); 
      field "age" ~typ:(non_null int) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.age); 
      field "job" ~typ:(non_null (list (non_null string))) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.job); 
      ]);;

