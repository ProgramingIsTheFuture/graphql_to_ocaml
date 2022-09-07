open Graphql_lwt;;

type todo = { id: int option; username: string; password: string option; 
              age: int; job: string; };;

let todo_schema = let open Schema in
  obj todo
    ~fields:[
      field "id" ~typ:(int not_null) ~args:Arg.[] ~resolve:(fun _ v -> v.id) 
      field "username" ~typ:(string ) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.username) 
      field "password" ~typ:(string not_null) ~args:Arg.[] 
        ~resolve:(fun _ v -> v.password) 
      field "age" ~typ:(int ) ~args:Arg.[] ~resolve:(fun _ v -> v.age) 
      field "job" ~typ:(string ) ~args:Arg.[] ~resolve:(fun _ v -> v.job) ];;

