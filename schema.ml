open Graphql_lwt;;

type todo = { id: int; username: string option; password: string option ->
                                                string;
              age: int option; job: string option list option; };;

type myquery = { todos: int option -> todo list; };;


