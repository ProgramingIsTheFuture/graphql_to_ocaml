let todos: Schema.todo list ref = ref Schema.([
    {id = 1; username = None; password = "Pass1"; age = None; job = None;};
    {id = 9; username = None; password = "Pass2"; age = None; job = None;};
    {id = 4; username = Some "Someone"; password = "Pass3"; age = None; job = None;};
  ]);;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.any "/graphql" (Dream.graphql Lwt.return Schema.(schema ({ todos = fun _info () i ->
        match i with
        | Some v -> List.filter (fun j -> v == j.id) !todos
        | None -> !todos
      })));
    Dream.get "/" (Dream.graphiql "/graphql");
  ]
