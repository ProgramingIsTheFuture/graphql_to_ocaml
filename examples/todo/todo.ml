
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.any "/graphql" (Dream.graphql Lwt.return Schema.(schema ({ todos = fun _info () i ->
        match i with
        | Some v -> {id = v; username = None; password = "aa"; age = None; job = None;} :: []
        | None -> {id = 1; username = None; password = "aa"; age = None; job = None;} :: []
      })));
    Dream.get "/" (Dream.graphiql "/graphql");
  ]
