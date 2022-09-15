
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.any "/graphql" (Dream.graphql Lwt.return Schema.(schema ({ hello = fun _info () ->
        "Hello World"
      })));
    Dream.get "/" (Dream.graphiql "/graphql");
  ]
