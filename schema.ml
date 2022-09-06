open Graphql_lwt


type todo = {
	id: int option
}

let todo_schema =
    let open Schema in
    obj
        todo
        ~fields:[
         
            field
                "id"
                ~typ:(int not_null)
                ~args:Arg.[]
                ~resolve:(fun _ v -> v.id)

        ];;