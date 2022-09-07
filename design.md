## Simple type declaration

Input:

```graphql
type User {
  id: ID!
  username: String!
}
```

Output:

```ocaml
open Graphql_lwt

type user = {id: int, username: string}

let user_schema =
    let open Schema in
    obj "user"
    ~fields:(fun _info ->
        field "id"
            ~typ:(not_null int)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.id
            )
        field "username"
            ~typ:(not_null string)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.username
            )
    )
```

## Another simple example

Input:

```graphql
type User {
  id: ID!
  username: String
}
```

Output:

```ocaml
open Graphql_lwt

type user = {id: int, username: string option}

let user_schema =
    let open Schema in
    obj "user"
    ~fields:(fun _info ->
        field "id"
            ~typ:(not_null int)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.id
            )
        field "username"
            ~typ:string
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.username
            )
    )
```

## Type with methods

Input:

```graphql
type User {
  id: ID!
  username: String!
}

type Query {
  user(id: ID!): User
}
```

Output:

```ocaml
open Graphql_lwt

type user = {id: int, username: string}

let user_schema =
    let open Schema in
    obj "user"
    ~fields:(fun _info ->
        field "id"
            ~typ:(not_null int)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.id
            )
        field "username"
            ~typ:(not_null string)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.username
            )
    )

let query_schema user_resolve =
    let open Schema in
    schema [
        field "user"
            ~typ:user_schema
            ~args:Arg.[arg "id" ~typ:(not_null int)]
            ~resolve:user_resolve
    ]
```

### TODO Sample

Input:

```graphql
type Todo {
  id: ID!
  body: String!
}

type Schema {
  GetAll: [Todo]!
  GetTodo(id: ID!): Todo
}
```

Output:

```OCaml

type user = {id: int, body: string}

let user_schema =
    let open Schema in
    obj "user"
    ~fields:(fun _info ->
        field "id"
            ~typ:(not_null int)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.id
            )
        field "body"
            ~typ:(not_null string)
            ~args:Arg.[]
            ~resolve:(fun _info user ->
                user.body
            )
    )

let query_schema getall_resolve gettodo_resolve =
    let open Schema in
    schema [
        field "getall"
            ~typ:user_schema
            ~args:Arg.[]
            ~resolve:getall_resolve;
        field "gettodo"
            ~typ:user_schema
            ~args:Arg.[arg "id" ~typ:(not_null int)]
            ~resolve:gettodo_resolve;
    ]

```
