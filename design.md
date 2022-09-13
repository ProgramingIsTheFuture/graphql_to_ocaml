Parsing the code order:

Compilation will the parts needed

- Find the schema definition
  - Ex: `schema {query: QueryName\n mutation: MutationName}`
- Generate the OCaml schema to QueryName with all its fields

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

### More OCaml exampels with Graphql_lwt

Input:

```graphql
type User {
  id: ID!
  Name: String
}

type Query {
  Users(id: ID): [Users!]!
}

schema {
  query: Query
}
```

Output:

```ocaml

type user = { id: int; name: string }

let hardcoded_users = [
    { id = 1; name = "FirstName" };
    { id = 2; name = "SecondtName" };
]

let user =
  Graphql_lwt.Schema.(obj "user"
    ~fields:(fun _info -> [
      field "id"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info user -> user.id);
      field "name"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info user -> user.name);
    ]))

let schema =
  Graphql_lwt.Schema.(schema [
    field "users"
      ~typ:(non_null (list (non_null user)))
      ~args:Arg.[arg "id" ~typ:int]
      ~resolve:(fun _info () id ->
        match id with
        | None -> hardcoded_users
        | Some id' ->
          match List.find_opt (fun {id; _} -> id = id') hardcoded_users with
          | None -> []
          | Some user -> [user]);
  ])

```

### Final work

Input:

```graphql
schema {
  query: MyQuery
}

type MyQuery {
  users: [User!]!
}

type User {
  id: ID!
  name: String
}
```

Output (for now):

it will take the schema and will create the new schema like this

`let schema %s =`
` Graphql_lwt.Schema.(schema [`
` field "users"`
` ~typ:(non_null (list (non_null user)))`
` ~args:Arg.[]`
` ~resolve:%s;`
` ])`

Where the first `%s` the name of all resolver methods
the second `%s` is and exemple of where that resolvers names are used

after that it will generate the types and types schemas

`type user = { id: int; name: string }`

`let user =`
` Graphql_lwt.Schema.(obj "user"`
` ~fields:(fun _info -> [`
` field "%s"`
` ~typ:(non_null int)`
` ~args:Arg.[]`
` ~resolve:(fun _info user -> user.id);`
` field "%s"`
` ~typ:(non_null string)`
` ~args:Arg.[]`
` ~resolve:(fun _info user -> user.name);`
` ]))`
