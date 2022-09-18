# graphql_to_ocaml

This projects helps us generating OCaml code based on a graphql code.

Build more, write less.

This project is an atempt to transform the process of building graphql api
into something faster and esier.

This is super useful to work with .graphql files and use them inside our ocaml application.

## Working with this project

Cli:

- `-o` - where to store the generated code (optional) (default: "schema.ml")
- filename - Graphql code to compile (required)

Exampels are located inside the examples folder.

## Generation Example

Receiving a Graphql file like this:

```graphql
schema {
  query: MyQuery
}

type MyQuery {
  Hello: String!
}
```

Will give us a OCaml file like this:

```ocaml
open Graphql_lwt;;

type 'ctx schema = { hello: 'ctx Schema.resolve_info -> unit -> string; };;

let schema schema_from_typ = let open Schema in
  Schema.schema [
    field "Hello" ~typ:(non_null string) ~args:Arg.[]
      ~resolve:schema_from_typ.hello;
    ];;
```

## Inspiration

This project is inspired by the [gqlgen](https://github.com/99designs/gqlgen) built with Golang.

## TODO

- [x] Create a basic CLI
  - Run tests
  - Compile graphql code
- [x] Upgrade the identation with Format module
- [x] Add support to types with methods
- [x] Add support to the schema initialization
- [x] Reorganize the project
- [x] Add more documentation
- [ ] Write more exampels and tests
- [ ] Test the code with a simple Graphql Application
- [ ] Add support for mutation schema
- [ ] Add graph dependency
