(lang dune 3.3)

(using menhir 2.1)

(name graphql_to_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "ProgramingIsTheFuture")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name GraphqlAPI)
 (synopsis "Compiler from graphql to ocaml")
 (description "A longer description")
 (depends ocaml dune dream))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
