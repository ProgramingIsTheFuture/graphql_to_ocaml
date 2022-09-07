%token <Ast.name> NAME
%token EOF DOUBLEDOT TYPEKW COMMA
%token EXCLAMATION // INPUT
%token RBRACK LBRACK RPARENT LPARENT RRETPARENT LRETPARENT

%start schema

%type <Ast.schema> schema

%%

typ:
// : ID!
| DOUBLEDOT lrp = LRETPARENT? n = NAME e2 = EXCLAMATION? rrp = RRETPARENT? e3 = EXCLAMATION?
 {
   let n = if n = "ID" then "int" else String.lowercase_ascii n in
   let e3 = if rrp = None then Some () else e3 in
   let lis = if lrp <> None && rrp <> None then " list" else "" in

   let opt s e =
     match e with
     | None -> s ^ " option"
     | Some _ -> s in

   Ast.Typ ( opt ((opt n e2) ^ lis) e3 )
 }

params:
| n = NAME t = typ COMMA?
 { Ast.Param(n, t) }

methods:
| n = NAME t = typ
 { Ast.Method (n, [], t) }
| n = NAME LPARENT p = list(params) RPARENT t = typ
 { Ast.Method (n, p, t) }

expr:
| TYPEKW n = NAME LBRACK m = list(methods) RBRACK
 { TypeDecl (n, m) }

schema:
| e = list(expr) EOF
 { e }
