%token <Ast.name> NAME
%token EOF DOUBLEDOT TYPEKW COMMA
%token <string> EXCLAMATION // INPUT
%token RBRACK LBRACK RPARENT LPARENT

%start schema

%type <Ast.schema> schema

%%

typ:
| DOUBLEDOT n = NAME e2 = EXCLAMATION?
 {
   match e2 with
   | Some _ -> Ast.Typ (n, true)
   | None -> Ast.Typ (n, false)
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
