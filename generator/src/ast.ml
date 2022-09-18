(** name is used to name types or methods inside the graphql schema *)
type name = string

(** typ corresponds to the type inside the schema *)
type typ =
  (* ID or String or Int... *)
  (* ! -> means it can be null or not  *)
  (* This bools means nullable or not *)
  | Typ of name

(** Paramethers to methods inside a type/schema *)
type params =
  (* Params to methods *)
  | Param of name * typ

(** Methods inside the schema
    {[
      type Name {
          Id (params list): typ
        }
    ]} *)
type methods =
  (* methods accept methos and fields *)
  | Method of name * (params list) * typ

(** {[
     schema {
       query: TypName;
       mutation: TypName;
     }
   ]} *)
type schemas = {
  query: name option;
  mutation: name option
};;

type expr =
  (* Declare a new type schema *)
  | TypeDecl of name * (methods list)
  (* Declare a new input schema *)
  | InpDecl of name * (name * typ)
  (* Declare a new Enum schema *)
  | EnumDecl of name * name list
  (* { *)
  (*   query: --- *)
  (*   mutation: ---- *)
  (* } *)
  | Schema of schemas

type schema =
  expr list
