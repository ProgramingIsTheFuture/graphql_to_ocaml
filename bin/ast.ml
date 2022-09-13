type name = string

type typ =
  (* ID or String or Int... *)
  (* ! -> means it can be null or not  *)
  (* This bools means nullable or not *)
  | Typ of name

type params =
  (* Params to methods *)
  | Param of name * typ

type methods =
  (* methods accept methos and fields *)
  | Method of name * (params list) * typ

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
