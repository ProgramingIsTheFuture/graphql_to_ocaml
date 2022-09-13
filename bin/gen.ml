module type Organizer = sig
  val find_schema_decl: Ast.schema -> Ast.schemas option

  val find_typ_by_name: Ast.name -> Ast.schema -> Ast.expr option
end

module Organizer: Organizer  = struct
  open Ast

  type t

  (*  Encontra o entry point  *)
  let rec find_schema_decl = function
    | Schema s :: _ -> Some s
    | _ :: l ->
      find_schema_decl l
    | [] -> None

  (*  Encontra um typo com o nome n  *)
  let rec find_typ_by_name (n: string) = function
    | TypeDecl (name, _) as t :: l ->
      if n = name then Some t else find_typ_by_name n l
    | _ :: l -> find_typ_by_name n l
    | [] -> None

end



module Interp: sig
  type t

  val types_names: string list ref

  val declare_expr: Ast.expr -> t -> unit

  val find_expr_opt: t -> string -> Ast.expr option

  val declare: Ast.schema -> t -> unit

  val interp: unit -> t
end = struct
  type t = (string, Ast.expr) Hashtbl.t;;

  let types_names = ref [];;

  let declare_expr exp ctx =
    match exp with
    | Ast.TypeDecl (n, _) ->
      Hashtbl.add ctx n exp;
      types_names := n :: !types_names;
    | _ -> ()

  let find_expr_opt ctx n =
    Hashtbl.find_opt ctx n

  let declare_all_typs (fst: Ast.expr) (l: Ast.schema) (ctx: t) =
    match fst with
    | Ast.TypeDecl (_, meths) ->
      declare_expr fst ctx;
      let rec handle_meths = function
        | Ast.Method (_, params, typ) :: mets ->
          let typ_to_str = function
            | Ast.Typ str_typ -> str_typ in
          let () = match Organizer.find_typ_by_name (typ_to_str typ) l with
            | Some decl -> declare_expr decl ctx
            | None -> () in

          List.map (fun i ->
              match i with
              | Ast.Param (_, typ) ->
                match Organizer.find_typ_by_name (typ_to_str typ) l with
                | Some decl -> declare_expr decl ctx
                | None -> ()
            ) params |> ignore;
          handle_meths mets
        | [] -> ()
      in

      handle_meths meths
    | _ -> ()


  let declare (l: Ast.schema) (ctx: t)=
    let schema = Organizer.find_schema_decl l in
    match schema with
    | Some s ->
      let query = match s.query with | Some n -> n | None -> "" in
      let typ = Organizer.find_typ_by_name query l in
      declare_all_typs (Option.get typ) l ctx
    | None -> ();;

  let interp () : t =
    Hashtbl.create 10
end



module PrettyPrint: sig
  type t = Format.formatter

  val pp: out_channel -> t

  val generate_code: Ast.schema -> t -> unit

end = struct
  type t = Format.formatter

  let name_to_lower = String.lowercase_ascii;;

  let type_from_typ = function
    | Ast.Typ n -> n

  let rec pp_params f = function
    | Ast.Param (_, tt) :: ll ->
      Format.fprintf f "%s ->@ " (type_from_typ tt);
      pp_params f ll
    | [] -> ()

  let pp_methods_to_types f = function
    | Ast.Method (n, p, typ) ->
      Format.fprintf f "%s: " (name_to_lower n);

      pp_params f p;
      Format.fprintf f "%s;" (type_from_typ typ);
  ;;

  let ocamltyp_to_graphtyp s =
    let l = String.split_on_char ' ' s |> List.map
              (fun ss ->
                 if ss = "option" then "non_null" else ss
              )  |> (*|> List.rev*)
            List.rev in
    List.fold_left (fun (ss, parent, first, len) el ->
        if first then (ss ^ el, parent, false, len-1)
        (* Closing all the parents *)
        else if len = 1 then (ss ^ " " ^ el ^ (String.concat "" parent), parent, false, len)
        (* Add parent *)
        else (ss ^ " (" ^ el, ")"::parent, false, len-1)
      ) ("", [], true, List.length l) l |> function (r, _, _, _) -> r;;

  let pp_imports (f: t) =
    Format.fprintf f "open Graphql_lwt;;@,@."; f;;

  let type_decl (f: t) (name: string) (pp_rest: unit -> unit)=
    Format.fprintf f "type %s = {@[<1>" (name_to_lower name);
    Format.pp_print_space f ();

    pp_rest ();

    Format.fprintf f "@]};;@,@.";;

  let pp_types (f: t) n lm =
    let open Ast in
    let h () =
      let rec get_fields llm=
        match llm with
        | Method (nn, [] ,typ) :: lllm ->
          Format.pp_print_tab f ();
          let typp = begin match typ with Typ (nnn) -> (if nnn = "ID" then "int" else String.lowercase_ascii nnn) end in
          Format.fprintf
            f
            "@[%s: %s;@]@ "
            (name_to_lower nn)
            typp;
          get_fields lllm
        | _ -> () in
      get_fields lm in
    type_decl f n h;;

  let pp_args args (f: t) =
    Format.fprintf f "@[<2>~args:Arg.[@,";
    let open Ast in
    let rec h = function
      | [] -> ()
      | Param (name, typ) :: ll ->
        let typp =
          match typ with
          | Typ s -> s
        in
        Format.fprintf f "arg \"%s\" ~typ:(%s);@,"
          (name_to_lower name)
          (ocamltyp_to_graphtyp typp);
        h ll
    in

    h args;
    Format.fprintf f "]@]@ @,";;

  let rec pp_expr e (f: t) =
    let open Ast in
    match e with
    (* *)
    (* Project *)
    (*  type Project {
        name: String
        tagline: String
        contributors: [User]
        } *)
    | TypeDecl (n, lm) :: tl ->
      pp_types f n lm;

      Format.fprintf f "@[<2>let %s_schema =@ @," (name_to_lower n);
      Format.fprintf f "let open Schema in@,";
      Format.fprintf f "@[<2>obj \"%s\"@," (name_to_lower n);
      Format.fprintf f "@[<2>~fields:(fun _info -> [@,";

      let rec h llm =
        match llm with
        | Method (nn, args ,typ) :: lllm ->
          let tp =
            match typ with
            | Typ (nnn) ->
              (* Convert the ocamltypes to graphqltypes TODO *)
              (ocamltyp_to_graphtyp nnn) in

          let namel = name_to_lower nn in
          Format.fprintf f "@[<2>field \"%s\"@ @," namel;
          Format.fprintf f "~typ:(%s)@ @," tp;
          pp_args args f;
          Format.fprintf f "~resolve:(fun _ v -> v.%s);@ @,@]@;<0 0>" namel;

          h lllm
        | _ -> () in

      h lm;
      Format.fprintf f "@]])@];;@]@.";

      pp_expr tl f
    | _ -> ();;


  let pp (file: out_channel): t =
    (Format.formatter_of_out_channel file);;

  let generate_code (e: Ast.schema) (pp: t) =
    let interp = Interp.interp () in
    Interp.declare e  interp;
    pp_imports pp |>
    pp_expr e;
    Format.pp_print_newline pp ();;

end


let generate s gen =
  PrettyPrint.pp gen |>
  PrettyPrint.generate_code (Tokenizer.get_tokens s);;
