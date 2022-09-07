module type PP = sig
  type t = Format.formatter

  val pp: out_channel -> t

  val generate_code: Ast.schema -> t -> unit

end

module PrettyPrint: PP = struct
  type t = Format.formatter

  let name_to_lower = String.lowercase_ascii

  let type_to_string typ =
    if typ = "ID" then "int"
    else String.lowercase_ascii typ

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
          let typp = begin match typ with Typ (nnn, t) -> ((if nnn = "ID" then "int" else String.lowercase_ascii nnn), t) end in
          Format.fprintf
            f
            "@[%s: %s%s;@]@ "
            (name_to_lower nn)
            (fst typp)
            (if snd typp then " option" else "");
          get_fields lllm
        | _ -> () in
      get_fields lm in
    type_decl f n h;;

  let rec pp_expr e (f: Format.formatter) =
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
      Format.fprintf f "@[<2>obj %s@," (name_to_lower n);
      Format.fprintf f "@[<2>~fields:[@,";

      let rec h llm =
        match llm with
        | Method (nn, [] ,typ) :: lllm ->
          let tp =
            match typ with
            | Typ (nnn, tpp) ->
              (type_to_string nnn) ^ " " ^ (if tpp then "not_null" else "") in

          let namel = name_to_lower nn in
          Format.fprintf f "@[<2>field \"%s\"@ @," namel;
          Format.fprintf f "~typ:(%s)@ @," tp;
          Format.fprintf f "~args:Arg.[]@ @,";
          Format.fprintf f "~resolve:(fun _ v -> v.%s)@ @,@]@;<0 0>" namel;

          h lllm
        | _ -> () in

      h lm;
      Format.fprintf f "@]]@];;@]@.";


      (* let %s_schema = *)
      (* let open Schema in *)
      (*                        obj %s *)
      (*                        ~fields:(fun _info -> *)

      (*                            field %method% *)
      (*                                ~typ:(not_null string) *)
      (*                                ~args:Arg.[] *)

      (*                                    user.username *)
      (*                                ) *)
      (*                        ) *)
      (* ~typ:(not_null int) *)

      (*         ~args:Arg.[] *)
      (*         ~resolve:(fun _info user -> *)
      (*             user.id *)
      (*         ) *)

      (*  *)
      pp_expr tl f
    | _ -> ();;


  let pp (file: out_channel): t =
    (Format.formatter_of_out_channel file);;

  let generate_code (e: Ast.schema) (pp: t) =
    pp_imports pp |>
    pp_expr e;
    Format.pp_print_newline pp ();;

end
