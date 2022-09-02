module type PP = sig
  type t = Format.formatter

  val pp: out_channel -> t

  val generate_code: Ast.schema -> t -> unit


end

module PrettyPrint: PP = struct
  type t = Format.formatter

  let type_to_string typ =
    if typ = "ID" then "int"
    else String.lowercase_ascii typ

  let pp_imports (f: Format.formatter) =
    Format.fprintf f "open Graphql_lwt\n" |> ignore; Format.fprintf f "\n\n"; f;;

  let pp_types (f: Format.formatter) n lm =
    let open Ast in
    Format.fprintf f "type %s = {\n" n;
    Format.pp_open_tbox f ();
    Format.fprintf f "\t";
    Format.pp_set_tab f ();
    let rec h llm =
      match llm with
      | Method (nn, [] ,typ) :: lllm ->
        Format.pp_print_tab f ();
        let typp = begin match typ with Typ (nnn, t) -> ((if nnn = "ID" then "int" else String.lowercase_ascii nnn), t) end in
        Format.fprintf
          f
          "%s: %s %s"
          nn
          (fst typp)
          (if snd typp then "option" else "");
        h lllm
      | _ -> () in
    h lm;
    Format.pp_close_tbox f ();
    Format.fprintf f "\n}\n";;

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
      let name_lower = String.lowercase_ascii n in
      pp_types f name_lower lm;

      Format.fprintf f "
let %s_schema =
    let open Schema in
    obj
        %s
        ~fields:[
         " name_lower name_lower;

      let rec h llm =
        match llm with
        | Method (nn, [] ,typ) :: lllm ->
          let tp =
            match typ with
            | Typ (nnn, tpp) ->
              (type_to_string nnn) ^ " " ^ (if tpp then "not_null" else "") in


          Format.fprintf f "
            field
                \"%s\"
                ~typ:(%s)
                ~args:Arg.[]
                ~resolve:(fun _ v -> v.%s)
" nn tp nn;

          h lllm
        | _ -> () in

      h lm;
      Format.fprintf f "
        ];;";


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
    Format.fprintf pp "@?";
    Format.pp_print_flush pp ();;

end
