open StdLabels
open Cmi_format
open Asttypes
open Types

let rec walk_sig path signature =
  List.iter signature ~f:(function
    | Sig_module (id, {md_type = Mty_signature s; _}, _) ->
        walk_sig (Printf.sprintf "%s.%s" path (Ident.name id)) s
    | Sig_value (id, vd) ->
        if
          List.exists vd.val_attributes ~f:(function
            | {txt = "toplevel_printer" | "ocaml.toplevel_printer"; _}, _ ->
                true
            | _ -> false )
        then
          Printf.printf "#install_printer %S\n"
            (Printf.sprintf "%s.%s" path (Ident.name id))
    | _ -> () )

let process_file fn =
  let cmi = Cmi_format.read_cmi fn in
  walk_sig cmi.cmi_name cmi.cmi_sign

module Cli = struct
  open Cmdliner

  let doc = "Generater toplevel printer installers"

  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Generate a bunch of toplevel directories that can be used
           in the toplevel by reading [@@toplevel_printer] attributes.|}
    ]

  let info = Term.info "auto-printer" ~doc ~man

  let term =
    let main includes files =
      List.iter includes ~f:(fun p ->
          Clflags.include_dirs := p :: !Clflags.include_dirs;
          Compmisc.init_path false;
          List.iter files ~f:process_file )
    in
    Term.(
      const main
      $ Arg.(
          value & opt_all dir []
          & info ["-I"] ~docv:"DIR" ~doc:"Add a directory to the search path")
      $ Arg.(
          value & pos_all file []
          & info [] ~docv:"CMI-FILE" ~doc:"Process the follwing .cmi file"))

  let command = (term, info)

  let () = ignore (Term.eval command : _ Term.result)
end
