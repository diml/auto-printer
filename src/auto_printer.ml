open Stdlabels

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
          Compmisc.init_path false )
    in
    let%map common = Common.term in
    Common.set_common common ~targets:[];
    let log = Log.create common in
    Scheduler.go ~log ~common (fun () ->
        Import.Main.setup ~log common
        >>= fun setup -> Dune.Upgrader.upgrade setup.file_tree )

  let command = (term, info)

  let () = ignore (Term.eval command : _ Term.result)
end
