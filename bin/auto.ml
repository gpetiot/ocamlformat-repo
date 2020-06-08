let run enable_outside_detected_project root =
  let aux root =
    match Ocamlformat_repo.auto ~enable_outside_detected_project ~root with
    | Some path -> `Ok (print_endline (Format.asprintf "%a" Fpath.pp path))
    | None -> `Ok ()
  in
  match root with
  | Some r -> (
      match Fpath.of_string r with
      | Ok root -> aux (Some root)
      (* the error will be handled by ocamlformat *)
      | Error _ -> `Ok () )
  | None -> aux None

let enable_outside_detected_project =
  let doc =
    "Read $(b,.ocamlformat) config files outside the current project. The \
     project root of an input file is taken to be the nearest ancestor \
     directory that contains a %s file. Formatting is enabled even if no \
     $(b,.ocamlformat) configuration file is found."
  in
  Cmdliner.Arg.(value & flag & info [ "enable-outside-detected-project" ] ~doc)

let root =
  let docv = "DIR" in
  let doc =
    "Root of the project. If specified, only take into account .ocamlformat \
     configuration files inside $(docv) and its subdirectories."
  in
  Cmdliner.Arg.(value & opt (some dir) None & info [ "root" ] ~doc ~docv)

let info =
  let open Cmdliner in
  let doc =
    "detect the required $(b,ocamlformat) version, install it if necessary"
  in
  Term.info "auto" ~doc

let term =
  Cmdliner.Term.(ret (pure run $ enable_outside_detected_project $ root))

let cmd = (term, info)
