let print (version, path) =
  print_endline (Format.asprintf "%s\t%a" version Fpath.pp path)

let run () =
  match Ocamlformat_repo.list () with
  | Ok installed -> `Ok (Stdlib.List.iter print installed)
  | Error (`Msg e) -> `Ok (Fmt.epr "[ERROR] %s\n" e)

let info =
  let open Cmdliner in
  let doc = "list managed $(b,ocamlformat) versions" in
  Term.info "list" ~doc

let term = Cmdliner.Term.(ret (const run $ pure ()))

let cmd = (term, info)
