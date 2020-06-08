let run = function
  | Some version -> (
      match Ocamlformat_repo.get version with
      | Some path -> `Ok (print_endline (Format.asprintf "%a" Fpath.pp path))
      | None -> `Ok () )
  | None -> `Ok ()

let version =
  let doc = "Version of ocamlformat package." in
  let docv = "VERSION" in
  Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv ~doc)

let info =
  let open Cmdliner in
  let doc = "get the binary path of some specific $(b,ocamlformat) version" in
  Term.info "get" ~doc

let term = Cmdliner.Term.(ret (pure run $ version))

let cmd = (term, info)
