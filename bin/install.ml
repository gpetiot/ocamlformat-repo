let run = function
  | Some version -> (
      match Ocamlformat_repo.install version with
      | Ok () -> `Ok ()
      | Error (`Msg e) -> `Ok (Fmt.epr "[ERROR] %s\n" e) )
  | None -> `Ok (print_endline "Nothing to do.")

let version =
  let doc = "Version of ocamlformat package." in
  let docv = "VERSION" in
  Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv ~doc)

let info =
  let open Cmdliner in
  let doc = "install some specific $(b,ocamlformat) versions" in
  Term.info "install" ~doc

let term = Cmdliner.Term.(ret (pure run $ version))

let cmd = (term, info)
