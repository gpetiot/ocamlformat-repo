let run () = `Help (`Pager, None)

let info =
  let open Cmdliner in
  let doc = "managing ocamlformat versions" in
  let sdocs = Manpage.s_common_options in
  let man_xrefs = [ `Tool "ocamlformat" ] in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) provides a convenient way to manage co-installation of \
         several $(b,ocamlformat) package versions.";
    ]
  in
  Term.info "ocamlformat-repo" ~version:Version.version ~doc ~man_xrefs ~sdocs
    ~man

let term = Cmdliner.Term.(ret (const run $ pure ()))

let cmd = (term, info)
