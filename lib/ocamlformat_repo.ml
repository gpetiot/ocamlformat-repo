open Bos.OS
open Rresult

let repo_dir () =
  Dir.user () >>= fun user_dir ->
  let fpath = Fpath.(user_dir / ".ocamlformat-repo") in
  Dir.create fpath >>| fun (_ : bool) -> fpath

let bin_dir () =
  repo_dir () >>= fun repo_dir ->
  let fpath = Fpath.(repo_dir / "bin") in
  Dir.create fpath >>| fun (_ : bool) -> fpath

let switch_dir () =
  match Bos.OS.Env.var "OPAM_SWITCH_PREFIX" with
  | Some v ->
      R.reword_error
        (fun _ -> R.msg "Could not detect opam root directory.")
        (Fpath.of_string v)
  | None -> R.error_msg "Could not detect opam root directory."

let list () =
  bin_dir () >>= fun bin_dir ->
  Dir.contents ~dotfiles:false ~rel:false bin_dir >>| fun contents ->
  List.fold_left
    (fun acc p ->
      if Fpath.is_file_path p then
        let name = Fpath.filename p in
        if Astring.String.is_prefix ~affix:"ocamlformat." name then
          (name, p) :: acc
        else acc
      else acc)
    [] contents

let get version =
  match list () with
  | Ok installed -> (
      match List.assoc_opt version installed with
      | Some x -> Some x
      | None -> List.assoc_opt ("ocamlformat." ^ version) installed )
  | Error _ -> None

let matches_version config (rel, ver) =
  let comparison =
    match rel with
    | `Eq -> ( = )
    | `Geq -> ( >= )
    | `Gt -> ( > )
    | `Leq -> ( <= )
    | `Lt -> ( < )
    | `Neq -> ( <> )
  in
  match Ocaml_version.of_string ver with
  | Ok ver -> comparison (Ocaml_version.compare config ver) 0
  | Error _ -> false

let read_ocaml_version (opam_file : OpamFile.OPAM.t) =
  match opam_file.ocaml_version with
  | Some f -> (
      let releases = List.rev Ocaml_version.Releases.all in
      let v =
        List.fold_left
          (fun acc release ->
            match acc with
            | Some r -> Some r
            | None when OpamFormula.eval (matches_version release) f ->
                Some release
            | None -> None)
          None releases
      in
      match v with
      | Some v -> Ok v
      | None -> R.error_msg "No suitable ocaml compiler version found." )
  | None -> Ok Ocaml_version.sys_version

let copy src dest =
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "cp") >>= fun cp ->
  let cmd = Bos.Cmd.(cp % Fpath.to_string src % Fpath.to_string dest) in
  Bos.OS.Cmd.run cmd

let current_switch () =
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "opam") >>= fun opam ->
  let cmd = Bos.Cmd.(opam % "switch" % "show") in
  let out = Bos.OS.Cmd.run_out cmd in
  Bos.OS.Cmd.to_string out

let git_clone version dest =
  let uri = "https://github.com/ocaml-ppx/ocamlformat.git" in
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "git") >>= fun git ->
  let cmd =
    Bos.Cmd.(
      git % "clone" % "-b" % version % "--depth" % "1" % uri
      % Fpath.to_string dest)
  in
  Bos.OS.Cmd.(run_out cmd |> to_null)

let install_pkg version =
  Logs.info (fun m -> m "Installing ocamlformat.%s" version);
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "opam") >>= fun opam ->
  let pkg = "ocamlformat." ^ version in
  let cmd = Bos.Cmd.(opam % "install" % "--yes" % pkg) in
  Bos.OS.Cmd.(run_out cmd |> to_null)

let find_ocaml_version version =
  let vname = "ocamlformat." ^ version in
  repo_dir () >>= fun repo_dir ->
  let src = Fpath.(repo_dir / vname) in
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "rm") >>= fun rm ->
  let cmd = Bos.Cmd.(rm % "-rf" % Fpath.to_string src) in
  Bos.OS.Cmd.run cmd >>= fun () ->
  git_clone version src >>= fun () ->
  let opam_file = Fpath.(src / "ocamlformat.opam") in
  let opam_file = OpamFilename.of_string (Fpath.to_string opam_file) in
  let opam_file = OpamFile.OPAM.read (OpamFile.make opam_file) in
  read_ocaml_version opam_file >>| Ocaml_version.to_string

let switch sw =
  Logs.info (fun m -> m "Switching to opam switch %s" sw);
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "opam") >>= fun opam ->
  let cmd = Bos.Cmd.(opam % "switch" % sw) in
  Bos.OS.Cmd.run cmd

let switch_status sw =
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "opam") >>= fun opam ->
  let cmd = Bos.Cmd.(opam % "switch" % sw) in
  Bos.OS.Cmd.(run_out cmd |> out_null) >>| fun ((), (_, x)) -> x

let on_switch ocaml_version ~f =
  let sw = "ocamlformat-repo." ^ ocaml_version in
  Bos.OS.Cmd.must_exist (Bos.Cmd.v "opam") >>= fun opam ->
  current_switch () >>= fun old_switch ->
  match switch_status sw with
  | Ok (`Exited 0) -> (
      Logs.info (fun m -> m "Switching to opam switch %s" sw);
      match f () with
      | Ok () -> switch old_switch
      | Error e -> switch old_switch >>= fun () -> Error e )
  | Ok (`Exited _ | `Signaled _) -> (
      Logs.info (fun m -> m "Creating opam switch %s ..." sw);
      let cmd = Bos.Cmd.(opam % "switch" % "create" % sw % ocaml_version) in
      Bos.OS.Cmd.(run_out cmd |> to_null) >>= fun () ->
      switch sw >>= fun _ ->
      match f () with
      | Ok () -> switch old_switch
      | Error e -> switch old_switch >>= fun () -> Error e )
  | Error e -> Error e

let install version =
  Logs.info (fun m -> m "Installing ocamlformat.%s ..." version);
  let vname = "ocamlformat." ^ version in
  bin_dir () >>= fun bin_dir ->
  find_ocaml_version version >>= fun ocaml_version ->
  Logs.info (fun m -> m "Require ocaml compiler %s" ocaml_version);
  on_switch ocaml_version ~f:(fun () ->
      install_pkg version >>= fun () ->
      switch_dir () >>= fun switch_dir ->
      copy Fpath.(switch_dir / "bin" / "ocamlformat") Fpath.(bin_dir / vname))
