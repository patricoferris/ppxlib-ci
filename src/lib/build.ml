module Git = Current_git
module Docker = Current_docker.Default

let spec ~ocaml ~opam ~deps ~package_pin ~package_name ~root_pkgs ~from_checkout
    =
  let open Obuilder_spec in
  let from = Current_docker.Default.Image.hash ocaml in
  let opam = Git.Commit.hash opam in
  let package_hash = Git.Commit.hash package_pin in
  let repo = Git.Commit.id package_pin |> Git.Commit_id.repo in
  let package_pin = Fmt.str "git+%s#%s" repo package_hash in
  let deps =
    List.map
      (fun dep ->
        match Astring.String.find_sub ~sub:(package_name ^ ".") dep with
        | Some _ -> Fmt.str "%s.dev" package_name
        | _ -> dep)
      deps
  in
  let revdep_opams =
    root_pkgs
    |> List.map (fun v ->
           try Filename.chop_extension v with Invalid_argument _ -> v)
    |> List.map (Fmt.str "%s.opam")
  in
  let revdep_roots = root_pkgs |> String.concat " " in
  let build_and_test =
    if from_checkout then
      [
        comment "Install the dependencies of the revdep";
        workdir "/src";
        copy revdep_opams ~dst:".";
        run "opam pin add -yn %s ./" revdep_roots;
        env "DEPS" (String.concat " " deps);
        run ~network:[ "host" ]
          "opam update --depexts && opam install --cli=2.1 --depext-only -y \
           $DEPS";
        run ~network:[ "host" ] "opam install $DEPS";
        comment "Copy in the checkout of the ppx";
        copy [ "." ] ~dst:".";
        run "opam exec -- dune build @install @check @runtest && rm -rf _build";
      ]
    else
      (* If from_checkout is false, then we're doing a more normal revdeps
         check, similar to opam-repo-ci and in this case there should only be
         a single root_pkg with a version number *)
      let rootpkg, rootpkg_version =
        match root_pkgs with
        | [ pkg ] -> (
            match Astring.String.cut ~sep:"." pkg with
            | Some (_, version) -> (pkg, version)
            | None -> failwith ("Expected pkg.version and got " ^ pkg))
        | _ -> failwith "Expected one package for a non-checkout build"
      in
      [
        run "opam pin add -k version -yn %s %s" rootpkg rootpkg_version;
        run ~network:[ "host" ]
          "opam update --depexts && opam install --cli=2.1 --depext-only -y \
           $DEPS";
        run ~network:[ "host" ] "opam install $DEPS";
        run "opam reinstall --with-test %s" rootpkg;
      ]
  in
  stage ~from
    ([
       comment "Use opam version 2.1";
       run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam";
       run "opam init --reinit -ni";
       run "uname -rs && opam exec -- ocaml -version && opam --version";
       comment
         "Checkout the specific commit of opam-repo that we solved against";
       run ~network:[ "host" ]
         "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
          master) && git reset -q --hard %s && git log --no-decorate -n1 \
          --oneline && opam update -u"
         opam opam;
       comment "Pin PACKAGE";
       run "opam pin -yn %s --with-version=dev" package_pin;
     ]
    @ build_and_test)

type mode = Docker_local | Cluster of Cluster_api.Submission.t

type t = {
  opam : Git.Commit.t Current.t;
  jane : Git.Commit.t Current.t;
  ocaml : Current_docker.Default.Image.t Current.t;
  source : Git.Commit.t Current.t;
}

let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.')
  then name ^ ".dev"
  else name

let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg =
        Filename.basename x |> fun v ->
        (try Filename.chop_extension v
         with Invalid_argument _ -> failwith "Chop extension: " ^ v)
        |> maybe_add_dev ~dir
      in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir ->
          (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [ x ], [ pkg ]) :: acc)

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) :: _ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let get_job_id x =
  let open Current.Syntax in
  let+ md =
    try Current.Analysis.metadata x with Failure _ -> Current.return None
  in
  match md with Some { Current.Metadata.job_id; _ } -> job_id | None -> None

let build ?(mode = Docker_local) ~pool ~revdep ~solver
    { opam; jane = _; ocaml; source } =
  match mode with
  | Docker_local ->
      let open Current.Syntax in
      let obuilder_spec =
        let opam_repository_commit = Current.map Git.Commit.id opam in
        let analysis = Solve.examine ~solver ~opam_repository_commit revdep in
        let+ opam = opam
        and+ ocaml = ocaml
        and+ analysis = analysis
        and+ package_pin = source in
        let selection = List.hd analysis.Solve.Analysis.selections in
        let groups = group_opam_files analysis.opam_files in
        let root_pkgs = get_root_opam_packages groups in
        let deps =
          selection.packages
          |> List.filter (fun pkg ->
                 (not (List.mem pkg root_pkgs))
                 && not
                      (Astring.String.take ~rev:true ~sat:(( <> ) '.') pkg
                      |> String.equal "opam"))
        in
        spec ~ocaml ~opam ~deps ~package_pin ~package_name:"ppxlib" ~root_pkgs
          ~from_checkout:analysis.src_content.from_checkout
      in
      let dockerfile =
        let+ obuilder_spec = obuilder_spec in
        `Contents
          (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix
             obuilder_spec)
      in
      let build =
        let* revdep = revdep in
        let src =
          match revdep with
          | Repo_content.Extract.Key.Git git -> `Git (Current.return git)
          | _ -> `No_context
        in
        Docker.build ~pull:false ~pool ~dockerfile src
      in
      let* id = get_job_id build
      and+ source = source
      and+ revdep = revdep
      and+ state = Current.state ~hidden:true build
      and+ obuilder_spec_state = Current.state ~hidden:true obuilder_spec in
      let status =
        match state with
        | Ok _image -> `Passed
        | Error (`Active (`Ready | `Waiting_for_confirmation)) -> (
            match obuilder_spec_state with
            | Error (`Active `Running) -> `Analysing
            | Error (`Msg m) -> `Failed m
            | _ -> `Not_started)
        | Error (`Active `Running) -> `Pending
        | Error (`Msg m) -> `Failed m
      in
      Index.record ~package:(Git.Commit.id source) ~revdep ~status
        (Index.Job_map.singleton "job" id);
      Current.ignore_value build
  | _ -> failwith "Unknown build mode"
