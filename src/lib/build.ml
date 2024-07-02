module Git = Current_git
module Docker = Current_docker.Default

let spec ~ocaml ~opam ~deps ppxlib_pin =
  let open Obuilder_spec in
  let from = Current_docker.Default.Image.hash ocaml in
  let opam = Git.Commit.hash opam in
  let ppxlib_hash = Git.Commit.hash ppxlib_pin in
  let repo = Git.Commit.id ppxlib_pin |> Git.Commit_id.repo in
  let ppxlib_pin = Fmt.str "git+%s#%s" repo ppxlib_hash in
  (* Update DEPS to have ppxlib.dev *)
  let deps =
    List.map
      (fun dep ->
        match Astring.String.find_sub ~sub:"ppxlib." dep with
        | Some _ -> "ppxlib.dev"
        | _ -> dep)
      deps
  in
  stage ~from
    [
      comment "Use opam version 2.1";
      run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam";
      run "opam init --reinit -ni";
      run "uname -rs && opam exec -- ocaml -version && opam --version";
      comment "Checkout the specific commit of opam-repo that we solved against";
      run ~network:[ "host" ]
        "cd ~/opam-repository && (git cat-file -e %s || git fetch origin \
         master) && git reset -q --hard %s && git log --no-decorate -n1 \
         --oneline && opam update -u"
        opam opam;
      comment "Pin PPXLIB";
      run "opam pin -yn %s --with-version=dev" ppxlib_pin;
      comment "Install the dependencies of the ppx";
      env "DEPS" (String.concat " " deps);
      run ~network:[ "host" ] "opam install $DEPS";
      comment "Copy in the checkout of the ppx";
      copy [ "." ] ~dst:".";
      run "opam exec -- dune build @runtest @install @check";
    ]

type mode = Docker_local | Obuilder_local

type t = {
  opam : Git.Commit.t Current.t;
  ocaml : Current_docker.Default.Image.t Current.t;
  source : [ `Pin of Git.Commit.t Current.t | `Package of string ];
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
        Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir
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

let build ?(mode = Docker_local) ~pool ~ppx ~solver package =
  match mode, package with
  | Docker_local, `Pin { opam; ocaml; ppxlib_pin = pin } ->
      let open Current.Syntax in
      let obuilder_spec =
        let src = Repo_content.extract ppx in
        let opam_repository_commit = Current.map Git.Commit.id opam in
        let analysis = Solve.examine ~solver ~opam_repository_commit ppx src in
        let+ opam = opam
        and+ ocaml = ocaml
        and+ analysis = analysis
        and+ pin = pin in
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
        spec ~ocaml ~opam ~deps pin
      in
      let dockerfile =
        let+ obuilder_spec = obuilder_spec in
        `Contents
          (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix
             obuilder_spec)
      in
      Docker.build ~pull:false ~pool ~dockerfile (`Git ppx)
      |> Current.ignore_value
  | _ -> failwith "Unknown build mode"

