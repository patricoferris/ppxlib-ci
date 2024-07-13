module Docker = Current_docker
module Git = Current_git
module Github = Current_github
module Config = Config
module Index = Index

let ppxlib_pin_to_url s =
  match Astring.String.cut ~sep:"/" s with
  | Some (org, repo_with_branch) -> (
      match Astring.String.cut ~sep:"#" repo_with_branch with
      | Some (repo, branch) ->
          (Fmt.str "https://github.com/%s/%s" org repo, branch)
      | _ -> Fmt.invalid_arg "Failed to parse: %s" s)
  | _ -> Fmt.invalid_arg "Failed to parse: %s" s

let pipeline (config : Config.t) =
  let pool = Current.Pool.create ~label:"builds" 1 in
  let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) () in
  (* First we get the latest opam checkout, we update this weekly *)
  let opam =
    Git.clone ~schedule:weekly ~gref:"master"
      "https://github.com/ocaml/opam-repository.git"
  in
  let jane =
    Git.clone ~schedule:weekly ~gref:"master"
      "https://github.com/janestreet/opam-repository.git"
  in
  let ocaml =
    Docker.Default.pull ~schedule:weekly "ocaml/opam:debian-12-ocaml-5.1"
  in
  let solver = Solver_worker.Solver_request.create ~n_workers:1 () in
  let sources (ppx : Config.ppx) =
    let org_repo = Uri.of_string ppx.url |> Uri.path in
    let fetch branch =
      let open Current.Syntax in
      let k =
        let+ git = Git.clone ~schedule:weekly ~gref:branch ppx.url in
        Repo_content.Extract.Key.Git git
      in
      (org_repo ^ "-" ^ branch, k)
    in
    List.map fetch ppx.branches
  in
  let revdeps = Revdep.revdeps ~package:"ppxlib" opam in
  let build_ppx source ppx =
    let build_ctx = Build.{ opam; ocaml; source; jane } in
    match ppx with
    | `Custom (ppx : Config.ppx) ->
        List.map
          (fun (_value, revdep) -> Build.build ~solver ~pool ~revdep build_ctx)
          (sources ppx)
        |> Current.all
    | `Package pkg -> Build.build ~solver ~pool ~revdep:pkg build_ctx
  in
  let ppxlib_pins (ppxlibs : Config.ppxlibs) =
    let main = ppxlibs.main |> ppxlib_pin_to_url in
    let extras =
      ppxlibs.extras |> List.map (fun s -> (s, ppxlib_pin_to_url s))
    in
    (ppxlibs.main, main) :: extras
  in
  let ppxlib_sources (ppxlibs : Config.ppxlibs) =
    let fetch (ppxlib_org_repo, (ppxlib_url, branch)) =
      ( Fmt.str "%s-%s" ppxlib_org_repo branch,
        Git.clone ~schedule:weekly ~gref:branch ppxlib_url )
    in
    List.map fetch (ppxlib_pins ppxlibs)
  in
  let builds =
    List.map
      (fun (_value, ppxlib) ->
        List.map
          (fun ppx -> build_ppx ppxlib ppx)
          (List.map (fun v -> `Custom v) config.ppxes)
        |> Current.all)
      (ppxlib_sources config.ppxlibs)
  in
  let revdeps_builds =
    let open Current.Syntax in
    let* revdeps = revdeps in
    List.map
      (fun (_value, ppxlib) ->
        List.map (build_ppx ppxlib) (List.map (fun v -> `Package v) revdeps)
        |> Current.all)
      (ppxlib_sources config.ppxlibs)
    |> Current.all
  in
  revdeps_builds :: builds |> Current.all
