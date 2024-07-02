module Docker = Current_docker
module Git = Current_git
module Github = Current_github
module Config = Config

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
  let ocaml = Docker.Default.pull ~schedule:weekly "ocaml/opam" in
  let solver = Solver_worker.Solver_request.create ~n_workers:1 () in
  let sources (ppx : Config.ppx) =
    let org_repo = Uri.of_string ppx.url |> Uri.path in
    let fetch branch =
      (org_repo ^ "-" ^ branch, Git.clone ~schedule:weekly ~gref:branch ppx.url)
    in
    List.map fetch ppx.branches
  in
  let revdeps = Revdep.revdeps ~package:"ppxlib" opam in
  let build_ppx ppxlib_pin (ppx : Config.ppx) =
    List.map
      (fun (value, ppx) ->
        let build_ctx = Build.{ opam; ocaml; ppxlib_pin } in
        Current.collapse ~input:ppx ~key:"ppx" ~value
        @@ Build.build ~solver ~pool ~ppx build_ctx)
      (sources ppx)
    |> Current.all
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
      (fun (value, ppxlib) ->
        Current.collapse ~input:ppxlib ~key:"ppxlib" ~value
          (List.map (fun ppx -> build_ppx ppxlib ppx) config.ppxes
          |> Current.all))
      (ppxlib_sources config.ppxlibs)
  in
  (Current.ignore_value revdeps) :: builds |> Current.all
