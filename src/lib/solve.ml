open Lwt.Infix
open Current.Syntax
module Worker = Solver_service_api.Worker

let pool = Current.Pool.create ~label:"analyse" 400
let ( >>!= ) = Lwt_result.bind

(* A logging service that logs to [job]. *)
let job_log job =
  let module X = Solver_service_api.Raw.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

module Analysis = struct
  type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
  [@@deriving yojson]

  type t = {
    opam_files : string list;
    selections : Worker.Selection.t list;
    src_content : Repo_content.Content.t;
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files

  (** Call the solver with a request containing these packages. When it returns
      a list, it is nonempty. *)
  let solve ~root_pkgs ~pinned_pkgs ~platforms ~opam_repository_commits ~job
      ~solver =
    let request =
      {
        Solver_service_api.Worker.Solve_request.opam_repository_commits =
          List.map
            (fun v ->
              (Current_git.Commit_id.repo v, Current_git.Commit_id.hash v))
            opam_repository_commits;
        root_pkgs;
        pinned_pkgs;
        platforms;
      }
    in
    Current.Job.log job "Solving with opam-repository commits: %a"
      Fmt.(list Current_git.Commit_id.pp)
      opam_repository_commits;
    Current.Job.log job "Request: %a" Yojson.Safe.pp
      (Worker.Solve_request.to_yojson request);
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Lwt_switch.with_switch (fun switch ->
        Solver_worker.Solver_request.solve ~switch ~request ~log solver)
    >|= function
    | Error `Cancelled -> Fmt.error_msg "Job cancelled"
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg
    | Ok [] -> Fmt.error_msg "No solution found for any supported platform"
    | Ok x -> (
        let has_no_platform (pkg_version, _) =
          if
            List.exists
              (fun (sel : Worker.Selection.t) ->
                List.mem pkg_version sel.compat_pkgs)
              x
          then None
          else Some pkg_version
        in
        match List.filter_map has_no_platform root_pkgs with
        | [] -> Ok x
        | missing_pkgs ->
            Fmt.error_msg "No solution found for %a on any supported platform"
              Fmt.(list ~sep:comma Dump.string)
              missing_pkgs)

  let opam_dep_file packages =
    {|opam-version: "2.0"|} :: {|depends: [|}
    :: List.map
         (fun (pkg, ver) -> Printf.sprintf {|  "%s" %s|} pkg ver)
         packages
    @ [ "]\n" ]
    |> String.concat "\n"

  let exactly v = Printf.sprintf {|{ = "%s" }|} v

  let of_content ~solver ~job ~platform ~opam_repository_commits
      (Repo_content.Content.{ opam_files; root_pkgs; pinned_pkgs; _ } as
       src_content) =
    let solve = solve ~opam_repository_commits ~job ~solver in
    if opam_files = [] then Lwt_result.fail (`Msg "No opam files found!")
    else if List.filter Fpath.is_seg opam_files = [] then
      Lwt_result.fail (`Msg "No top-level opam files found!")
    else
      let platforms = [ platform ] in
      solve ~root_pkgs ~pinned_pkgs ~platforms >>!= fun selections ->
      let r = { opam_files; selections; src_content } in
      Current.Job.log job "@[<v2>Results:@,%a@]"
        Yojson.Safe.(pretty_print ~std:true)
        (to_yojson r);
      Lwt_result.return r
end

let platform =
  ( "debian-12-5.1_opam-2.1",
    Worker.Vars.
      {
        arch = "x86_64";
        os = "linux";
        os_family = "linux";
        os_distribution = "ubuntu";
        os_version = "24.04";
        ocaml_package = "ocaml-base-compiler";
        ocaml_version = Ocaml_version.Releases.v5_1 |> Ocaml_version.to_string;
        opam_version = "2.1.5";
        lower_bound = false;
      } )

module Examine = struct
  type t = Solver_worker.Solver_request.t

  module Key = struct
    type t = Repo_content.Extract.Key.t

    let digest src =
      let src =
        match src with
        | Repo_content.Extract.Key.Git git ->
            `String (Current_git.Commit.hash git)
        | Pkg (name, opam_file) ->
            `Assoc
              [
                ("pkg", `String name);
                ("opam_file", `String (Digest.string opam_file));
              ]
      in
      let json =
        `Assoc
          [ ("src", src); ("platform", Worker.Vars.to_yojson (snd platform)) ]
      in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository_commits : Current_git.Commit_id.t list;
      src_content : Repo_content.Content.t;
    }

    let digest { opam_repository_commits; src_content } =
      let json =
        `Assoc
          [
            ( "opam-repositories",
              `List
                (List.map
                   (fun opam_repository_commit ->
                     `String (Current_git.Commit_id.hash opam_repository_commit))
                   opam_repository_commits) );
            ("src_content", `String (Repo_content.Content.marshal src_content));
          ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job _src { Value.opam_repository_commits; src_content } =
    Current.Job.start job ~pool ~level:Current.Level.Harmless >>= fun () ->
    Analysis.of_content ~solver ~opam_repository_commits ~platform ~job
      src_content

  let pp f _ = Fmt.string f "Analyse"
  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic (Examine)

let examine ~solver ~opam_repository_commit revdep =
  Current.component "Analyse"
  |> let> src = revdep
     and> src_content = Repo_content.extract revdep
     and> opam_repository_commit = opam_repository_commit in
     let opam_repository_commits = [ opam_repository_commit ] in
     Examine_cache.run solver src
       { Examine.Value.opam_repository_commits; src_content }
