(* This module is an OCurrentised reverse dependency
   solver. It takes a package to calculate the revdeps
   for and an opam repository commit. Note, it will actually
   do this via Docker/OBuilder, it would be nice to 0install
   for this. *)

module Key = struct
  type t = { opam_repository : Current_git.Commit.t; package : string }

  let to_yojson t =
    `Assoc
      [
        ("opam_repository", `String (Current_git.Commit.hash t.opam_repository));
        ("package", `String t.package);
      ]

  let digest t = Yojson.Safe.to_string (to_yojson t)
  let pp ppf t = Yojson.Safe.pretty_print ppf (to_yojson t)
end

module Value = struct
  type t = { revdeps : string list } [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    Yojson.Safe.from_string s |> of_yojson |> function
    | Ok v -> v
    | Error s -> Fmt.failwith "Failed to unmarshal revdeps %s" s
end

let id = "opam-revdeps"

type t = { pool : unit Current.Pool.t; level : Current.Level.t }

let spec opam =
  let open Obuilder_spec in
  let from = "ocaml/opam" in
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
    ]

let reveps_image opam =
  let open Current.Syntax in
  let dockerfile =
    let+ opam = opam in
    spec (Current_git.Commit.hash opam)
    |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false ~os:`Unix
    |> fun s -> `Contents s
  in
  Current_docker.Default.build ~pull:true ~dockerfile `No_context

let mangle_opam_url s =
  match Astring.String.cut ~sep:"+" s with
  | Some ("git", url) -> url
  | None | Some _ -> s

let js_maintainer = function "Jane Street developers" -> true | _ -> false

(** This is very slow probably :S *)
let opamfile opam package =
  Current_docker.Default.pread ~args:[ "opam"; "show"; "--raw"; package ] opam

let revdeps ?(disable_upstream = false) ~package opam =
  let open Current.Syntax in
  let image = reveps_image opam in
  let+ pkgs =
    Current_docker.Default.pread
      ~args:
        [
          "opam";
          "list";
          "--depends-on";
          package;
          "--column=package,dev-repo:,maintainer:";
          "--installable";
        ]
      image
  in
  String.split_on_char '\n' pkgs
  |> List.map (String.split_on_char '"')
  |> List.map (List.map String.trim)
  |> List.map (List.filter (fun v -> not String.(equal empty v)))
  |> List.filter_map (function
       | [ name; dev_repo ] -> Some (name, dev_repo |> mangle_opam_url, false)
       | [ name; dev_repo; maintainer ] ->
           Logs.info (fun f ->
               f "Checking maintainer field: %s %b" maintainer
                 (js_maintainer maintainer));
           Some (name, dev_repo |> mangle_opam_url, js_maintainer maintainer)
       | s ->
           Logs.info (fun f -> f "Not testing %a" Fmt.(list string) s);
           None)
  |> List.filter_map (fun (v, url, js) ->
         if js then
           let v =
             Current.component "opam-pkg: %s (janestreet)" v
             |>
             let> opamfile = opamfile image v in
             Current.Primitive.const
             @@ Repo_content.Extract.Key.Pkg (v, opamfile)
           in
           Some v
         else if disable_upstream then None
         else
           let v =
             let+ git =
               Current_git.clone ~schedule:(Current_cache.Schedule.v ()) url
             in
             Repo_content.Extract.Key.Git git
           in
           Some v)
  |> List.filteri (fun i _ -> i < 15)
