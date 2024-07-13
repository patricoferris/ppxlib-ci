open Htmlit
module Git = Current_git

let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _x :: rest -> last_two rest

let commit_to_owner_name c =
  match Astring.String.cuts ~sep:"/" (Git.Commit_id.repo c) |> last_two with
  | Some (repo, owner) -> (repo, owner)
  | None -> ("unknown", "unknown")

let git_commit_to_element git =
  El.li
    [
      El.a
        ~at:
          [
            At.href
              (Fmt.str "/package/%a"
                 Fmt.(pair ~sep:(Fmt.any "/") string string)
                 (commit_to_owner_name git));
          ]
        [ El.txt (Git.Commit_id.repo git) ];
    ]

let distinct (p : 'a -> 'b) ps =
  List.fold_left
    (fun (cs, vs) v ->
      let t = p v in
      if List.mem t cs then (cs, vs) else (t :: cs, v :: vs))
    ([], []) ps
  |> snd

let header breadcrumbs =
  El.splice
    [
      El.h2 [ El.txt "Revdeps CI" ];
      El.p ~at:[ At.style "font-style: italic" ] breadcrumbs;
      El.p [ El.a ~at:[ At.href "/jobs" ] [ El.txt "jobs" ] ];
      El.hr ();
    ]

let text_ref ~href txt = El.a ~at:[ At.href href ] [ El.txt txt ]

let homepage =
  object
    inherit Current_web.Resource.t

    method! private get site =
      (* Reroute for graph *)
      match
        Current_web.Context.request site |> Cohttp.Request.uri |> Uri.query
      with
      | _ :: _ as q ->
          let uri = Uri.with_query (Uri.of_string "/index.html") q in
          Cohttp_lwt_unix.Server.respond_redirect ~uri ()
      | [] ->
          let packages = Ppxlib_ci.Index.list_packages () in
          (* Filter down to disinct repos *)
          let packages = distinct Git.Commit_id.repo packages in
          let page =
            El.div
              [
                header [];
                El.p
                  [
                    El.txt
                      "The reverse dependency CI is a tool for checking \
                       packages that might depend on your package.";
                  ];
                El.p
                  [
                    El.txt
                      (Fmt.str "There are %i registered packages for this CI."
                         (List.length packages));
                  ];
                El.ol (List.map git_commit_to_element packages);
              ]
          in
          let html =
            Template.template "revdeps-ci" page |> El.to_string ~doctype:true
          in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:html ()
  end

let package ~owner ~repo =
  object
    inherit Current_web.Resource.t

    method! private get _site =
      let packages = Ppxlib_ci.Index.list_packages () in
      (* Filter down to disinct repos *)
      let packages =
        List.filter (fun v -> commit_to_owner_name v = (owner, repo)) packages
      in
      let package_revdeps_url package =
        Uri.of_string "/revdeps" |> fun v ->
        Uri.add_query_params v
          [
            ("repo", [ Uri.pct_encode @@ Git.Commit_id.repo package ]);
            ("gref", [ Uri.pct_encode @@ Git.Commit_id.gref package ]);
            ("hash", [ Uri.pct_encode @@ Git.Commit_id.hash package ]);
          ]
        |> Uri.to_string
      in
      let page =
        El.div
          [
            header [ text_ref ~href:"/" "home" ];
            El.p
              [ El.txt (Fmt.str "You are viewing builds for %s/%s" owner repo) ];
            El.ul
              (List.map
                 (fun v ->
                   El.li
                     [
                       El.a
                         ~at:[ At.href (package_revdeps_url v) ]
                         [ El.txt (Git.Commit_id.gref v) ];
                       El.span
                         ~at:[ At.style "color: grey" ]
                         [ El.txt (Fmt.str " (%s)" @@ Git.Commit_id.hash v) ];
                     ])
                 packages);
          ]
      in
      let html =
        Template.template "revdeps-ci" page |> El.to_string ~doctype:true
      in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:html ()
  end

module Revdeps_map = Map.Make (String)

let status_to_element : Ppxlib_ci.Index.Status_cache.elt -> El.html = function
  | `Passed -> El.span ~at:[ At.style "color: green" ] [ El.txt "passed" ]
  | `Failed s ->
      El.span ~at:[ At.style "color: red" ] [ El.txt "failure: "; El.txt s ]
  | `Pending -> El.span ~at:[ At.style "color: orange" ] [ El.txt "running" ]
  | `Not_started -> El.span ~at:[ At.style "color: grey" ] [ El.txt "waiting" ]
  | `Analysing ->
      El.span ~at:[ At.style "color: yellow" ] [ El.txt "analysing" ]

let revdeps =
  object
    inherit Current_web.Resource.t

    method! private get site =
      let uri = Current_web.Context.request site |> Cohttp.Request.uri in
      let params = Uri.query uri in
      let repo = List.assoc "repo" params |> List.hd |> Uri.pct_decode in
      let gref = List.assoc "gref" params |> List.hd |> Uri.pct_decode in
      let hash = List.assoc "hash" params |> List.hd |> Uri.pct_decode in
      let package = Current_git.Commit_id.v ~repo ~gref ~hash in
      Logs.info (fun f -> f "Digest: %s" (Git.Commit_id.digest package));
      let revdeps = Ppxlib_ci.Index.list_rev_deps package in
      let add_to_map (up, op) builds k (v, id, status) = match v with
        | Ppxlib_ci.Index.Git _ -> (Revdeps_map.add k ((v, id, status) :: builds) up, op)
        | Pkg _ -> (up, Revdeps_map.add k ((v, id, status) :: builds) op)
      in
      let upstream_map, opam_map =
        List.fold_left
          (fun ((up, op) as acc) (v, id, status) ->
            let v_string = Ppxlib_ci.Index.revdep_to_string v in
            match Revdeps_map.find_opt v_string up, Revdeps_map.find_opt v_string op with
            | Some builds, None -> add_to_map acc builds v_string (v, id, status)
            | None, Some builds -> add_to_map acc builds v_string (v, id, status)
            | None, None -> add_to_map acc [] v_string (v, id, status)
            | _ -> 
                Logs.warn (fun f -> f "Package identifical in upstream and opam");
                (up, op))
          (Revdeps_map.empty, Revdeps_map.empty) revdeps
      in
      let owner, repo = commit_to_owner_name package in
      Logs.info (fun f -> f "Revdeps CI: %s/%s" owner repo);
      let page rest =
        El.div
          [
            header
              [
                text_ref ~href:"/" "home";
                El.txt " > ";
                text_ref
                  ~href:(Fmt.str "/package/%s/%s" owner repo)
                  (Fmt.str "%s/%s" owner repo);
                El.txt " > ";
                El.txt gref;
              ];
            El.p
              [
                El.txt
                  (Fmt.str
                     "You are viewing %i reverse dependency builds for %s/%s"
                     (List.length revdeps) owner repo);
              ];
              rest
          ]   
      in
      let render_map title map = [
            El.h4 [ El.txt title ];
            El.hr();
            El.ul
              (List.map
                 (fun (_repo, builds) ->
                   let owner, repo =
                     match List.hd builds |> fun (v, _, _) -> v with
                     | Ppxlib_ci.Index.Git git ->
                         commit_to_owner_name (Git.Commit.id git)
                     | Pkg (pkg, _) -> (pkg, ":opam-repository")
                   in
                   El.li
                     [
                       El.txt (Fmt.str "%s/%s" owner repo); 
                       El.ul
                         (List.map
                            (fun (v, id, status) ->
                              match v with
                              | Ppxlib_ci.Index.Pkg (pkg, _) ->
                                  El.li [ El.txt pkg ]
                              | Ppxlib_ci.Index.Git git ->
                                  let v = Git.Commit.id git in
                                  El.li
                                    [
                                      (match id with
                                      | Some id ->
                                          text_ref ~href:(Fmt.str "job/%s" id)
                                            (Git.Commit_id.gref v)
                                      | None -> El.txt (Git.Commit_id.gref v));
                                      El.nbsp;
                                      status_to_element status;
                                      El.span
                                        ~at:[ At.style "color: grey" ]
                                        [
                                          El.txt
                                            (Fmt.str "   (%s)"
                                               (Git.Commit_id.hash v));
                                        ];
                                    ])
                            builds);
                     ])
                 (Revdeps_map.to_list map))
            ] in
      let ups = render_map "Upstream" upstream_map in
      let opam = render_map "Opam" opam_map in
      let html =
        Template.template "revdeps-ci" (page (El.div (ups @ opam))) |> El.to_string ~doctype:true
      in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:html ()
  end
