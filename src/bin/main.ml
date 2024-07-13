let program_name = "revdeps-ci"

let to_string_err = function
  | Ok v -> Ok v
  | Error (`Msg s) -> Error ("Failed: " ^ s)

let check_config () =
  let config = In_channel.input_all In_channel.stdin in
  let yaml = Yaml.of_string_exn config in
  Ppxlib_ci.Config.of_yaml yaml
  |> Result.map (fun _ -> Fmt.pr "%s%!" (Yaml.to_string_exn yaml))
  |> to_string_err

let routes ~engine =
  let open Routes in
  [
    route nil Web.homepage;
    route
      (s "package" / str / str /? nil)
      (fun owner repo -> Web.package ~owner ~repo);
    route (s "revdeps" /? nil) Web.revdeps;
  ]
  @ Current_web.routes engine

let main () config mode ppxlib_ci_config =
  let conf = In_channel.with_open_bin ppxlib_ci_config In_channel.input_all in
  let yaml = Yaml.of_string_exn conf in
  let ppx_conf = Ppxlib_ci.Config.of_yaml yaml |> Result.get_ok in
  let engine =
    Current.Engine.create ~config (fun () -> Ppxlib_ci.pipeline ppx_conf)
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all) ~name:program_name (routes ~engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])
  |> to_string_err

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let ppx_conf =
  Arg.required
  @@ Arg.pos 0 Arg.(some file) None
  @@ Arg.info ~doc:"The path to the ppxlib-ci config" ~docv:"CONFIG" []

let check_config =
  let doc = "Try to parse the CI's config file." in
  let info = Cmd.info "check-config" ~doc in
  Cmd.v info Term.(const check_config $ setup_log)

let main =
  let doc = "Run the CI." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(
      const main $ setup_log $ Current.Config.cmdliner $ Current_web.cmdliner
      $ ppx_conf)

let cmds = [ main; check_config ]

let () =
  let doc = "a command line for the ppxlib CI system" in
  let info = Cmd.info ~doc "ppxlib-ci" in
  exit (Cmd.eval_result @@ Cmd.group info cmds)
