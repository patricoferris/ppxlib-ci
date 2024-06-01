open Astring

(* The different ppxlibs to run. The main
   branch and any others you might want. *)
type ppxlibs = { main : string; extras : string list } [@@deriving yaml]

(* A ppx can either be a record a simple
   Github [org/repo#branch] string, hence the custom
   yaml parsing. *)
type ppx = {
  url : string;
  extras : bool; [@default false]
  branches : string list;
}
[@@deriving yaml]

let branch_of_url url =
  match Astring.String.cut ~sep:"#" url with
  | Some (v, branch) -> (v, branch)
  | None -> (url, "main")

let ppx_of_yaml = function
  | `String s -> (
      match Astring.String.cut ~sep:"/" s with
      | Some _ ->
          let org_repo, br = branch_of_url s in
          let url = Fmt.str "https://github.com/%s.git" org_repo in
          Ok { url; extras = false; branches = [ br ] }
      | None ->
          Fmt.error_msg
            "Expected a Github URL like `ocaml-ppx/ppxlib#main` but got %s" s)
  | yaml -> ppx_of_yaml yaml

let ppx_to_yaml s =
  match (String.find_sub ~sub:"github" s.url, List.length s.branches) with
  | Some start, (0 | 1) -> (
      match
        String.cuts ~sep:"/" (String.sub ~start s.url |> String.Sub.to_string)
      with
      | _com :: org :: repo :: _ -> (
          match s.branches with
          | [] -> `String (Fmt.str "%s/%s" org repo)
          | [ br ] -> `String (Fmt.str "%s/%s#%s" org repo br)
          | _ -> assert false)
      | _ -> failwith "Failed to reconstruct Github URL for ppx")
  | _ -> ppx_to_yaml s

type t = { ppxlibs : ppxlibs; ppxes : ppx list } [@@deriving yaml]
