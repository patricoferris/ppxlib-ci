let src = Logs.Src.create "revdeps-ci.index" ~doc:"revdeps ci indexer"

module Log = (val Logs.src_log src : Logs.LOG)
module Db = Current.Db
module Job_map = Astring.String.Map

type job_ids = Current.job_id option Job_map.t

type t = {
  record_job : Sqlite3.stmt;
  remove : Sqlite3.stmt;
  list_packages : Sqlite3.stmt;
  list_rev_deps : Sqlite3.stmt;
  get_job : Sqlite3.stmt;
  get_job_ids : Sqlite3.stmt;
}

type job_state =
  [ `Not_started | `Active | `Failed of string | `Passed | `Aborted ]
[@@deriving show]

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let is_valid_hash hash =
  let open Astring in
  String.length hash >= 6 && String.for_all Char.Ascii.is_alphanum hash

let get_short_hash = Astring.String.with_range ~len:6

let db =
  lazy
    (let db = Lazy.force Current.Db.v in
     Current_cache.Db.init ();
     Sqlite3.exec db
       {|
CREATE TABLE IF NOT EXISTS revdeps_ci_index (
  package  TEXT NOT NULL,
  revdep   TEXT NOT NULL,
  job_id   TEXT,
  PRIMARY KEY (package, revdep)
)|}
     |> or_fail "create table";
     let record_job =
       Sqlite3.prepare db
         "INSERT OR REPLACE INTO revdeps_ci_index (package, revdep, job_id) \
          VALUES (?, ?, ?)"
     in
     let remove =
       Sqlite3.prepare db
         "DELETE FROM revdeps_ci_index WHERE package = ? AND revdep = ?"
     in
     let list_packages =
       Sqlite3.prepare db "SELECT DISTINCT package FROM revdeps_ci_index"
     in
     let list_rev_deps =
       Sqlite3.prepare db
         "SELECT DISTINCT revdep, job_id FROM revdeps_ci_index WHERE package = \
          ?"
     in
     let get_job =
       Sqlite3.prepare db
         "SELECT job_id FROM revdeps_ci_index WHERE package = ? AND revdep = ?"
     in
     let get_job_ids =
       Sqlite3.prepare db
         "SELECT job_id FROM revdeps_ci_index WHERE package = ? AND revdep = ?"
     in
     { record_job; list_packages; list_rev_deps; get_job; remove; get_job_ids })

module Git = Current_git

type current_git_commit_id = Git.Commit_id.t

let current_git_commit_id_to_yojson id =
  `Assoc
    [
      ("repo", `String (Git.Commit_id.repo id));
      ("gref", `String (Git.Commit_id.gref id));
      ("hash", `String (Git.Commit_id.hash id));
    ]

let current_git_commit_id_of_yojson = function
  | `Assoc
      [ ("repo", `String repo); ("gref", `String gref); ("hash", `String hash) ]
    ->
      Ok (Git.Commit_id.v ~repo ~gref ~hash)
  | _ -> Fmt.failwith "Failed to deserialise commit_id"

let package_to_string p =
  Yojson.Safe.to_string (current_git_commit_id_to_yojson p)

let package_of_string s =
  Yojson.Safe.from_string s |> current_git_commit_id_of_yojson |> function
  | Ok v -> v
  | Error s -> Fmt.failwith "Failed to deserialise package: %s" s

type revdep = Repo_content.Extract.Key.t =
  | Git of Git.Commit.t
  | Pkg of string * string

let revdep_to_string p =
  Yojson.Safe.to_string (Repo_content.Extract.Key.to_yojson p)

let revdep_of_string s =
  Yojson.Safe.from_string s |> Repo_content.Extract.Key.of_yojson |> function
  | Ok v -> v
  | Error s -> Fmt.failwith "Failed to deserialise package: %s" s

let package_revdep_key ~package ~revdep =
  let p = current_git_commit_id_to_yojson package in
  let r = Repo_content.Extract.Key.to_yojson revdep in
  `Assoc [ ("package", p); ("revdep", r) ] |> Yojson.Safe.to_string

let get_job_ids_with_variant t ~package ~revdep =
  let key = package_revdep_key ~package ~revdep in
  Db.query t.get_job_ids
    Sqlite3.Data.
      [ TEXT (package_to_string package); TEXT (revdep_to_string revdep) ]
  |> List.fold_left
       (fun acc -> function
         | Sqlite3.Data.[ NULL ] -> Job_map.add key None acc
         | Sqlite3.Data.[ TEXT id ] -> Job_map.add key (Some id) acc
         | row -> Fmt.failwith "get_job_ids: invalid row %a" Db.dump_row row)
       Job_map.empty

let init () = ignore (Lazy.force db)

module Status_cache = struct
  let cache = Hashtbl.create 1_000
  let cache_max_size = 1_000_000

  type elt =
    [ `Not_started | `Pending | `Failed of string | `Passed | `Analysing ]

  let add ~package ~revdep (status : elt) =
    if Hashtbl.length cache > cache_max_size then Hashtbl.clear cache;
    Hashtbl.add cache (package, revdep) status

  let find ~package ~revdep : elt =
    Hashtbl.find_opt cache (package, revdep) |> function
    | Some s -> s
    | None -> `Not_started
end

type n_per_status_t = {
  not_started : int;
  pending : int;
  failed : int;
  passed : int;
}

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

let list_packages () =
  let t = Lazy.force db in
  let packages = Db.query t.list_packages [] in
  List.map
    (function
      | [ Sqlite3.Data.TEXT t ] -> t | _ -> failwith "Malformed database")
    packages
  |> List.map package_of_string

let list_rev_deps package =
  let t = Lazy.force db in
  let revdeps =
    Db.query t.list_rev_deps [ Sqlite3.Data.TEXT (package_to_string package) ]
  in
  List.map
    (function
      | [ Sqlite3.Data.TEXT t; NULL ] -> (t, None)
      | [ Sqlite3.Data.TEXT t; TEXT v ] -> (t, Some v)
      | _ -> failwith "Malformed database")
    revdeps
  |> List.map (fun (v, id) ->
         Logs.info (fun f ->
             f "Package: %s Revdep: %s: " (package_to_string package) v);
         let revdep = revdep_of_string v in
         (revdep, id, Status_cache.find ~package ~revdep))

let record ~package ~revdep ~status jobs =
  let t = Lazy.force db in
  let previous = get_job_ids_with_variant t ~package ~revdep in
  let () = Status_cache.add ~package ~revdep status in
  let package = package_to_string package in
  let revdep = revdep_to_string revdep in
  let merge variant prev job =
    let set job_id =
      Log.info (fun f ->
          f "@[<h>Index.record %s/%s %s -> %a@]" package revdep variant
            Fmt.(option ~none:(any "-") string)
            job_id);
      match job_id with
      | None ->
          Db.exec t.record_job Sqlite3.Data.[ TEXT package; TEXT revdep; NULL ]
      | Some id ->
          Db.exec t.record_job
            Sqlite3.Data.[ TEXT package; TEXT revdep; TEXT id ]
    in
    let update j1 j2 =
      match (j1, j2) with
      | Some j1, Some j2 when j1 = j2 -> ()
      | None, None -> ()
      | _, j2 -> set j2
    in
    let remove () =
      Log.info (fun f -> f "@[<h>Index.record %s/%s REMOVED@]" package revdep);
      Db.exec t.remove Sqlite3.Data.[ TEXT package; TEXT revdep ]
    in
    (match (prev, job) with
    | Some j1, Some j2 -> update j1 j2
    | None, Some j2 -> set j2
    | Some _, None -> remove ()
    | None, None -> assert false);
    None
  in
  let _ : [ `Empty ] Job_map.t = Job_map.merge merge previous jobs in
  ()

let get_job ~package ~revdep =
  let t = Lazy.force db in
  let package = package_to_string package in
  let revdep = revdep_to_string revdep in
  Log.info (fun f -> f "@[<h>Index.get_job %s %s @]" package revdep);
  match Db.query_some t.get_job Sqlite3.Data.[ TEXT package; TEXT revdep ] with
  | None -> Error `No_such_variant
  | Some Sqlite3.Data.[ TEXT id ] -> Ok (Some id)
  | Some Sqlite3.Data.[ NULL ] -> Ok None
  | _ -> failwith "get_job: invalid result!"
