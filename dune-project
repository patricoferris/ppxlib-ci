(lang dune 3.14)

(generate_opam_files true)

(using mdx 0.4)

(package
  (name ppxlib-ci)
  (depends
    ppx_deriving_yaml
    current
    current_git
    current_web
    current_docker
    current_github
    solver-service
    solver-worker
    (mdx :with-test)))
