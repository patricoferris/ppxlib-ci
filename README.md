ppxlib-ci
---------

A CI system for testing `ppxlib` and its reverse dependencies.

The core of the CI could be refactored to test any package and its reverse dependencies.

The CI requires a configuration file to run that explains how to test packages
along with the packages to test.

```sh
$ cat docs/config.yaml | ppxlib-ci check-config
ppxlibs:
  main: ocaml-ppx/ppxlib#main
  extras:
  - patricoferris/ppxlib#refactor-driver-transformer
ppxes:
- janestreet/ppx_sexp_conv#master
- ocaml-ppx/ppx_deriving#master
- url: https://github.com/patricoferris/ppx_deriving_yaml.git
  extras: true
  branches:
  - main
  - ppxlib-ci
```
