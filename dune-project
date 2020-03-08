(lang dune 2.3.1)
(name lambda_streams)

(generate_opam_files true)

(version 0.1.0)
(source (github Risto-Stevcev/lambda-streams))
(license BSD-3-Clause)
(authors "Risto Stevcev")
(maintainers "me@risto.codes")

(package
 (name lambda_streams)
 (synopsis "Lambda-based streaming library")
 (description "A simple and principled streaming library based on lambdas")
 (depends
  (ppx_inline_test (and (>= 0.13.1) (< 0.14)))
  (dune (and (>= 2.3.1) (< 3.0)))))
