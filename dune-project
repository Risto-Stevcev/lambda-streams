(lang dune 2.3.1)
(name lambda_streams)
(version 0.1.0)
(source (github Risto-Stevcev/lambda-streams))
(license BSD-3-Clause)
(authors "Risto Stevcev")
(maintainers "me@risto.codes")
(generate_opam_files true)
(implicit_transitive_deps false)

(package
 (name lambda_streams)
 (synopsis "Lambda-based streaming library")
 (description "A simple and principled streaming library based on lambdas")
 (depends
  (alcotest (and (= 1.0.1) :with-test))
  (fmt (and (= 0.8.8) :with-test))
  (dune (and (>= 2.3.1) (< 3.0)))))
