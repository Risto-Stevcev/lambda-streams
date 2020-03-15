(lang dune 2.4.0)
(name lambda_streams)
(version 0.1.0)
(source (github Risto-Stevcev/lambda-streams))
(license BSD-3-Clause)
(authors "Risto Stevcev")
(maintainers "me@risto.codes")
(generate_opam_files true)
(implicit_transitive_deps false)
(using mdx 0.1)

(package
 (name lambda_streams)
 (synopsis "Lambda-based streaming library")
 (description "A simple and principled streaming library based on lambdas")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (and (>= 4.06.1) (<= 4.10.0)))
  (alcotest (and (= 1.0.1) :with-test))
  (alcotest-lwt (and (= 1.0.1) :with-test))
  (alcotest-async (and (= 1.0.1) :with-test))
  (fmt (and (= 0.8.8) :with-test))
  (odoc (and (= 1.5.0) :with-doc))
  (mdx (= 1.6.0))
  (dune (= 2.4.0))))

(package
 (name lambda_streams_lwt)
 (synopsis "Lwt helpers for lambda_streams")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (and (>= 4.06.1) (<= 4.10.0)))
  (lwt (= 5.2.0))
  (lambda_streams (= :version))
  (dune (= 2.4.0))))

(package
 (name lambda_streams_async)
 (synopsis "Async helpers for lambda_streams")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (and (>= 4.06.1) (<= 4.10.0)))
  (async (= v0.13.0))
  (async_kernel (= v0.13.0))
  (async_unix (= v0.13.0))
  (core (= v0.13.0))
  (lambda_streams (= :version))
  (dune (= 2.4.0))))
