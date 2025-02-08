(lang dune 3.17)

; Change to false once in a while to see what we are really using
; Too fragile to turn off by default though
(implicit_transitive_deps true)

(generate_opam_files true)

(license MIT)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Kate <kit-ty-kate@outlook.com>")
(source (github ocurrent/opam-health-check))

(package
 (name opam-health-check)
 (synopsis "A toolchain to check for broken opam packages")
 (tags (opam check))
 (depends
   (ocaml (>= 4.10.0))
   (tyxml (>= 4.3.0))
   cohttp
   cohttp-lwt
   cohttp-lwt-unix
   (conduit-lwt-unix (>= 6.2.0))
   (http-lwt-client (>= 0.2.0))
   (containers (>= 3.4))
   (opam-core (>= 2.2))
   (opam-format (>= 2.2))
   (mirage-crypto-pk (>= 1.2.0))
   (mirage-crypto-rng (>= 1.2.0))
   (mirage-crypto-rng-lwt (>= 1.2.0))
   (cmdliner (>= 1.1.0))
   (prometheus-app (>= 1.2))
   fpath
   (fmt (>= 0.8.7))
   (re (>= 1.7.2))
   (yaml (>= 2.0.0))
   (xdg-basedir (>= 0.0.4))
   (obuilder-spec (>= 0.5))
   (ocluster-api (>= 0.1))
   (current_ansi (>= 0.1))
   (yojson (>= 1.6.0))
   github
   github-unix
   github-data
   cstruct
   (capnp-rpc (>= 1.2))
   (capnp-rpc-lwt (>= 1.2))
   (capnp-rpc-unix (and (>= 1.2) (< 2.0)))
   (lwt (>= 5.3.0))
   uri
   (x509 (>= 1.0.0))
   (docker_hub (and (>= 0.1.0) (< 0.2.0)))
   (memtrace (>= 0.2.3))
   (tls-lwt (>= 0.16.0)) ; activate conduit with TLS for slack webhooks
   conf-libev ; Required for lwt to avoid hitting the limits of select(2)
   conf-pixz ; TODO: Make it a library
   ; TODO: Add conf-ugrep
  )
 (conflicts
  (dns-client-lwt (< 6.1.4)) ; Required to fix https://github.com/roburio/http-lwt-client/issues/8
  (happy-eyeballs-lwt (< 0.1.3)) ; Required to fix https://github.com/roburio/http-lwt-client/issues/8
  )
)
