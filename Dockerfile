FROM ocaml/opam:debian-12-ocaml-4.14@sha256:f2058e217865907102f49886543f7c92952ec990ddfb1ee9c5e9d82e55457778 AS build
RUN sudo apt-get update
RUN sudo ln -f /usr/bin/opam-2.2 /usr/bin/opam && opam init --reinit -n
RUN cd ~/opam-repository && git fetch origin master && git reset --hard e2e8f58bc257422abfd1132c3363e5b7ed2226db && opam update
COPY --chown=opam opam-health-check.opam /src/
RUN opam install --deps-only /src/
COPY --chown=opam . /src/
WORKDIR /src/
RUN opam exec -- dune build --release @install

FROM debian:12
RUN apt-get update && apt-get install netbase dumb-init libev-dev ca-certificates xz-utils tar pixz ugrep -y --no-install-recommends
WORKDIR /var/lib/opam-health-check
ENTRYPOINT ["dumb-init", "/usr/local/bin/opam-health-serve"]
ENV OCAMLRUNPARAM=b
COPY --from=build /src/_build/install/default/bin/opam-health-serve /usr/local/bin/
COPY --from=build /src/_build/install/default/bin/opam-health-check /usr/local/bin/
