#!/bin/sh

logdir=/tmp/llvm-logs
pkg=cohttp.1.1.0
repo=git://github.com/ocaml/opam-repository.git
branch=master
distro=debian-unstable
version=4.05.0

mkdir -p "${logdir}/${version}"
cd "${logdir}/${version}"

echo "FROM ocaml/opam2:${distro}-ocaml-${version}" > Dockerfile
echo 'WORKDIR /home/opam/opam-repository' >> Dockerfile
echo 'RUN git pull origin master' >> Dockerfile
echo "RUN git pull '${repo}' '${branch}'" >> Dockerfile
echo 'RUN opam update' >> Dockerfile
echo 'RUN opam install -y opam-depext' >> Dockerfile
echo 'RUN sudo apt-get update' >> Dockerfile
echo "RUN opam pin add -yn \$(echo '${pkg}' | sed 's/\\./ /')" >> Dockerfile
echo "RUN opam depext -yi ${pkg}" >> Dockerfile
echo "RUN opam list --depends-on ${pkg} --installable --available --all-versions --short > revdeps" >> Dockerfile
echo 'RUN for pkg in $(cat revdeps); do opam depext -yi $pkg; done' >> Dockerfile

echo "Checking revdeps using OCaml ${version}..."
docker build . > revdeps-log
