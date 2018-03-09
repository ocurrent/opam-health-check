#!/bin/sh

logdir=/tmp/logs
pkgs="llopt.1.0.0"
repo=git://github.com/jpdeplaix/opam-repository.git
branch=llopt.1.0.0
distro=debian-unstable
compilers="4.01.0 4.02.3 4.03.0 4.04.2 4.05.0 4.06.0"

for version in ${compilers}; do
    mkdir "${logdir}/${version}"
    cd "${logdir}/${version}"
    echo "FROM ocaml/opam:${distro}_ocaml-${version}" >> Dockerfile
    echo 'WORKDIR /home/opam/opam-repository' >> Dockerfile
    echo 'RUN git pull origin master' >> Dockerfile
    echo "RUN git pull '${repo}' '${branch}'" >> Dockerfile
    echo 'RUN opam update' >> Dockerfile
    echo "RUN opam depext -ui ${pkgs}" >> Dockerfile
    docker build --rm --force-rm . > log
done
