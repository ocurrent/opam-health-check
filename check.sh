#!/bin/sh

logdir=/tmp/logs
switch=4.06.1

mkdir -p "$logdir/good"
mkdir -p "$logdir/bad"
touch "$logdir/pkgs"

cd $(mktemp -d)
echo 'FROM ocaml/opam2:debian-9-ocaml-4.05.0' >> Dockerfile
echo 'RUN sudo apt-get update' >> Dockerfile
echo 'RUN git pull origin master && opam update' >> Dockerfile
echo "RUN opam switch create -y $switch" >> Dockerfile
echo 'RUN opam install -y opam-depext' >> Dockerfile
echo 'CMD sudo chown opam:opam /logs && opam list --installable --available --short > /logs/pkgs' >> Dockerfile
docker build -t base-opam-check-all .
docker run -v "$logdir:/logs" base-opam-check-all

for pkg in $(cat "$logdir/pkgs"); do
    cd $(mktemp -d)
    echo 'FROM base-opam-check-all' >> Dockerfile
    echo "RUN opam depext -iyv $pkg" >> Dockerfile
    if docker build --rm --force-rm . > "$logdir/$pkg"; then
        mv "$logdir/$pkg" "$logdir/good/$pkg"
    else
        mv "$logdir/$pkg" "$logdir/bad/$pkg"
    fi
done

docker rm base-opam-check-all
