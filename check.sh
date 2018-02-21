#!/bin/sh

logdir=/tmp
switch=4.06.1

mkdir "$logdir/good"
mkdir "$logdir/bad"

cd $(mktemp -d)
echo 'FROM ocaml/opam2:debian-9-ocaml-4.05.0' >> Dockerfile
echo 'RUN sudo apt-get update' >> Dockerfile
echo 'RUN git pull origin master && opam update' >> Dockerfile
echo "RUN opam switch create -y $switch" >> Dockerfile
echo 'RUN opam install -y opam-depext' >> Dockerfile
docker build -t base-opam-check-all .

for pkg in $(opam list --installable --available --short); do
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
