#!/bin/bash
# Generate document using ocamldoc

# エラーが起きた時は終了するようにする
set -eu

cd "`dirname "$0"`"'/..' # `cd` to the project root directory

current_branch=`git symbolic-ref --short HEAD`
echo "currently at branch '"$current_branch"'"

git checkout gh-pages
git pull --no-edit origin main

opam exec -- dune build @doc
opam exec -- dune build @doc-private

mkdir -p docs/ocamldoc
cp -r -f _build/default/_doc/_html/* docs/ocamldoc/

echo "generated documents"

# そもそも更新したものがない場合は add や commit は失敗するので，
# 一旦エラーが起きても継続するようにする
set +eu
git add --all
git commit -m "updated documents"
set -eu

git push

git checkout $current_branch

echo "done"

