#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal new-build --enable-documentation --haddock-for-hackage --docdir="$dir"
# cabal new-haddock --builddir="$dir" --haddock-for-hackage
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload -d $dir/*-docs.tar.gz $1
