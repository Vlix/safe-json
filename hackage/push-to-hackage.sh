#!/bin/bash
set -e

USAGE="Usage: $0 [-d <build path>] [-hsxcp] [--help]"

if [[ "$1" == "--help" ]]; then
  echo "$USAGE"
  echo "<This script requires 'cabal' and 'stack' to be available>"
  echo
  echo " -d   path to build to (default: uses temporary directory)"
  echo " -n   do NOT publish to hackage       (default)"
  echo " -s   publish to hackage"
  echo " -c   set to publish as candidate     (default)"
  echo " -p   publish as PERMANENT"
  echo " -h   build (and send) haddock documentation"
  echo " -x   do NOT build/test with stack"
  echo
  exit 0
fi

HADDOCK=""
COMMAND="v2-sdist"
PUBLISH=""
SEND=""
STACK="true"
BUILDDIR=""

while getopts "hxscpnd:" opt; do
  case "$opt" in
    d) BUILDDIR=${OPTARG%\/};;
    h)
      HADDOCK="docs"
      COMMAND="v2-haddock"
      ;;
    s) SEND="1";;
    n) SEND="";;
    c) PUBLISH="";;
    p) PUBLISH="--publish";;
    x) STACK="";;
    [?])
      echo "$USAGE"
      exit 1
      ;;
  esac
done

FILE=$(ls --format="single-column" | grep .cabal)

if [[ -e "$FILE" ]]; then
  echo "Building library using: $FILE"
  sleep 1
else
  echo "Run $0 in the root folder (where the cabal file is)"
  exit 1
fi

if cabal --version 1>/dev/null; then
  if [[ "$STACK" == "true" ]]; then
    if stack --version; then
      if stack test --fast --ghc-options="-j -Wall"; then
        :
      else
        echo "Failed build or test(s) for: $FILE"
      fi
    else
      echo "'stack' not found, exiting"
    fi
  fi
else
  echo "'cabal' not found, exiting"
  exit 1
fi

# ---------- Building package ----------

TMPDIR=$(mktemp -dt "cabal$HADDOCK-XXXXXXXXXXXX")
trap 'rm -r "$TMPDIR"' EXIT

if [[ -z "$BUILDDIR" ]]; then
  BUILDDIR="$TMPDIR"
fi

if [[ -n "$HADDOCK" ]]; then
  HADDOCKOPTS="--enable-documentation --haddock-for-hackage"
  HADDOCKCOMMENT=" documentation"
  HADDOCKUPLOAD="-d"
else
  HADDOCKOPTS=""
  HADDOCKCOMMENT=""
  HADDOCKUPLOAD=""
fi

echo "Building the following$HADDOCKCOMMENT into tarball"

if cabal v2-sdist -l; then
  if cabal "$COMMAND" $HADDOCKOPTS --builddir="$BUILDDIR"; then
    :
  else
    echo "Failed building tarball for: $FILE"
    exit 1
  fi
else
  echo "Couldn't build tarball for: $FILE"
  exit 1
fi

if [[ -z "$PUBLISH" ]]; then
  CANDIDATE="candidate"
else
  CANDIDATE="PERMANENT"
fi

if [[ -n "$SEND" ]]; then
  echo "Sending $CANDIDATE${HADDOCKCOMMENT:- library} to hackage"

  # ---------- USERNAME / PASSWORD ----------

  HACKAGEUSERNAME=""
  HACKAGEPASSWORD=""
  if read -rp "Hackage username: " "HACKAGEUSERNAME"; then
    if read -srp "Hackage password: " "HACKAGEPASSWORD"; then
      :
    else
      echo "Failed processing password"
      exit 1
    fi
  else
    echo "Failed processing username"
    exit 1
  fi

  if [[ -z "$HADDOCK" ]]; then
    BUILDDIR="$BUILDDIR/sdist"
  fi

  echo
  echo "Listing everything in $BUILDDIR"
  ls -la "$BUILDDIR"
  TARFILE=$(ls --format="single-column" "$BUILDDIR" | grep .tar.gz | head -1)
  echo "TARFILE: $TARFILE"

  if [[ -z "$TARFILE" ]]; then
    echo "could not locate .tar.gz file"
    exit 1
  fi

  if cabal upload $PUBLISH $HADDOCKUPLOAD -u "$HACKAGEUSERNAME" -p "$HACKAGEPASSWORD" "$BUILDDIR/$TARFILE"; then
    echo "Succesfully sent $CANDIDATE${HADDOCKCOMMENT:- library} to hackage"
    exit 0
  else
    echo "Something went wrong sending $CANDIDATE${HADDOCKCOMMENT:- library} to hackage"
    exit 1
  fi
else
  echo "Done building${HADDOCKCOMMENT:- library} for: $FILE"
fi


