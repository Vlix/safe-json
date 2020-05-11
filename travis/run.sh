#!/usr/bin/env bash
stack $ARGS --no-terminal test --ghc-options="-Wall -Wredundant-constraints -Wpartial-fields -Wincomplete-uni-patterns -Wincomplete-record-updates" safe-json
