#!/usr/bin/env bash

stack --no-terminal test --ghc-options="-Wall -Wredundant-constraints -Wpartial-fields -Wincomplete-uni-patterns -Wincomplete-record-updates" --fast safe-json
