#!/usr/bin/env bash

set -euxo pipefail

exec stack --no-terminal test --pedantic --fast safe-json
