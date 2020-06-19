#! /usr/bin/env sh
set -o xtrace
exec hlint lint --hint tools/hlint.yaml src
