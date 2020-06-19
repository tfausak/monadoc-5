#! /usr/bin/env sh
set -o xtrace
exec find src -name '*.hs' -exec \
  stylish-haskell --config tools/stylish-haskell.yaml --inplace {} +
