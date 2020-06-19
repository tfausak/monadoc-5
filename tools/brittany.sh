#! /usr/bin/env sh
set -o xtrace
exec find src -name '*.hs' -exec \
  brittany --config-file tools/brittany.yaml --write-mode inplace {} +
