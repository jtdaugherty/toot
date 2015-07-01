#!/usr/bin/env bash

set -e

brew install icu4c
DYLD_LIBRARY_PATH=/usr/local/opt/icu4c/lib cabal install text-icu \
    --extra-include-dirs=/usr/local/opt/icu4c/include \
    --extra-lib-dirs=/usr/local/opt/icu4c/lib
