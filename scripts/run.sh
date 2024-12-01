#!/usr/bin/env bash

(return 0 2>/dev/null) && SOURCED=1 || SOURCED=0

if [ "${SOURCED}" = 1 ]; then
    alias aoc="cabal run exes --"
else
    cabal run exes -- "${@}"
fi
