#!/usr/bin/env bash

# for i in {01..25}; do ./gen.sh "${i}"; done; ls tmp/

DAY="${@}"

cat > "tmp/Day${DAY}.hs" <<EOF
module Day${DAY} (run) where
import Aoc

run :: FilePath -> IO Result
run inputFile = solve2 f f contents
  where
    contents = return ""
    f _ = "Not done"
EOF
