#!/usr/bin/env bash

echo "Quick suite starting..."

for file in *; do
    if [[ -f $file && $file == *.hs ]]; then
        # For each file, run the following
        stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A1m -N &>> "${file}.16.3.a1m.g2"
	echo "${file} done"
    fi
done
