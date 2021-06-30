#!/usr/bin/env bash

echo "Benchmark suite starting..."

for file in *; do
    if [[ -f $file && $file == *.hs ]]; then
        # For each file, run the following
        stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A1m -N &>> "${file}.16.3.a1m.g2"
	echo "${file} benchmark 1"
	
	stack --resolver lts-17.10 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A1m -xn -N &>> "${file}.17.10.newgc.a1m.g2"
	echo "${file} benchmark 2"

        stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A1m -c -N &>> "${file}.16.3.compactgc.a1m.g2"
	echo "${file} benchmark 3"

        stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A50k -N &>> "${file}.16.3.a50k.g2"
	echo "${file} benchmark 4"
        stack --resolver lts-17.10 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A50k -xn -N &>> "${file}.17.10.newgc.a50k.g2"
	echo "${file} benchmark 5"

        stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A500m -N &>> "${file}.16.3.a500m.g2"
	echo "${file} benchmark 6"
        stack --resolver lts-17.10 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -A500m -xn -N &>> "${file}.17.10.newgc.a500m.g2"
	echo "${file} benchmark 7"

        # stack --resolver lts-16.3 ghc -- "$file" -freduction-depth=0 -fforce-recomp +RTS -tstderr -G1 -N &>> "${file}.16.3.g1"
	# echo "${file} benchmark 8"
    fi
done
