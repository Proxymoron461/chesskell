#!/usr/bin/env bash

for n in {1..5}; do

cd 6

../benchmark.sh

cd ../8

../benchmark.sh

cd ../10

../benchmark.sh

cd ../middlesegments

../benchmark.sh

cd 4

../../benchmark.sh

# cd 0

# ../../benchmark.sh

cd ../2

../../benchmark.sh

cd ../../last4of10

../benchmark.sh

cd ../bastianvariations

../benchmark.sh

cd ../backandforth

cd 0

../../benchmark.sh

cd ../2

../../benchmark.sh

cd ../4

../../benchmark.sh

cd ../../

done
