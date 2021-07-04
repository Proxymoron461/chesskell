#!/usr/bin/env bash

for n in {1..5}; do

cd 6

../quickbenchmark.sh

cd ../8

../quickbenchmark.sh

cd ../10

../quickbenchmark.sh

cd ../middlesegments

../quickbenchmark.sh

cd 4

../../quickbenchmark.sh

# cd 0

# ../../quickbenchmark.sh

cd ../2

../../quickbenchmark.sh

cd ../../last4of10

../quickbenchmark.sh

cd ../bastianvariations

../quickbenchmark.sh

cd ../backandforth

cd 0

../../quickbenchmark.sh

cd ../2

../../quickbenchmark.sh

cd ../4

../../quickbenchmark.sh

cd ../../

done
