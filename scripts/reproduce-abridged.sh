#!/bin/bash

# Build figure-6 from the draft, but with smaller benchmark suite
# Script takes ~6min running in docker on 1.4GHz Intel i5 laptop with 8G RAM

# To be run from nerode-learn
mkdir ../output

# Reproduce the png for Figure 6 (a)
dune exec nerodelearn -- lsblanks-dir -ge -f ../benchmarks/oliveira/04_07 > ../output/small-fig-a-baseline.csv
dune exec nerodelearn -- lsblanks-dir -s -ge -f ../benchmarks/oliveira/04_07 > ../output/small-fig-a-unsats.csv
python3 ../scripts/state-vs-wlitems.py ../output/small-fig-a-baseline.csv ../output/small-fig-a-unsats.csv 
mv plot.png ../output/small-fig-6-a.png

# Reproduce the png for Figure 6 (b)
python3 ../scripts/time.py ../output/small-fig-a-unsats.csv
mv plot.png ../output/small-fig-6-b.png
