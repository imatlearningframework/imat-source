#!/bin/bash

# To be run from nerode-learn
mkdir ../output

# Reproduce the csv for Table 1 (p 23) [~25min on laptop]
dune exec nerodelearn -- lsblanks-dir -s -ge -f ../benchmarks/table-1 > ../output/table-1.csv

# Reproduce the png for Figure 6 (a)
dune exec nerodelearn -- lsblanks-dir -ge -f ../benchmarks/oliveira/04_09 > ../output/fig-a-baseline.csv
dune exec nerodelearn -- lsblanks-dir -s -ge -f ../benchmarks/oliveira/04_09 > ../output/fig-a-unsats.csv
python3 ../scripts/state-vs-wlitems.py ../output/fig-a-baseline.csv ../output/fig-a-unsats.csv 
mv plot.png ../output/fig-6-a.png

# Reproduce the png for Figure 6 (b)
dune exec nerodelearn -- lsblanks-dir -s -ge -f ../benchmarks/oliveira/04_11 > ../output/fig-b.csv
python3 ../scripts/time.py ../output/fig-b.csv
mv plot.png ../output/fig-6-b.png
