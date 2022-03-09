#!/bin/bash

for ((i=0; i<1890; i++)) 
do 
    Rscript sim/simulation-3.0.0_step-1.R --args $i 100 output/3.0
done 

Rscript output/combine_files.R --args output/3.0 3.0.0 1

