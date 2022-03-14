#!/bin/bash

# 1.0.0 
for ((i=0; i<3780; i++)) 
do 
    Rscript sim/simulation-1.0.0_step-1.R --args $i 50 output/1.0
done 

Rscript output/combine_files.R output/1.0 1.0.0 1

for ((i=0; i<1050; i++)) 
do 
    Rscript sim/simulation-1.0.0_step-2.R --args $i 4 output/1.0
done 

Rscript output/combine_files.R output/1.0 1.0.0 2

# 2.0.0 
for ((i=0; i<3780; i++)) 
do 
    Rscript sim/simulation-2.0.0_step-1.R --args $i 50 output/2.0
done 

Rscript output/combine_files.R output/2.0 2.0.0 1

for ((i=0; i<1050; i++)) 
do 
    Rscript sim/simulation-2.0.0_step-2.R --args $i 4 output/2.0
done 

Rscript output/combine_files.R output/2.0 2.0.0 2

# 3.0.0 
for ((i=0; i<1890; i++)) 
do 
    Rscript sim/simulation-3.0.0_step-1.R --args $i 100 output/3.0
done 

Rscript output/combine_files.R output/3.0 3.0.0 1

for ((i=0; i<1050; i++)) 
do 
    Rscript sim/simulation-3.0.0_step-2.R --args $i 4 output/3.0
done 

Rscript output/combine_files.R output/3.0 3.0.0 2

# 4.0.0 
for ((i=0; i<3780; i++)) 
do 
    Rscript sim/simulation-4.0.0_step-1.R --args $i 50 output/4.0
done 

Rscript output/combine_files.R output/4.0 4.0.0 1

for ((i=0; i<1050; i++)) 
do 
    Rscript sim/simulation-4.0.0_step-2.R --args $i 4 output/4.0
done 

Rscript output/combine_files.R output/4.0 4.0.0 2
