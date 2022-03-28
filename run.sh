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


# 5.0.0, fiber features 
for ((i=0; i<100; i++)) 
do 
    python sim/dcis-ff-5.0.1_step-1.py $i 1 output/5.0 data/processed
done 

python sim/dcis-ff-5.0.1_step-2.py 100 1 output/5.0 data/processed
Rscript sim/dcis-ff-5.0.1_step-3.R output/5.0