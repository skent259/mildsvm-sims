#!/bin/bash

# 1.0, 2.0, 3.0, 4.0  --------------------------------------------------------#
for sim in 1.0 2.0 3.0 4.0
do
    for ((i=0; i<3780; i++)) 
    do 
        Rscript sim/simulation-X.0.0_step-1.R --args $sim.0 $i 50 output/$sim
    done 

    for ((i=0; i<540; i++)) 
    do 
        Rscript sim/simulation-X.0.1_step-1.R --args $sim.1 $i 200 output/$sim
    done 

    Rscript output/combine_files.R output/$sim $sim.0 1
    Rscript output/combine_files.R output/$sim $sim.1 1

    for ((i=0; i<1050; i++)) 
    do 
        Rscript sim/simulation-X.0.0_step-2.R --args $sim.0 $i 4 output/$sim
    done 

    for ((i=0; i<480; i++)) 
    do 
        Rscript sim/simulation-X.0.1_step-2.R --args $sim.1 $i 5 output/$sim
    done 

    Rscript output/combine_files.R output/$sim $sim.0 2
    Rscript output/combine_files.R output/$sim $sim.1 2

done


# 5.0.0, fiber features -----------------------------------------------------#
for ((i=0; i<3150; i++)) 
do 
    Rscript sim/dcis-ff-5.0.0_step-1.R --args $i 10 output/5.0 data/processed
done 

Rscript output/combine_files.R output/5.0 5.0.0 1

for ((i=0; i<500; i++)) 
do 
    Rscript sim/dcis-ff-5.0.0_step-1.R --args $i 1 output/5.0 data/processed
done 

Rscript output/combine_files.R output/5.0 5.0.0 2
