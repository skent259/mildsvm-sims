#!/bin/bash

sim="3.0.1"
step="2"
cd $sim 

# Record the total run time and any errors 
grep "Total Remote Usage" log/*.log > "log-time_$sim-$step.txt"
grep "Error" out/*.out > "error-$sim-$step.txt"

# Clean up the err, log, out files 
rm err/*.err
rm log/*.log
rm out/*.out

cd .. 