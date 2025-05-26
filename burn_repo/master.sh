#!/bin/bash

#sbatch -p ckpt -A csde-ckpt --array=1-250 --nodes=1 --ntasks-per-node=20 --time=10:00:00 --mem=100G --job-name=s1000 --export=ALL,SIMNO=1000,NJOBS=250,NSIMS=5000 runsim.sh
sbatch -p ckpt -A csde-ckpt --array=1-250 --nodes=1 --ntasks-per-node=20 --time=10:00:00 --mem=100G --job-name=s2000 --export=ALL,SIMNO=2000,NJOBS=250,NSIMS=5000 runsim2019.sh
