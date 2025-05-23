#!/bin/bash

sbatch -p csde -A csde --job-name=ABC.2.nsfg --export=ALL,--ntasks-per-node=20 --mem=100G --time=200:00:00 runsim.sh
