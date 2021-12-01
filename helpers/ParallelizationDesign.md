# Library independent general framework to parallelize grid search

## Shell script

Calls batch script inputting arguments

```
set -e
for modelnum in model4 model5 model6 model7
do
  sed -e "s/{MODELNUM}/$modelnum/g" run_level1.batch | sbatch
done
```
If you're running it locally you could submit the batch script to shell

```
set -e
for modelnum in model4 model5 model6 model7
do
  sed -e "s/{MODELNUM}/$modelnum/g" run_level1.batch | zsh
done
```

## Batch script (if running on cluster)

- Calls docker container
- Can you run a batch script locally without slurm? Would bash ignore the `SBATCH` commands as comments and execute the lines below? Yes!

```
#!/bin/bash

#SBATCH -J jobname-{MODELNUM}
#SBATCH -c 8

# Outputs ----------------------------------
#SBATCH -o /shared/.out/jobname-{MODELNUM}.out
#SBATCH -e /shared/.err/{MODELNUM}.err
# ------------------------------------------

docker run --rm -e DATA_PATH=/data -e OUT_PATH=/out \
-v $DATA_PATH:/data -v $OUT_PATH:/out -v $CODE_PATH:/code \
zenkavi/containerName:6.0.3 ./code/simulationScript.R -s --mnum {MODELNUM}
```

- The container call should be the smallest execution unit. E.g. for parallelizing a grid search it would be a single iteration

## Simulation script

- A function for a single iteration that takes all the details of the simulation as arguments (e.g. model name, parameter combination to simulate for, output location)
- Outputs the result for a single iteration with all the inputs in the file name

## Cleanup script

- Check if all iterations ran
- Concatenate all output to desired format
- Remove intermediate outputs
