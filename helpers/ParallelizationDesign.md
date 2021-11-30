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

## Batch script

- Calls docker container

```
#!/bin/bash

#SBATCH -J level1-{SUBNUM}-{MODELNUM}-reg_rt-{REG_RT}
#SBATCH -c 8

# Outputs ----------------------------------
#SBATCH -o /shared/.out/level1-{SUBNUM}-{MODELNUM}-reg_rt-{REG_RT}.out
#SBATCH -e /shared/.err/level1-{SUBNUM}-{MODELNUM}-reg_rt-{REG_RT}.err
# ------------------------------------------

docker run --rm -e DATA_PATH=/data -e OUT_PATH=/out -e BEHAVIOR_PATH=/beh \
-v $DATA_PATH:/data -v $OUT_PATH:/out -v $BEHAVIOR_PATH:/beh -v $CODE_PATH:/code \
zenkavi/fsl:6.0.3 ./code/level1.py -s {SUBNUM} --mnum {MODELNUM} --reg_rt {REG_RT} --save_contrast {SAVE_CONTRAST}
```

## Cleanup script

