# Test fitting in interactive container

```
docker run --rm -it -v $DATA_PATH:/ddModels zenkavi/roptim:0.0.4 bash

Rscript --vanilla /ddModels/cluster_scripts/ddm_Roptim.R --model oneIntegrator_sepProbDistortion --start_vals=0.353243110299734,0.550765508242755 --data=sub_data_oneParamAsymmLinear/sub01_data --par_names=d,sigma --out_path=fitOneInt_oneParamAsymmLinear --num_optim_rounds 1 --fix_par_names=none --fix_par_vals none

Rscript --vanilla /ddModels/cluster_scripts/ddm_Roptim.R --model twoIntegrators_sepProbDistortion --start_vals=0.932282596703016,0.0299321074307384,0.957637683131231,0.196293133611851 --data=sub_data_oneParamAsymmLinear/sub01_data --par_names=dLott,dFrac,sigmaLott,sigmaFrac --out_path=fitTwoInts_oneParamAsymmLinear --num_optim_rounds 1 --fix_par_names=none --fix_par_vals none

Rscript --vanilla /ddModels/cluster_scripts/ddm_Roptim.R --model twoIntegrators_sepProbDistortion --start_vals=0.932282596703016,0.0299321074307384 --data=sub_data_oneParamAsymmLinear/sub01_data --par_names=dLott,dFrac --out_path=fitTwoInts_oneParamAsymmLinear_fixSigma --num_optim_rounds 1 --fix_par_names=sigmaLott,sigmaFrac --fix_par_vals .05,.05
```

# Move files from s3 to cluster

```
aws s3 sync s3://described-vs-experienced/ddModels/cluster_scripts/start_vals /shared/ddModels/cluster_scripts/start_vals

aws s3 sync s3://described-vs-experienced/ddModels/r_ddm_models /shared/ddModels/r_ddm_models
```

# Move files from cluster to s3

```
aws s3 sync /shared/ddModels/cluster_scripts/optim_out/fitTwoInts_oneParamAsymmLinear s3://described-vs-experienced/ddModels/cluster_scripts/optim_out/fitTwoInts_oneParamAsymmLinear
```

# Move files from s3 to local (this is only converged parameter values)

```
docker run --rm -it -v ~/.aws:/root/.aws -v $(pwd):/fitTwoInts_oneParamAsymmLinear amazon/aws-cli s3 sync s3://described-vs-experienced/ddModels/cluster_scripts/optim_out/fitTwoInts_oneParamAsymmLinear /fitTwoInts_oneParamAsymmLinear --exclude "*" --include "*optim_par*"
```

# Job submission commands

## Examples for one subjets

```
sh run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub01_data -s sub_sv_oneInt01.csv -o fitOneInt_oneParamAsymmLinear -p d,sigma
sh run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_oneParamAsymmLinear_noExt/sub01_data -s sub_sv_oneInt01.csv -o fitOneIntnoExt_oneParamAsymmLinear -p d,sigma
sh run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_oneParamSymmLinear/sub01_data -s sub_sv_oneInt01.csv -o fitOneInt_oneParamSymmLinear -p d,sigma
sh run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_oneParamSymmLinear_noExt/sub01_data -s sub_sv_oneInt01.csv -o fitOneIntnoExt_oneParamSymmLinear -p d,sigma
```

```
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub01_data -s sub_sv_twoInts01.csv -o fitTwoInts_oneParamAsymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamAsymmLinear_noExt/sub01_data -s sub_sv_twoInts01.csv -o fitTwoIntsnoExt_oneParamAsymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamSymmLinear/sub01_data -s sub_sv_twoInts01.csv -o fitTwoInts_oneParamSymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamSymmLinear_noExt/sub01_data -s sub_sv_twoInts01.csv -o fitTwoIntsnoExt_oneParamSymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
```

```
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub01_data -s sub_sv_twoInts_fixSigma01.csv -o fitTwoInts_oneParamAsymmLinear_fixSigma -p dLott,dFrac -f sigmaLott,sigmaFrac -v .5,.5
```

## Loop to submit for more subjects

```
for subnum in 03 04 05 06 07 08 09 10
do
sh run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub$subnum\_data -s sub_sv_oneInt$subnum.csv -o fitOneInt_oneParamAsymmLinear -p d,sigma
done
```

```
for subnum in 11 12 13 14 15 16 17 18 19 20 22 23 24 25 27
do
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub$subnum\_data -s sub_sv_twoInts$subnum.csv -o fitTwoInts_oneParamAsymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
done
```

```
for subnum in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 22 23 24 25 27
do
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamSymmLinear/sub$subnum\_data -s sub_sv_twoInts$subnum.csv -o fitTwoInts_oneParamSymmLinear -p dLott,dFrac,sigmaLott,sigmaFrac
done
```

```
for subnum in 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 22 23 24 25 27
do
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamAsymmLinear/sub$subnum\_data -s sub_sv_twoInts_fixSigma$subnum.csv -o fitTwoInts_oneParamAsymmLinear_fixSigma -p dLott,dFrac -f sigmaLott,sigmaFrac -v .5,.5
done
```

```
for subnum in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 22 23 24 25 27
do
sh run_ddm_Roptim.sh -m twoIntegrators_sepProbDistortion -d sub_data_oneParamSymmLinear/sub$subnum\_data -s sub_sv_twoInts_fixSigma$subnum.csv -o fitTwoInts_oneParamSymmLinear_fixSigma -p dLott,dFrac -f sigmaLott,sigmaFrac -v .5,.5
done
```
