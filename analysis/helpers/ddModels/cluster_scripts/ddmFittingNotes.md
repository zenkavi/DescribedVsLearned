# Test fitting in interactive container

docker run --rm -it -v $DATA_PATH:/ddModels zenkavi/roptim:0.0.4 bash

Rscript --vanilla /ddModels/cluster_scripts/ddm_Roptim.R --model oneIntegrator_sepProbDistortion --start_vals=0.353243110299734,0.550765508242755 --data=sub_data_distV_noExt/sub01_data --par_names=d,sigma --out_path=fitOneIntnoExt --num_optim_rounds 1 --fix_par_names=none --fix_par_vals none

# Move files from s3 to cluster

aws s3 sync s3://described-vs-experienced/ddModels/cluster_scripts/start_vals /shared/ddModels/cluster_scripts/start_vals

# Move files from cluster to s3

aws s3 sync /shared/ddModels/cluster_scripts/optim_out/fitOneIntnoExt s3://described-vs-experienced/ddModels/cluster_scripts/optim_out/fitOneIntnoExt

# Move files from s3 to local (this is only converged parameter values)

docker run --rm -it -v ~/.aws:/root/.aws -v $(pwd):/fitOneIntnoExt amazon/aws-cli s3 sync s3://described-vs-experienced/ddModels/cluster_scripts/optim_out/fitOneIntnoExt /fitOneIntnoExt --exclude "*" --include "*optim_par*"

# Job submission commands

./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub01_data -s sub_sv_oneInt01.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub02_data -s sub_sv_oneInt02.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub03_data -s sub_sv_oneInt03.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub04_data -s sub_sv_oneInt04.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub05_data -s sub_sv_oneInt05.csv -o fitOneIntnoExt -p d,sigma

./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub06_data -s sub_sv_oneInt06.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub07_data -s sub_sv_oneInt07.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub08_data -s sub_sv_oneInt08.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub09_data -s sub_sv_oneInt09.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub10_data -s sub_sv_oneInt10.csv -o fitOneIntnoExt -p d,sigma

./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub11_data -s sub_sv_oneInt11.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub12_data -s sub_sv_oneInt12.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub13_data -s sub_sv_oneInt13.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub14_data -s sub_sv_oneInt14.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub15_data -s sub_sv_oneInt15.csv -o fitOneIntnoExt -p d,sigma

./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub16_data -s sub_sv_oneInt16.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub17_data -s sub_sv_oneInt17.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub18_data -s sub_sv_oneInt18.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub19_data -s sub_sv_oneInt19.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub20_data -s sub_sv_oneInt20.csv -o fitOneIntnoExt -p d,sigma

./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub22_data -s sub_sv_oneInt21.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub23_data -s sub_sv_oneInt22.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub24_data -s sub_sv_oneInt23.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub25_data -s sub_sv_oneInt24.csv -o fitOneIntnoExt -p d,sigma
./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV_noExt/sub27_data -s sub_sv_oneInt25.csv -o fitOneIntnoExt -p d,sigma
