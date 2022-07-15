set -e

# Default options
numOptimRounds=1
fixedParNames=none
fixedParVals=none

while getopts m:d:s:p:o:r:f:v: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        d) data=${OPTARG};;
        s) starts=${OPTARG};;
        p) parNames=${OPTARG};;
        o) outPath=${OPTARG};;
        r) numOptimRounds=${OPTARG};;
        f) fixedParNames=${OPTARG};;
        v) fixedParVals=${OPTARG};;
    esac
done

while IFS= read -r line;
do
    sed -e "s/{MODEL}/$model/g" -e "s/{START_VALS}/$line/g" -e "s|{DATA}|$data|g" -e "s/{PAR_NAMES}/$parNames/g" -e "s/{OUT_PATH}/$outPath/g" -e "s/{NUM_OPTIM_ROUNDS}/$numOptimRounds/g" -e "s/{FIX_PAR_NAMES}/$fixedParNames/g" -e "s/{FIX_PAR_VALS}/$fixedParVals/g" run_ddm_Roptim.batch | sbatch
done < ./start_vals/$starts

# ./run_ddm_Roptim.sh -m oneIntegrator_sepProbDistortion -d sub_data_distV/sub01_data -s sub_sv_oneInt01.csv -o fitOneInt -p d,sigma
# ./run_ddm_Roptim.sh -m model1c -d sub_data/sub_data01 -s test.csv -o fit1 -p d,sigma,delta
# ./run_ddm_Roptim.sh -m model1c -d sub_data/sub_data01 -s sub_start_vals01.csv -o fit1 -p d,sigma,delta
# ./run_ddm_Roptim.sh -m model1c -d test_data/sim_single_sub_data46 -s ddm_Roptim_start_vals46.csv -p d,sigma,delta -o sim3
# ./run_ddm_Roptim.sh -m model1c -d test_data/sim_single_sub_data46 -s ddm_Roptim_start_vals46.csv -p d,sigma -o sim3b -r 2 -f delta -v 1
