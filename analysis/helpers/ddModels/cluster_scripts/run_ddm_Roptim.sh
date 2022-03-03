set -e
while getopts m:d:s:p: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        d) data=${OPTARG};;
        s) starts=${OPTARG};;
        p) parNames=${OPTARG};;
        o) outPath=${OPTARG};;
    esac
done

while IFS= read -r line;
do
    sed -e "s/{MODEL}/$model/g" -e "s/{START_VALS}/$line/g" -e "s/{DATA}/$data/g" -e "s/{PAR_NAMES}/$parNames/g" -e "s/{OUT_PATH}/$outPath/g" run_ddm_Roptim.batch | sbatch
done < ./start_vals/$starts

# Usage: ./run_ddm_Roptim.sh -m model1a -d sim_single_sub_data1 -s ddm_Roptim_start_vals1.csv
# ./run_ddm_Roptim.sh -m model1c -d sim_single_sub_data46 -s ddm_Roptim_start_vals46.csv -p d, sigma, delta -o /ddModels/cluster_scripts/optim_out/sim3
