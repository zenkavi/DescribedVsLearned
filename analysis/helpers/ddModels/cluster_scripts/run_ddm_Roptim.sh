set -e
while getopts m:d:s: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
        d) data=${OPTARG};;
        s) starts=${OPTARG};;
    esac
done

while IFS= read -r line;
do
    sed -e "s/{MODEL}/$model/g" -e "s/{START_VALS}/$line/g" -e "s/{DATA}/$data/g" run_ddm_Roptim.batch | sbatch
done < $starts

# Usage: ./run_ddm_Roptim.sh -m model1a -d sim_single_sub_data -s ddm_optim_start_vals.csv
