set -e
while getopts m: flag
do
    case "${flag}" in
        m) model=${OPTARG};;
    esac
done

while IFS= read -r line;
do
    sed -e "s/{MODEL}/$model/g" -e "s/{START_VALS}/$line/g" run_ddm_Roptim.batch | sbatch
done < ddm_Roptim_start_vals.csv
