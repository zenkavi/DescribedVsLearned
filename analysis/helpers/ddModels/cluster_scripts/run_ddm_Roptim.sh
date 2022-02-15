set -e
while IFS= read -r line;
do
    sed -e "s/{MODELNUM}/model1a/g" -e "s/{START_VALS}/$line/g" run_ddm_Roptim.batch | sbatch
done < ddm_Roptim_start_vals.csv
