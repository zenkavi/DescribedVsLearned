set.seed(38573)

library(here)
helpers_path = here('analysis/helpers/')

n = 1000

out = data.frame(start_d = runif(n, 0, 1),
                 start_sigma = runif(n, 0, 1),
                 start_delta = runif(n, 1, 5),
                 start_gamma = runif(n, 1, 5))

write.table(out, file = paste0(helpers_path, 'ddModels/cluster_scripts/ddm_Roptim_start_vals.csv'), row.names = F, col.names = F, sep=',')
