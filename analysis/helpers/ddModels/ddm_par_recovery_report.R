library(tidyverse)
library(here)
helpers_path = here('analysis/helpers/')

ddm_par_recovery_report = function(model_, data_, optim_out_path_){
  
  model_name = model_
  data_name = data_
  optim_out_path = paste0(helpers_path, 'ddModels/cluster_scripts/optim_out/')
  
  # List all files for this model and dataset combination
  fns = list.files(optim_out_path)
  fns = fns[grepl(model_name, fns) & grepl(data_name, fns) & grepl("iter", fns)]
  
  # Read in all starting and converged values
  out = data.frame()
  for(i in 1:length(fns)){
    tmp = read.csv(paste0(optim_out_path, fns[i]))
    start_row = tmp[1,]
    end_row = tmp[nrow(tmp),]
    tmp_row = rbind(start_row, end_row)
    tmp_row$kernel = i
    out = rbind.all.columns(out, tmp_row)  
  }
  
  out = out %>%
    rename(d = Param1, sigma = Param2, delta = Param3, gamma = Param4, nll = Result)
  
  ###########################
  # Get true parameters
  ###########################
  true_pars = read.csv(paste0(helpers_path, 'ddModels/cluster_scripts/test_data/',data_name,'.csv'))
  true_pars = true_pars %>%
    select(d, sigma, delta, gamma) %>%
    distinct()
  
  true_pars_str = paste0("d = ", true_pars$d, ", sigma = ", true_pars$sigma, ", delta = ", true_pars$delta, ", gamma = ", true_pars$gamma)
  
  true_pars = true_pars %>%
    gather(key, value)
  
  print(true_pars)
  
  ###########################
  # For random start where did each parameter end
  ###########################
  
  tmp1 = out %>%
    select(-delta, -gamma) %>%
    mutate(start = ifelse(Iteration == 1, "start", "end")) %>%
    select(-Iteration, -nll) %>%
    gather(key, value, -kernel, -start) %>%
    spread(start, value)
  
  tmp2 = out %>%
    select(-d, -sigma) %>%
    mutate(start = ifelse(Iteration == 1, "start", "end")) %>%
    select(-Iteration, -nll) %>%
    gather(key, value, -kernel, -start) %>%
    spread(start, value)
  
  p1 = tmp1 %>% ggplot(aes(start, end))+
    geom_point()+
    geom_hline(data=true_pars %>% filter(key %in% c("d", "sigma")), aes(yintercept=value), color="red")+
    facet_wrap(~key, scale="free")
  
  print(p1)
  
  p2 = tmp2 %>% ggplot(aes(start, end))+
    geom_point()+
    geom_hline(data=true_pars %>% filter(key %in% c("delta", "gamma")), aes(yintercept=value), color="red")+
    facet_wrap(~key, scale="free")+
    ylim(0, 8)
  
  print(p2)
  
  ###########################
  # Distribution of the converged parameters
  ###########################
  
  tmp1 = out %>%
    filter(Iteration != 1) %>%
    select(d, sigma) %>%
    gather(key, value)
  
  tmp2 = out %>%
    filter(Iteration != 1) %>%
    select(delta, gamma) %>%
    gather(key, value)
  
  p3 = tmp1 %>%
    ggplot(aes(value))+
    geom_histogram(bins = 30) +
    facet_wrap(~key, scales="free") +
    geom_vline(data=true_pars %>% filter(key %in% c("d", "sigma")), aes(xintercept=value), color="red")
  
  print(p3)
  
  p4 = tmp2 %>%
    ggplot(aes(value))+
    geom_histogram(bins = 30) +
    facet_wrap(~key, scales="free") +
    geom_vline(data=true_pars %>% filter(key %in% c("delta", "gamma")), aes(xintercept=value), color="red")+
    xlim(0, 8)
  
  print(p4)
  
  return(list(true_pars = true_pars_str, p1=p1, p2=p2, p3=p3, p4=p4))
}