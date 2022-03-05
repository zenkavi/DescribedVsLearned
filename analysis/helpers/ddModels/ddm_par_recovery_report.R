library(tidyverse)
theme_set(theme_classic())
library(here)
helpers_path = here('analysis/helpers/')
cpueaters_path = '/Users/zeynepenkavi/CpuEaters/DescribedVsLearned_beh/analysis/helpers/'
source(paste0(helpers_path, 'ddModels/get_optim_out.R'))
source(paste0(helpers_path, 'ddModels/get_true_pars.R'))


rbind.all.columns <- function(x, y) {
  
  if(ncol(x) == 0 | ncol(y) == 0){
    out = plyr::rbind.fill(x, y)
  } else{
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    out = rbind(x, y)
  }
  return(out)
}

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

rename_cols = function(rename_dict, rename_df){
  
  for(i in 1:length(names(rename_df))){
    cur_col_name = names(rename_df)[i]
    if (cur_col_name %in% names(rename_dict)){
      names(rename_df)[i] = rename_dict[cur_col_name]
    }
  }
  
  return(rename_df)
}

ddm_par_recovery_report = function(model_, data_, optim_out_path_= paste0(cpueaters_path, 'ddModels/cluster_scripts/optim_out/'), true_pars_path_ = paste0(helpers_path, 'ddModels/cluster_scripts/test_data/'), diff_pct_plots_ = TRUE, start_end_scatter = FALSE, par_hist = FALSE){
  
  model_name = model_
  data_name = data_
  optim_out_path = optim_out_path_
  
  # Temporary solution to renaming parameters for outputs of models with different number of parameters
  param_dict = data.frame(Param1="d", Param2="sigma", Param3="delta", Param4="gamma", Result="nll")
  
  ###########################
  # Get true parameters
  ###########################
  
  tmp = get_true_pars(data_ = data_name, true_pars_path_ = true_pars_path_)
  
  true_pars = tmp$true_pars
  
  true_pars_str = tmp$true_pars_str
  
  true_pars = true_pars %>%
    gather(key, value)
  
  ###########################
  # Percentage of difference between true and converged parameter values
  ###########################
  
  if(diff_pct_plots_){
    # Get all files for this model and dataset combination
    out = get_optim_out(model_ = model_name, data_ = data_name, optim_out_path_ = optim_out_path, iters_ = FALSE)
    
    if(nrow(out) > 0){
      
      out = rename_cols(param_dict, out)
      
      diff_pct_data = out %>%
        gather(key, est) %>%
        left_join(true_pars %>% rename(true = value), by="key") %>%
        mutate(abs_diff_pct = abs(est-true)/true*100) %>%
        group_by(key) %>%
        summarise(median_diff = median(abs_diff_pct),
                  true_val = unique(true))
      p5 = diff_pct_data %>%
        ggplot(aes(key, median_diff))+
        geom_bar(stat="identity")+
        labs(y = "Median % difference", x="", title=wrapper(true_pars_str, 26))+
        theme(plot.title = element_text(size=8))+
        geom_text(aes(label = round(median_diff, 2), x = key, y = median_diff), vjust = -.5)
      
      return(list(true_pars = true_pars_str, p5=p5, diff_pct_data = diff_pct_data))
    } else{
      return(list())
    }
    
  } else {
    # Get all files for this model and dataset combination
    out = get_optim_out(model_ = model_name, data_ = data_name, optim_out_path_ = optim_out_path, iters_ = TRUE)
    
    if(nrow(out)> 0){
      
      out = rename_cols(param_dict, out)
      plots = list()
      
      ###########################
      # For random start where did each parameter end
      ###########################
      
      for(i in 1:names(out)){
        
        cur_par = names(out)[i]
        cur_par_out = out[,c(cur_par, "kernel", "Iteration")]
        
        p = cur_par_out %>% 
          mutate(start = ifelse(Iteration == 1, "start", "end")) %>%
          select(-Iteration) %>%
          gather(key, value, -kernel, -start) %>%
          spread(start, value) %>%
          ggplot(aes(start, end))+
          geom_point()+
          geom_hline(data=true_pars %>% filter(key == cur_par), aes(yintercept=value), color="red")+
          facet_wrap(~key, scale="free")
        
        plots[[i]] = p
        
      }
      
      # tmp1 = out %>%
      #   select(-delta, -gamma) %>%
      #   mutate(start = ifelse(Iteration == 1, "start", "end")) %>%
      #   select(-Iteration, -nll) %>%
      #   gather(key, value, -kernel, -start) %>%
      #   spread(start, value)
      # 
      # tmp2 = out %>%
      #   select(-d, -sigma) %>%
      #   mutate(start = ifelse(Iteration == 1, "start", "end")) %>%
      #   select(-Iteration, -nll) %>%
      #   gather(key, value, -kernel, -start) %>%
      #   spread(start, value)
      # 
      # p1 = tmp1 %>% ggplot(aes(start, end))+
      #   geom_point()+
      #   geom_hline(data=true_pars %>% filter(key %in% c("d", "sigma")), aes(yintercept=value), color="red")+
      #   facet_wrap(~key, scale="free")
      # 
      # p2 = tmp2 %>% ggplot(aes(start, end))+
      #   geom_point()+
      #   geom_hline(data=true_pars %>% filter(key %in% c("delta", "gamma")), aes(yintercept=value), color="red")+
      #   facet_wrap(~key, scale="free")+
      #   ylim(0, 8)
      
      ###########################
      # Histograms of the converged parameters
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
      
      p4 = tmp2 %>%
        ggplot(aes(value))+
        geom_histogram(bins = 30) +
        facet_wrap(~key, scales="free") +
        geom_vline(data=true_pars %>% filter(key %in% c("delta", "gamma")), aes(xintercept=value), color="red")+
        xlim(0, 8)
      
      return(list(true_pars = true_pars_str, plots = plots))
    } else{
      return(list())
    }
    
  }
  
}