get_rt_sumsq = function(sub_data, sim_data){
  
  out = sim_data %>%
    select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
    mutate(data_type = "sim") %>%
    rbind(sub_data %>%
            mutate(choice = ifelse(choiceLeft == 1, "left", "right"),
                   data_type = "true") %>%
            select(-subnum, -choiceLeft)) %>%
    drop_na()%>%
    mutate(probFractalDraw = as.factor(probFractalDraw), 
           log_rt = log(reactionTime)) %>%
    filter(is.finite(log_rt)) %>%
    group_by(probFractalDraw, data_type) %>%
    summarise(mean_log_rt = mean(log_rt),.groups="keep") %>%
    spread(data_type, mean_log_rt) %>%
    mutate(sqdev = (sim-true)^2)
  
  rt_sumsq = sum(out$sqdev)
  
  return(rt_sumsq)
}

get_choice_sumsq = function(sub_data, sim_data){
  out1 = sim_data %>%
    select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
    mutate(probFractalDraw = as.factor(probFractalDraw),
           choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
           EVDiff = EVLeft - EVRight, 
           QVDiff = QVLeft - QVRight) %>%
    nest(data = -probFractalDraw) %>% 
    mutate(
      fit = map(data, ~ glm(choiceLeft ~ EVDiff + QVDiff, data = .x, family=binomial(link="logit"))),
      tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>%
    filter(term != "(Intercept)") %>%
    select(probFractalDraw, term, estimate) %>%
    mutate(term = paste0(term, "_sim"))
  
  out2 = sub_data %>%
    select(-subnum) %>%
    mutate(choice = ifelse(choiceLeft == 1, "left", "right")) %>%
    select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
    mutate(probFractalDraw = as.factor(probFractalDraw),
           choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
           EVDiff = EVLeft - EVRight, 
           QVDiff = QVLeft - QVRight) %>%
    nest(data = -probFractalDraw) %>% 
    mutate(
      fit = map(data, ~ glm(choiceLeft ~ EVDiff + QVDiff, data = .x, family=binomial(link="logit"))),
      tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>%
    filter(term != "(Intercept)") %>%
    select(probFractalDraw, term, estimate) %>%
    mutate(term = paste0(term, "_true"))
  
  out = out1 %>%
    rbind(out2) %>%
    spread(term, estimate) %>%
    mutate(evsqdev = (EVDiff_sim - EVDiff_true)^2,
           qvsqdev = (QVDiff_sim - QVDiff_true)^2,
           sqdev = evsqdev + qvsqdev)
  
  choice_sumsq = sum(out$sqdev)
  
  return(choice_sumsq)
}

get_opt_heatmaps = function(out){
  
  if(d %in% names(out)){
    p1 = out %>%
      mutate(d = as.factor(d),
             sigma = as.factor(sigma)) %>%
      ggplot(aes(d, sigma, fill=log(rt_sumsq))) +
      geom_tile()+
      theme(legend.position = "bottom")
    
    p2 = out %>%
      mutate(d = as.factor(d),
             sigma = as.factor(sigma)) %>%
      ggplot(aes(d, sigma, fill=log(choice_sumsq))) +
      geom_tile()+
      theme(legend.position = "bottom")
    
    p3 = grid.arrange(p1, p2, ncol=2)
    print(p3)
  } else {
    print("Too many dimensions to make heatmaps")
  }
  
  
}


sim_par_combs = function(sub_data, model_name, d_par_space, sigma_par_space, heatmaps = TRUE, save_output=TRUE, out_path = here(), ...){
  
  out = data.frame()
  
  if(model_name %in% c("model4", "model5")){
    for(cur_dArb in d_par_space){
      for(cur_dAttr in d_par_space){
        for(cur_sigmaArb in sigma_par_space){
          for(cur_sigmaAttr in sigma_par_space){
            print(paste0("cur_dArb = ", cur_dArb,"cur_dAttr = ", cur_dAttr, " cur_sigmaArb = ", cur_sigmaArb,  " cur_sigmaAttr = ", cur_sigmaAttr))
            out_row = data.frame(dArb = cur_dArb, dAttr = cur_dAttr, sigmaArb = cur_sigmaArb, sigmaAttr = cur_sigmaAttr)
            sim_data = sim_task(stimuli=sub_data, model_name = model_name, dArb = cur_dArb, dAttr = cur_dAttr, sigmaArb = cur_sigmaArb, sigmaAttr = cur_sigmaAttr)
            out_row$rt_sumsq = get_rt_sumsq(sub_data, sim_data)
            out_row$choice_sumsq = get_choice_sumsq(sub_data, sim_data)
            out_row$avg_sumsq = with(out_row, (rt_sumsq + choice_sumsq)/2)
            
            out = rbind.all.columns(out, out_row)
          }
        }
      } 
    }
  } else{
    for(cur_d in d_par_space){
      for(cur_sigma in sigma_par_space){
        print(paste0("cur_d = ", cur_d, " cur_sigma = ", cur_sigma))
        out_row = data.frame(d = cur_d, sigma = cur_sigma)
        sim_data = sim_task(stimuli=sub_data, model_name = model_name, d = cur_d, sigma = cur_sigma)
        out_row$rt_sumsq = get_rt_sumsq(sub_data, sim_data)
        out_row$choice_sumsq = get_choice_sumsq(sub_data, sim_data)
        out_row$avg_sumsq = with(out_row, (rt_sumsq + choice_sumsq)/2)
        
        out = rbind.all.columns(out, out_row)
      }
    }
  }
  
  out$model_name = model_name
  
  if(save_output){
    rand_str = round(runif(1, 100, 1000))
    write.csv(out, paste0(out_path, '/outputs/grid_search_', model_name,'_' , rand_str,'.csv'), row.names = FALSE)
  }
  
  if(heatmaps){
    get_opt_heatmaps(out)
  }
  
  return(out)
}

find_best_par_combo = function(sub_data, model_name, d_par_space, sigma_par_space){
  
  obj_name = paste0("m", readr::parse_number(model_name), "_opt_out")
  
  latest_opt_out = file.info(list.files(paste0(here(), '/outputs'), full.names=T)) %>%
    mutate(fname=  row.names(.)) %>%
    filter(grepl(model_name, fname)) %>%
    filter(mtime == max(mtime))
  
  if(exists(obj_name)){#object name in r env
    print("Object already exists.")
    out = get(obj_name)
    get_opt_heatmaps(out)
    
  } else if (nrow(latest_opt_out)>0){#if it doesn't exist but a previous output has been saved
    
    out = read.csv(latest_opt_out$fname)
    get_opt_heatmaps(out)
    out = list(opt_rt_pars = out[out$rt_sumsq == min(out$rt_sumsq, na.rm=T),] %>% drop_na(),
               opt_choice_pars = out[out$choice_sumsq == min(out$choice_sumsq, na.rm = T),] %>% drop_na(),
               opt_avg_pars = out[out$avg_sumsq == min(out$avg_sumsq, na.rm=T),] %>% drop_na())
    
  } else{ #no objcet of output found
    print("Could not find saved opt out file for this model. Running new simulation.")
    out = sim_par_combs(sub_data, model_name = model_name, 
                        d_par_space = d_par_space, 
                        sigma_par_space = sigma_par_space, 
                        heatmaps=TRUE)
  }
  
  return(out)
}


sim_w_best_combo = function(opt_out, model_name){
  
  if (model_name %in% c("model4", "model5")){
    if(opt_out$opt_rt_pars$dArb == opt_out$opt_choice_pars$dArb & opt_out$opt_rt_pars$dAttr == opt_out$opt_choice_pars$dAttr & opt_out$opt_rt_pars$sigmaArb == opt_out$opt_choice_pars$sigmaArb & opt_out$opt_rt_pars$sigmaAttr == opt_out$opt_choice_pars$sigmaAttr){
      print("Same parameter combination works best for both RT and choice")
      sim_data_opt_rt = sim_task(stimuli = sub_data, model_name = model_name, dArb = opt_out$opt_rt_pars$dArb, dAttr = opt_out$opt_rt_pars$dAttr, sigmaArb = opt_out$opt_rt_pars$sigmaArb, sigmaAttr = opt_out$opt_rt_pars$sigmaAttr)
      sim_data_opt_choice = NA
    } else {
      sim_data_opt_rt = sim_task(stimuli = sub_data, model_name = model_name, dArb = opt_out$opt_rt_pars$dArb, dAttr = opt_out$opt_rt_pars$dAttr, sigmaArb = opt_out$opt_rt_pars$sigmaArb, sigmaAttr = opt_out$opt_rt_pars$sigmaAttr)
      sim_data_opt_choice = sim_task(stimuli = sub_data, model_name = model_name, dArb = opt_out$opt_choice_pars$dArb, dAttr = opt_out$opt_choice_pars$dAttr, sigmaArb = opt_out$opt_choice_pars$sigmaArb, sigmaAttr = opt_out$opt_choice_pars$sigmaAttr)
    }
  } else{
    if(opt_out$opt_rt_pars$d == opt_out$opt_choice_pars$d & opt_out$opt_rt_pars$sigma == opt_out$opt_choice_pars$sigma){
      print("Same parameter combination works best for both RT and choice")
      sim_data_opt_rt = sim_task(stimuli = sub_data, model_name = model_name,d = opt_out$opt_rt_pars$d, sigma = opt_out$opt_rt_pars$sigma)
      sim_data_opt_choice = NA
    } else {
      sim_data_opt_rt = sim_task(stimuli = sub_data, model_name = model_name,d = opt_out$opt_rt_pars$d, sigma = opt_out$opt_rt_pars$sigma)
      sim_data_opt_choice = sim_task(stimuli = sub_data, model_name = model_name,d = opt_out$opt_choice_pars$d, sigma = opt_out$opt_choice_pars$sigma)
    }
  }
  
  return(list(sim_data_opt_rt = sim_data_opt_rt,
              sim_data_opt_choice = sim_data_opt_choice))
}
