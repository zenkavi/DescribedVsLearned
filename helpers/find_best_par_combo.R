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


find_best_par_combo = function(sub_data, model_name, d_par_space = ..., sigma_par_space = seq(.01, .1, .01), barrier_par_space = c(1), heatmaps = TRUE, save_output=TRUE, out_path = here()){
  
  out = data.frame()
  
  for(cur_d in d_par_space){
    for(cur_sigma in sigma_par_space){
      for(cur_barrier in barrier_par_space){
        print(paste0("cur_d = ", cur_d, " cur_sigma = ", cur_sigma))
        out_row = data.frame(d = cur_d, sigma = cur_sigma, barrier_decay = cur_barrier)
        sim_data = sim_task(stimuli=sub_data, model_name = model_name, d = cur_d, sigma = cur_sigma, barrierDecay = cur_sigma)
        out_row$rt_sumsq = get_rt_sumsq(sub_data, sim_data)
        out_row$choice_sumsq = get_choice_sumsq(sub_data, sim_data)
        out_row$avg_sumsq = with(out_row, (rt_sumsq + choice_sumsq)/2)
        
        out = rbind.all.columns(out, out_row)
      }
    }
  }
  
  if(save_output){
    rand_str = round(runif(1, 100, 1000))
    write.csv(out, paste0(out_path, '/outputs/find_best_par_combo_', model_name,'_' , rand_str,'.csv'), row.names = FALSE)
  }
  
  if(heatmaps){
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
  }
  
  return(list(opt_rt_pars = out[out$rt_sumsq == min(out$rt_sumsq, na.rm=T),],
              opt_choice_pars = out[out$choice_sumsq == min(out$choice_sumsq, na.rm = T),],
              opt_avg_pars = out[out$avg_sumsq == min(out$avg_sumsq, na.rm=T),]))
}
