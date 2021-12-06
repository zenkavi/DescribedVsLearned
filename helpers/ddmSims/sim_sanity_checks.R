library(broom)

sim_sanity_checks = function(sim_data, checks = c(1,2,3,4,5), compare_rts = TRUE, compare_logits = FALSE, true_data = sub_data, yrange_lim = 25){
  #Check 1
  if (1 %in% checks){
    print(paste0("Proportion of time out trials if no decision made: ", round(sum(is.na(sim_data$reactionTime))/nrow(sim_data), 3)))
    
    if("decPreStim" %in% names(sim_data)){
      
      print(paste0("Proportion of trials with decision before stim: ", round(sum(sim_data$decPreStim)/nrow(sim_data), 3)))
    }
    
    if("timeOut" %in% names(sim_data)){
      print(paste0("Proportion of time out trials with decision of closest boundary sampled RTs: ", round(sum(sim_data$timeOut)/nrow(sim_data), 3)))
    }
  }
  
  
  # Check 2
  if(2 %in% checks){
    p = sim_data %>%
      drop_na()%>%
      select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
      filter(probFractalDraw == 0 | probFractalDraw == 1) %>%
      mutate(choiceLeft =ifelse(choice == "left", 1, ifelse(choice == "right", 0, NA))) %>%
      mutate(EVLeftMinRight = EVLeft - EVRight,
             QVLeftMinRight = QVLeft - QVRight) %>%
      select(-choice, -EVLeft, -EVRight, -QVLeft, -QVRight) %>%
      gather(key, value, -probFractalDraw, -reactionTime, -choiceLeft) %>%
      ggplot(aes(value, choiceLeft))+
      geom_jitter(height = .1)+
      geom_smooth(formula = 'y~x', method = "glm", method.args = list(family=binomial), fullrange=TRUE, alpha=.1)+
      facet_grid(key ~ probFractalDraw)
    print(p)
  }
  
  
  # Check 3
  if(3 %in% checks){
    if(compare_rts){
      p = sim_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
        mutate(data_type = "sim") %>%
        rbind(true_data %>%
                mutate(choice = ifelse(choiceLeft == 1, "left", "right"),
                       data_type = "true") %>%
                select(-subnum, -choiceLeft)) %>%
        mutate(probFractalDraw = as.factor(probFractalDraw)) %>%
        ggplot(aes(reactionTime, fill=data_type)) +
        geom_histogram(position="identity", bins=30, alpha=.5) +
        labs(title="RT long tail?", fill="")+
        facet_wrap(~probFractalDraw)+
        theme(legend.position = "bottom")
    }
    else{
      p = sim_data %>%
        mutate(probFractalDraw = as.factor(probFractalDraw)) %>%
        ggplot(aes(reactionTime)) +
        geom_histogram(position="identity", bins=30) +
        labs(title="RT long tail?")+
        facet_wrap(~probFractalDraw)
    }
    
    print(p)
  }
  
  
  # Check 4
  if(4 %in% checks){
    if(compare_rts){
      p = sim_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
        mutate(data_type = "sim") %>%
        rbind(true_data %>%
                mutate(choice = ifelse(choiceLeft == 1, "left", "right"),
                       data_type = "true") %>%
                select(-subnum, -choiceLeft)) %>%
        drop_na()%>%
        mutate(probFractalDraw = as.factor(probFractalDraw), 
               log_rt = log(reactionTime)) %>%
        filter(is.finite(log_rt)) %>%
        group_by(probFractalDraw, data_type) %>%
        summarise(mean_log_rt = mean(log_rt),
                  sem_log_rt = sem(log_rt), .groups="keep") %>%
        ggplot(aes(probFractalDraw, mean_log_rt, color=data_type))+
        geom_point()+
        geom_errorbar(aes(ymin = mean_log_rt - sem_log_rt, ymax = mean_log_rt + sem_log_rt), width=.2)+
        labs(title="Inverse U and faster when pFrac ==1?", color="")+
        theme(legend.position = "bottom")
    } else{
      p = sim_data %>%
        drop_na()%>%
        mutate(probFractalDraw = as.factor(probFractalDraw), 
               log_rt = log(reactionTime)) %>%
        group_by(probFractalDraw) %>%
        summarise(mean_log_rt = mean(log_rt),
                  sem_log_rt = sem(log_rt)) %>%
        ggplot(aes(probFractalDraw, mean_log_rt))+
        geom_point()+
        geom_errorbar(aes(ymin = mean_log_rt - sem_log_rt, ymax = mean_log_rt + sem_log_rt), width=.2)+
        labs(title="Inverse U and faster when pFrac ==1?")
    }
    print(p)
  }
  
  # Check 5
  if(5 %in% checks){
    tmp = sim_data %>%
      select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
      mutate(probFractalDraw = as.factor(probFractalDraw),
             choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
             EVDiff = EVLeft - EVRight, 
             QVDiff = QVLeft - QVRight) %>%
      nest(data = -probFractalDraw) %>% 
      mutate(
        fit = map(data, ~ glm(choiceLeft ~ scale(EVDiff) + scale(QVDiff), data = .x, family=binomial(link="logit"))),
        tidied = map(fit, tidy)
      ) %>% 
      unnest(tidied) %>%
      filter(term != "(Intercept)") %>%
      select(probFractalDraw, term, estimate, std.error) %>%
      mutate(data_type = "sim")
    
    p =  tmp %>%
      ggplot(aes(probFractalDraw, estimate, col=term, group=term))+
      geom_point()+
      geom_line()+
      geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate +std.error), width=0.2)+
      geom_hline(aes(yintercept=0), linetype="dashed")+
      scale_color_manual(values = cbbPalette[2:1])+
      theme(legend.position = "bottom")+
      labs(color="", title="Relevant attribute effect on choice")
    
    if(compare_logits){
      tmp_true = true_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choiceLeft, reactionTime) %>%
        mutate(probFractalDraw = as.factor(probFractalDraw),
               # choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
               EVDiff = EVLeft - EVRight, 
               QVDiff = QVLeft - QVRight) %>%
        nest(data = -probFractalDraw) %>% 
        mutate(
          fit = map(data, ~ glm(choiceLeft ~ scale(EVDiff) + scale(QVDiff), data = .x, family=binomial(link="logit"))),
          tidied = map(fit, tidy)
        ) %>% 
        unnest(tidied) %>%
        filter(term != "(Intercept)") %>%
        select(probFractalDraw, term, estimate, std.error)
      
      p = p +
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate +std.error), width=0.2)+
        geom_point( data=tmp_true, aes(probFractalDraw, estimate, col=term, group=term, alpha=.1))+
        geom_line(data=tmp_true, aes(probFractalDraw, estimate, col=term, group=term, alpha=.1))+
        geom_errorbar(data=tmp_true,aes(ymin = estimate - std.error, ymax = estimate +std.error, alpha=.1), width=0.2)
        
    } 
    
    yrange = layer_scales(p)$y$range$range
    large_range = abs(yrange) > yrange_lim
    if(large_range){
      p = p+ylim(-5, yrange_lim)
    }
    
    print(p)
  }  
}