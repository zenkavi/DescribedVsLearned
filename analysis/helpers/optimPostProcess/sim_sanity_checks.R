library(broom)
library(here)
sem <- function(x) {sd(x, na.rm=T) / sqrt(length(x))}

sim_sanity_checks = function(sim_data, checks = c(1,2,3,4,5), compare_rts = TRUE, compare_logits = FALSE, true_data = sub_data, yrange_lim = 25, scale_logits=F, return_plot=F, logit_facets = T){
  
  if(return_plot){
    plots = list()
  }
  
  if("choiceLeft" %in% names(true_data)){
    true_data = true_data %>%
      mutate(choice = ifelse(choiceLeft == 1, "left", "right"),
             data_type = "true")
  }
  
  if("choiceLeft" %in% names(sim_data)){
    sim_data = sim_data %>%
      mutate(choice = ifelse(choiceLeft == 1, "left", "right"),
             data_type = "true")
  }
  
  if("leftQValue" %in% names(true_data)){
    true_data = true_data %>%
      rename(QVLeft = leftQValue, QVRight = rightQValue, EVLeft = leftLotteryEV, EVRight = rightLotteryEV) %>%
      select(subnum, EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime, data_type)
  }
  
  if("leftQValue" %in% names(sim_data)){
    sim_data = sim_data %>%
      rename(QVLeft = leftQValue, QVRight = rightQValue, EVLeft = leftLotteryEV, EVRight = rightLotteryEV) %>%
      select(subnum, EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime, data_type)
  }
  
  
  
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
  
  
  # Check 2: choice by value difference for extreme cases of probfractaldraw
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
      facet_grid(key ~ probFractalDraw)+
      theme(panel.grid = element_blank())
    
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
  }
  
  
  # Check 3: RT histograms overlaid with predicted densities
  if(3 %in% checks){
    if(compare_rts){
      p = sim_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
        mutate(data_type = "sim") %>%
        mutate(probFractalDraw = as.factor(probFractalDraw)) %>%
        ggplot(aes(reactionTime, color=data_type)) +
        geom_density()+
        geom_histogram(data=true_data, aes(reactionTime, y = ..density..), bins=20, alpha=.5)+
        facet_wrap(~probFractalDraw)+
        theme(legend.position = "bottom")+
        scale_color_manual(values=c("red",NA))+
        theme(panel.grid = element_blank())
    }
    else{
      p = sim_data %>%
        mutate(probFractalDraw = as.factor(probFractalDraw)) %>%
        ggplot(aes(reactionTime)) +
        geom_histogram(position="identity", bins=10) +
        facet_wrap(~probFractalDraw)+
        theme(panel.grid = element_blank())
    }
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
  }
  
  
  # Check 4: Average rt by probfractaldraw
  if(4 %in% checks){
    if(compare_rts){
      p = sim_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
        mutate(data_type = "sim") %>%
        rbind(true_data %>% select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime, data_type)) %>%
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
        labs(color="")+
        theme(legend.position = "bottom", panel.grid = element_blank())+
        labs(y="Mean Log RT", x="p(Fractal)")
        
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
        theme(panel.grid = element_blank())+
        labs(y="Mean Log RT", x="p(Fractal)")
    }
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
  }
  
  # Check 5: Logit slopes
  if(5 %in% checks){
    tmp = sim_data %>%
      select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
      mutate(probFractalDraw = as.factor(probFractalDraw),
             choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
             EVDiff = EVLeft - EVRight, 
             QVDiff = QVLeft - QVRight) %>%
      nest(data = -probFractalDraw)
    
    if(scale_logits){
      tmp = tmp %>%
        mutate(
          fit = map(data, ~ glm(choiceLeft ~ scale(EVDiff) + scale(QVDiff), data = .x, family=binomial(link="logit"))),
          tidied = map(fit, tidy)
        )
    } else {
      tmp = tmp %>%
        mutate(
          fit = map(data, ~ glm(choiceLeft ~ EVDiff + QVDiff, data = .x, family=binomial(link="logit"))),
          tidied = map(fit, tidy)
        )
    }
    
    tmp = tmp %>%
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
      theme(legend.position = "bottom", panel.grid = element_blank())+
      labs(y="Beta Estimate", x="p(Fractal)", color="")
    
    if(compare_logits){
      tmp_true = true_data %>%
        select(EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime) %>%
        mutate(probFractalDraw = as.factor(probFractalDraw),
               choiceLeft = ifelse(choice == "left", 1, ifelse(choice=="right", 0, NA)),
               EVDiff = EVLeft - EVRight, 
               QVDiff = QVLeft - QVRight) %>%
        nest(data = -probFractalDraw)
      
      if(scale_logits){
        tmp_true = tmp_true %>%
          mutate(
            fit = map(data, ~ glm(choiceLeft ~ scale(EVDiff) + scale(QVDiff), data = .x, family=binomial(link="logit"))),
            tidied = map(fit, tidy)
          )  
      } else {
        tmp_true = tmp_true %>%
          mutate(
            fit = map(data, ~ glm(choiceLeft ~ EVDiff + QVDiff, data = .x, family=binomial(link="logit"))),
            tidied = map(fit, tidy))
      }
      
      tmp_true = tmp_true %>% 
        unnest(tidied) %>%
        filter(term != "(Intercept)") %>%
        select(probFractalDraw, term, estimate, std.error)%>%
        mutate(data_type = "true")
      
      if(logit_facets){
        p = tmp %>%
          rbind(tmp_true) %>%
          ggplot(aes(probFractalDraw, estimate, col=term, group=term))+
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate +std.error), width=0.2)+
          geom_hline(aes(yintercept=0), linetype="dashed")+
          facet_wrap(~data_type)+
          scale_color_manual(values = cbbPalette[2:1])+
          theme(legend.position = "bottom", panel.grid = element_blank())+
          labs(y="Beta Estimate", x="p(Fractal)")
      } else{
        p = tmp %>%
          rbind(tmp_true) %>%
          mutate(line_group = paste0(term,data_type)) %>%
          ggplot(aes(probFractalDraw, estimate, col=term, group=line_group, alpha=data_type))+
          geom_point()+
          geom_line()+
          geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate +std.error), width=0.2)+
          geom_hline(aes(yintercept=0), linetype="dashed")+
          # facet_wrap(~data_type)+
          scale_color_manual(values = cbbPalette[2:1])+
          scale_alpha_manual(values = c(1, .3), guide="none")+
          theme(legend.position = "bottom", panel.grid = element_blank())+
          labs(y="Beta Estimate", x="p(Fractal)")
      }
      
      
      
    } 
    
    yrange = layer_scales(p)$y$range$range
    large_range = abs(yrange) > yrange_lim
    if(sum(large_range)>0){
      p = p+ylim(-5, yrange_lim)
    }
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
  }
  
  # Check 6: EV difference effect on RT
  if(6 %in% checks){
    
    tmp = sim_data %>%
      mutate(lottery_ev_diff = round(abs(EVLeft - EVRight),3),
             lottery_ev_diff = ifelse(lottery_ev_diff ==0, "no EV diff", ifelse(lottery_ev_diff == .2, "small EV diff", ifelse(lottery_ev_diff == .4, "large EV diff", NA))),
             lottery_ev_diff = factor(lottery_ev_diff, levels=c("no EV diff", "small EV diff", "large EV diff"), labels = c("no", "small", "large")),
             logRt = log(reactionTime),
             probFractalDraw = as.factor(probFractalDraw)) %>%
      group_by(probFractalDraw, lottery_ev_diff) %>%
      summarise(.groups = "keep",
                mean_logRt = mean(logRt),
                sem_logRt = sd(logRt)/sqrt(n())) %>%
      mutate(data_type="sim")
    
    if(compare_rts){
      
      tmp_true = true_data %>%
        mutate(lottery_ev_diff = round(abs(EVLeft - EVRight),3),
               lottery_ev_diff = ifelse(lottery_ev_diff ==0, "no EV diff", ifelse(lottery_ev_diff == .2, "small EV diff", ifelse(lottery_ev_diff == .4, "large EV diff", NA))),
               lottery_ev_diff = factor(lottery_ev_diff, levels=c("no EV diff", "small EV diff", "large EV diff"), labels = c("no", "small", "large")),
               logRt = log(reactionTime),
               probFractalDraw = as.factor(probFractalDraw)) %>%
        group_by(probFractalDraw, lottery_ev_diff) %>%
        summarise(.groups = "keep",
                  mean_logRt = mean(logRt),
                  sem_logRt = sd(logRt)/sqrt(n())) %>%
        mutate(data_type="true")
      
      tmp = rbind(tmp, tmp_true)
    }
    
    p = tmp %>%
      ggplot(aes(probFractalDraw, mean_logRt,color=lottery_ev_diff, alpha = data_type, shape=data_type))+
      geom_point(position=position_dodge(width=.5))+
      geom_errorbar(aes(ymin = mean_logRt - sem_logRt, ymax = mean_logRt + sem_logRt), width=.2,position=position_dodge(width=.5))+
      theme(legend.position = "bottom", panel.grid = element_blank())+
      labs(color="Lottery EV difference", y="Mean Log RT", x="p(Fractal)")+
      scale_color_manual(values = c(cbbPalette[3], cbbPalette[5:6]))+
      scale_alpha_manual(values=c(1, .5))
    
    if(length(unique(tmp$data_type))==1){
      p = p+
        guides(shape="none", alpha="none")
    }
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
  }
  
  # Check 7: QV difference effect on RT
  if(7 %in% checks){
    
    tmp = sim_data %>%
      mutate(logRt = log(reactionTime),
             QVDiff = abs(QVLeft - QVRight),
             diff_level = ifelse(QVDiff < quantile(QVDiff, probs=c(.33))[[1]], "small",
                                 ifelse(QVDiff > quantile(QVDiff, probs=c(.66))[[1]], "large", "medium")),
             diff_level = factor(diff_level, levels=c("small", "medium", "large")),
             probFractalDraw = as.factor(probFractalDraw)) %>%
      group_by(probFractalDraw, diff_level) %>%
      summarise(.groups = "keep",
                mean_logRt = mean(logRt),
                sem_logRt = sd(logRt)/sqrt(n())) %>%
      mutate(data_type="sim")
    
    if(compare_rts){
      tmp_true = true_data %>%
        # group_by(subnum) %>%
        mutate(logRt = log(reactionTime),
               QVDiff = abs(QVLeft - QVRight),
               diff_level = ifelse(QVDiff < quantile(QVDiff, probs=c(.33))[[1]], "small",
                                   ifelse(QVDiff > quantile(QVDiff, probs=c(.66))[[1]], "large", "medium")),
               diff_level = factor(diff_level, levels=c("small", "medium", "large")),
               probFractalDraw = as.factor(probFractalDraw)) %>%
        group_by(probFractalDraw, diff_level) %>%
        summarise(.groups = "keep",
                  mean_logRt = mean(logRt),
                  sem_logRt = sd(logRt)/sqrt(n())) %>%
        mutate(data_type="true")
      
      tmp = rbind(tmp, tmp_true)
    }
    
    p = tmp %>%
      ggplot(aes(probFractalDraw, mean_logRt,color=diff_level, alpha = data_type, shape=data_type))+
      geom_point(position=position_dodge(width=.5))+
      geom_errorbar(aes(ymin = mean_logRt - sem_logRt, ymax = mean_logRt + sem_logRt), width=.2,position=position_dodge(width=.5))+
      theme(legend.position = "bottom", panel.grid = element_blank())+
      labs(color="Fractal QV difference", y="Mean Log RT", x="p(Fractal)")+
      scale_color_manual(values = c(cbbPalette[3], cbbPalette[5:6]))+
      scale_alpha_manual(values=c(1, .5))
    
    if(length(unique(tmp$data_type))==1){
      p = p+
        guides(shape="none", alpha="none")
    }
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
    
  }
  
  # Check 8: Relevant error effect on RT
  if(8 %in% checks){
    
    tmp = sim_data %>%
      filter(probFractalDraw != 0.5) %>%
      mutate(leftLotterySubjBetter = distortedEVDiff > 0,
             leftFractSubjBetter = distortedQVDiff > 0,
             choseBetterSubjLott = ifelse(choice == "left" & leftLotterySubjBetter, "correct", ifelse(choice == "right" & !leftLotterySubjBetter, "correct", "incorrect")),
             choseBetterSubjFrac = ifelse(choice == "left" & leftFractSubjBetter, "correct", ifelse(choice == "right" & !leftFractSubjBetter, "correct", "incorrect")),
             logRt = log(reactionTime),
             fractalMoreRelevant = ifelse(probFractalDraw > .5, "fractal more relevant", "lottery more relevant"),
             fractalMoreRelevant = factor(fractalMoreRelevant, levels=c("lottery more relevant", "fractal more relevant"))) %>%
      select(fractalMoreRelevant, choseBetterSubjLott, choseBetterSubjFrac, logRt) %>%
      gather(key, value, -fractalMoreRelevant, -logRt) %>%
      group_by(fractalMoreRelevant, key, value) %>%
      summarise(.groups="keep",
                meanLogRt = mean(logRt),
                semLogRt = sd(logRt)/sqrt(n())) %>%
      mutate(data_type="sim") 
    
    if(compare_rts){
      
      tmp_true = true_data %>%
        filter(probFractalDraw != 0.5) %>%
        mutate(leftLotterySubjBetter = distortedEVDiff > 0,
               leftFractSubjBetter = distortedQVDiff > 0,
               choseBetterSubjLott = ifelse(choice == "left" & leftLotterySubjBetter, "correct", ifelse(choice == "right" & !leftLotterySubjBetter, "correct", "incorrect")),
               choseBetterSubjFrac = ifelse(choice == "left" & leftFractSubjBetter, "correct", ifelse(choice == "right" & !leftFractSubjBetter, "correct", "incorrect")),
               logRt = log(reactionTime),
               fractalMoreRelevant = ifelse(probFractalDraw > .5, "fractal more relevant", "lottery more relevant"),
               fractalMoreRelevant = factor(fractalMoreRelevant, levels=c("lottery more relevant", "fractal more relevant"))) %>%
        select(fractalMoreRelevant, choseBetterSubjLott, choseBetterSubjFrac, logRt) %>%
        gather(key, value, -fractalMoreRelevant, -logRt) %>%
        group_by(fractalMoreRelevant, key, value) %>%
        summarise(.groups="keep",
                  meanLogRt = mean(logRt),
                  semLogRt = sd(logRt)/sqrt(n())) %>%
        mutate(data_type =  "true")
      
      tmp = rbind(tmp, tmp_true)
      
    }
    
    p = tmp %>%
      mutate(correctBasedOn = ifelse(key == "choseBetterSubjLott", "Correct based on Lottery", "Correct based on Fractal"),
             correctBasedOn = factor(correctBasedOn, levels = c("Correct based on Lottery", "Correct based on Fractal"))) %>%
      ggplot(aes(fractalMoreRelevant, meanLogRt, color=value, alpha=data_type, shape=data_type))+
      geom_point(position=position_dodge(width=.5))+
      geom_errorbar(aes(ymin=meanLogRt-semLogRt, ymax=meanLogRt+semLogRt),width=.2,position=position_dodge(width=.5))+
      xlab("")+
      theme(legend.position = "bottom", panel.grid = element_blank())+
      scale_alpha_manual(values = c(1, .5))+
      scale_color_manual(values = c("blue", "red"))+
      facet_wrap(~correctBasedOn)+
      labs(y="Mean Log RT", color="")
    
    if(length(unique(tmp$data_type))==1){
      p = p+
        guides(shape="none", alpha="none")
    }
    
    if(return_plot){
      plots[[length(plots)+1]]=p
    } else{
      print(p)
    }
    
  }
  
  if(return_plot){
    return(plots)
  }
  
}