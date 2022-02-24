library(tidyverse)

get_optim_out = function(model_, data_, optim_out_path_, iters_ = TRUE){
  
  fns = list.files(optim_out_path_)
  
  if(iters_){
    # List all files for this model and dataset combination
    
    fns = fns[grepl(model_, fns) & grepl(paste0(data_, '_'), fns) & grepl("iter", fns)]
    
    # Read in all starting and converged values
    out = data.frame()
    for(i in 1:length(fns)){
      tmp = read.csv(paste0(optim_out_path_, fns[i]))
      start_row = tmp[1,]
      end_row = tmp[nrow(tmp),]
      tmp_row = rbind(start_row, end_row)
      tmp_row$kernel = i
      out = rbind.all.columns(out, tmp_row)  
    }
    
  } else {
    
    fns = fns[grepl(model_, fns) & grepl(paste0(data_, '_'), fns) & grepl("par", fns)]
    
    out = data.frame()
    for(i in 1:length(fns)){
      tmp = read.csv(paste0(optim_out_path_, fns[i]))
      
      # This makes it independent of model parameters but the "Param" columns would need to be renamed
      for(j in 1:nrow(tmp)){
        tmp$key[j] = paste0("Param",j)
      }
      tmp = tmp %>% spread(key, x)
      out = rbind.all.columns(out, tmp)  
    }
  }
  
  
  return(out)
}