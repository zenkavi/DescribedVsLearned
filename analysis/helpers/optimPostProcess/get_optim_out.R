library(tidyverse)

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

get_optim_out = function(model_, data_, optim_out_path_, iters_ = TRUE){
  
  fns = list.files(optim_out_path_)
  out = data.frame()
  
  if(iters_){
    # List all files for this model and dataset combination
    
    fns = fns[grepl(model_, fns) & grepl(paste0(data_, '_'), fns) & grepl("iter", fns)]
    
    if(length(fns)>0){
      # Read in all starting and converged values
      
      for(i in 1:length(fns)){
        tmp = read.csv(paste0(optim_out_path_, fns[i]))
        start_row = tmp[1,]
        end_row = tmp[nrow(tmp),]
        tmp_row = rbind(start_row, end_row)
        tmp_row$kernel = i
        out = rbind.all.columns(out, tmp_row)  
      }
    } else{
      print(paste0("No optim_out files for ", data_))
    }
    
  } else {
    
    fns = fns[grepl(model_, fns) & grepl(paste0(data_, '_'), fns) & grepl("par", fns)]
    
    if(length(fns) > 0){
      for(i in 1:length(fns)){
        tmp = read.csv(paste0(optim_out_path_, fns[i]))
        
        # This makes it independent of model parameters but the "Param" columns would need to be renamed
        for(j in 1:nrow(tmp)){
          tmp$key[j] = paste0("Param",j)
        }
        tmp = tmp %>% spread(key, x)
        out = rbind.all.columns(out, tmp)  
      }
    } else{
      print(paste0("No optim_out files for ", data_))
    }
  }
  
  
  return(out)
}