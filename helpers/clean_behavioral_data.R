# Check if data is loaded, load if not
if (!exists('beh_data')){
  helpers_path = '~/Dropbox/RangelLab/DescribedVsLearned/helpers/'
  source(paste0(helpers_path,'get_behavioral_data.R'))
}

library(tidyverse)

# Clean raw behavioral data
clean_beh_data = beh_data %>%
  filter(responded == 1,
         reactionTime > .2)

rm(beh_data)

