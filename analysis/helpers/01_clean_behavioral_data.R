library(here)
library(tidyverse)

# Check if data is loaded, load if not
if (!exists('beh_data')){
  helpers_path = here('analysis/helpers/')
  source(paste0(helpers_path,'00_get_behavioral_data.R'))
}

# Clean raw behavioral data
clean_beh_data = beh_data %>%
  filter(responded == 1,
         reactionTime > .2)

rm(beh_data)

