library(tidyverse)

extract_var_for_stan <- function(data, var) {
  as.matrix(data %>% 
              select(subnum, {{ var }}) %>%
              group_by(subnum) %>%
              mutate(trial= 1:n()) %>%
              spread(trial, {{ var }}) %>%
              ungroup()%>%
              select(-subnum) %>%
              replace(is.na(.), -1))
}