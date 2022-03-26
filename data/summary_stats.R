##-----------------------------------------------------------------------------#
#' Look at summary information of the dcis fiber feature data 
#' 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(skimr)

## Pull in data ---------------------------------------------------------------#

d <- readRDS(here("data/raw/dcis-fiber-features.rds"))

## Summary statistics ---------------------------------------------------------#

# number of patients 
length(unique(d$pid))
# 244

# number of slides
d %>% 
  select(pid, TN) %>% distinct() %>% 
  nrow()
# 348

# number of spots
d %>% 
  select(pid, sid, TN) %>% distinct() %>% 
  nrow()
# 1583 

# number of spots per slide
d %>% 
  select(pid, sid, TN) %>% distinct() %>% 
  group_by(pid, TN) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  skim()
# min = 1, max = 8, mean = 4.55


# number of fibers per spot
d %>% 
  group_by(pid, sid, TN) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  skim()
# min = 73, max = 1092, mean = 558



colnames(d)
