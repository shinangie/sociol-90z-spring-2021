# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(haven)

# read in NLSY97 data
nlsy97 <- read_dta("nlsy97/nlsy97.dta") %>%
  rename_all(str_to_lower)

# extract variable labels
nlsy97_varlabels <- nlsy97 %>%
  map(~ attr(.x, "label")) %>% 
  enframe() %>%
  transmute(name, label = str_to_lower(value))

# view variable labels
View(nlsy97_varlabels)

