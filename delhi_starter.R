# SQUIRE model for NCT Delhi

library(squire)
library(tidyverse)
library(ggplot2)

pop_dist <- read.csv("./data/delhi_pop.csv")

pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
  as.character %>% 
  str_remove_all(",") %>% 
  as.integer
state_wise_data <- read.csv("./data/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

# Setup data
STATE = 'DL'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))