# SQUIRE model for NCT Delhi

library(squire)
library(tidyverse)
library(ggplot2)

pop_dist <- read.csv("./data/delhi_pop.csv")

pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
  as.character %>% 
  str_remove_all(",") %>% 
  as.integer
state_wise_data <- read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

# Setup data
STATE = 'DL'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))

# Note date of national lockdown
natl.lockdown <- as.numeric(as.Date("2020-03-24"))
natl.lockdown.relaxed <- as.numeric(as.Date("2020-05-03"))

int_unique <- list(dates_change = c("2020-03-17", "2020-03-24", "2020-05-03"), # Day of lockdown
                   change = c(0.5, 0.2, 0.3))

# Basic model; India generic contact matrix
out <- calibrate(
  data = df,
  R0_min = 2,
  R0_max = 10,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-12",
  day_step = 1,
  replicates = 20,
  n_particles = 20,
  population = pop_vector,
  forecast = 14,
  baseline_contact_matrix  = india_params_list$contact_matrix_set,
  R0_change = int_unique$change,
  date_R0_change = int_unique$dates_change
)

# Save model locally to avoid re-running grid search in new sessions
#save(out, file = "/tmp/delhi_out_1205.rda") # this doesn't work??
# Reload model if applicable
#out <- load(file = "/tmp/delhi_out_1205.rda")

plot(out$scan_results)

# Plot with particle fit
plot(out, 'deaths', particle_fit = TRUE) + 
  ggtitle(label = "Delhi: Deaths per day") +
  geom_vline(xintercept = natl.lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 10, 
           label = "Nat'l lock-\ndown begins", size = 3, 
           fontface = 'italic', hjust = 1) +
  geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
  annotate("text", x = as.Date("2020-05-01"), y = 10, 
           label = "Nat'l lock-\ndown relaxed", size = 3, 
           fontface = 'italic', hjust = 1) 


# Plot Delhi infections
plot(out, 'infections', date_0 = max(df$date), x_var = "date") + 
  labs(title = "Estimated new cases in Delhi, based on known deaths",
       #subtitle = "",
       caption = "[caption]") +
  ylab("Daily new infections") +
  geom_line(data = df, aes(x = date, y = cases)) +
  geom_vline(xintercept = natl.lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 2000, 
           label = "Nat'l lock-\ndown begins", size = 3, 
           fontface = 'italic', hjust = 1) +
  geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
  annotate("text", x = as.Date("2020-05-01"), y = 2000, 
           label = "Nat'l lock-\ndown relaxed", size = 3, 
           fontface = 'italic', hjust = 1) +
  theme(legend.position = "none")  # suppress legend



