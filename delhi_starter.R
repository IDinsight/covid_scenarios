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

'
# Original India contact matrix
india_default_matrix <- india_params_list$contact_matrix_set
names <- seq(5, 80, by = 5) 
original_matrix <- as.data.frame(india_default_matrix[[1]])
rownames(original_matrix) <- names
colnames(original_matrix) <- names

original_matrix_long <- original_matrix %>% 
  rownames_to_column(var = "age_grp") %>%
  gather("other_age_grp", "contact", -age_grp) %>%
  transform(age_grp = as.numeric(age_grp)) %>%
  transform(other_age_grp = as.numeric(other_age_grp))

ggplot(original_matrix_long, aes(x = age_grp, y = other_age_grp, fill = contact)) + 
  geom_tile(colour = "white") +
  viridis::scale_fill_viridis() +
  theme_minimal() +
  ggtitle("Contact matrix (India default)")
'

# Visualize contact matrix

matrix_viz <- function(matrix_df) {
  
  names <- seq(5, 80, by = 5) 
  rownames(matrix_df) <- names
  colnames(matrix_df) <- names
  
  matrix_df_long <- matrix_df %>% 
    rownames_to_column(var = "age_grp") %>%
    gather("other_age_grp", "contact", -age_grp) %>%
    transform(age_grp = as.numeric(age_grp)) %>%
    transform(other_age_grp = as.numeric(other_age_grp))
  
  return(
  ggplot(matrix_df_long, aes(x = age_grp, y = other_age_grp, fill = contact)) + 
    geom_tile(colour = "white") +
    
    viridis::scale_fill_viridis( 
      # HARD-CODE to make any vals above 3 show up as maximal yellow
      rescaler = function(x, to = c(0, 1), from = NULL) {
        ifelse(x < 3, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 3)), 1)}
      ) + 
    theme_minimal()
  )
  
}


# Diminish all contacts to 20% but open up schools for 5-9, 10-14, 15-19

open_schools <- function(old_matrix_df, reduction_rate) {
  
  # Take original contact matrix (dataframe), return
  # new one at given contact reduction (e.g. 0.2, 0.5),
  # but keep o.g. values for 5-9, 10-14, 15-19 age groups.
  
  
  new_matrix_df <- old_matrix * 0.2
  
  for (i in seq(1, 3)) {
    new_matrix_df[i, i] <- old_matrix_df[i, i]
  }
  
  return(new_matrix_df)
}

india_default_matrix_df <- as.data.frame(india_default_matrix[[1]])

twenty_perc_df <- original_matrix_df * 0.2
twenty_perc_schools_open_df <- open_schools(original_matrix_df, 0.2)

matrix_viz(india_default_matrix_df) +
  ggtitle("Contact matrix: India standard contact levels")

matrix_viz(twenty_perc_df) +
  ggtitle("Contact matrix: 20% of India standard contact levels")

matrix_viz(twenty_perc_schools_open_df) +
  ggtitle("Contact matrix: 20% of India standard contact levels, \nwith schools open")
