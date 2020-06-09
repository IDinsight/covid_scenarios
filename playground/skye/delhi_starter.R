# SQUIRE model for NCT Delhi

library(squire)
library(tidyverse)
library(ggplot2)

########################################### SETUP
#################################################

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

################################# CONTACT MATRICES
##################################################

# Diminish all contacts to 20% but open up schools for 5-9, 10-14, 15-19

open_schools <- function(old_matrix_df, reduction_rate) {
  
  # Take original contact matrix (dataframe), return
  # new one at given contact reduction (e.g. 0.2, 0.5),
  # but keep o.g. values for 5-9, 10-14, 15-19 age groups.
  
  new_matrix_df <- old_matrix_df * 0.2
  
  for (i in seq(1, 3)) {
    new_matrix_df[i, i] <- old_matrix_df[i, i]
  }
  
  return(new_matrix_df)
}

india_default_matrix_df <- as.data.frame(india_params_list$contact_matrix_set[[1]])

fifty_perc_df <- india_default_matrix_df * 0.5
twenty_perc_df <- india_default_matrix_df * 0.2
twenty_perc_schools_open_df <- open_schools(india_default_matrix_df, 0.2)


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
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = 'bold')) +
      coord_fixed(ratio = 1.2) +
      xlab("Age group") +
      ylab("Other age group")
  )
  
}

# Visualize matrices for different options

matrix_viz(india_default_matrix_df) +
  ggtitle("India standard \ncontact patterns")
  #labs(subtitle = bquote("Contact behaviour based on 2018 Haryana survey"^1))

ggsave("tmp/matrix_india_standard.png")

matrix_viz(twenty_perc_df) +
  ggtitle("Simple lockdown assumption \n(20% standard contact)")
  #labs(subtitle = bquote("20% of typical contact behaviour"^2))

ggsave("tmp/matrix_20_percent.png")

matrix_viz(fifty_perc_df) +
  ggtitle("Simple relaxed lockdown \nassumption") 
  #labs(subtitle = "50% of typical contact behaviour")

ggsave("tmp/matrix_50_percent.png")

matrix_viz(twenty_perc_schools_open_df) +
  ggtitle("Extended lockdown with \nschools open") 
       #subtitle = bquote("General public at 20% of standard behaviour; children at 100%"^3)) +

ggsave("tmp/matrix_20_percent_children.png")

###################################### NEW MODELS
#################################################

# With 20% contact under lockdown, and full release afterwards

int_unique1 <- list(dates_change = c("2020-03-24", "2020-05-10"), 
                    change = list(data.matrix(twenty_perc_df), 
                                 data.matrix(india_default_matrix_df)))

out1 <- calibrate(
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
  baseline_contact_matrix = data.matrix(india_default_matrix_df),
  contact_matrix_set = int_unique1$change,
  date_contact_matrix_set_change = int_unique1$dates_change,
)

# With 20% contact under lockdown, and 50% after relaxing

int_unique2 <- list(dates_change = c("2020-03-24", "2020-05-10"), 
                    change = list(data.matrix(twenty_perc_df), 
                                  data.matrix(fifty_perc_df)))

out2 <- calibrate(
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
  baseline_contact_matrix = data.matrix(india_default_matrix_df),
  contact_matrix_set = int_unique2$change,
  date_contact_matrix_set_change = int_unique2$dates_change,
)

# With 20% contact under lockdown, and only schoolchildren
# resuming full contact after May 10th (demo purposes only; a lot
# of wonky assumptions here)

int_unique3 <- list(dates_change = c("2020-03-24", "2020-05-10"), 
                    change = list(data.matrix(twenty_perc_df), 
                                  data.matrix(twenty_perc_schools_open_df)))

out3 <- calibrate(
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
  baseline_contact_matrix = data.matrix(india_default_matrix_df),
  contact_matrix_set = int_unique3$change,
  date_contact_matrix_set_change = int_unique3$dates_change,
)

############################## NEW MODEL PLOTTING
#################################################

# With 20% contact under lockdown, and full release afterwards

plot(out1, 'infections', date_0 = max(df$date), x_var = "date") + 
  labs(title = "Projected daily infections, given lockdown total cessation",
       subtitle = "Assume no social distancing after May 10") +
  ylab("Daily new infections") +
  #geom_line(data = df, aes(x = date, y = cases)) +
  geom_vline(xintercept = natl.lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 420000, 
           label = "Nat'l lock-\ndown begins\n(24/03)", size = 3, 
           fontface = 'italic', hjust = 1) +
  geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
  annotate("text", x = as.Date("2020-05-09"), y = 420000, 
           label = "Nat'l lock-\ndown ends\n(10/05)", size = 3, 
           fontface = 'italic', hjust = 1) +
  theme(legend.position = "none")  # suppress legend

ggsave("tmp/may_10_lockdown_ends.png")

# With 20% contact under lockdown, and 50% after relaxing

plot(out2, 'infections', date_0 = max(df$date), x_var = "date") + 
  labs(title = "Projected daily infections, given relaxed social distancing",
       subtitle = "Assume 50% of standard contact resumes 10 May") +
  ylab("Daily new infections") +
  #geom_line(data = df, aes(x = date, y = cases)) +
  geom_vline(xintercept = natl.lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 40000, 
           label = "Nat'l lock-\ndown begins\n(24/03)", size = 3, 
           fontface = 'italic', hjust = 1) +
  geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
  annotate("text", x = as.Date("2020-05-09"), y = 40000, 
           label = "Nat'l lock-\ndown relaxed\n(10/05)", size = 3, 
           fontface = 'italic', hjust = 1) +
  theme(legend.position = "none")  # suppress legend

ggsave("tmp/may_10_lockdown_relax.png")

# With 20% contact under lockdown, and only schoolchildren
# resuming full contact after May 10th 

plot(out3, 'infections', date_0 = max(df$date), x_var = "date") + 
  labs(title = "Projected daily infections, given only school reopening",
       subtitle = "Assume adults stay in lockdown but students 5-19 return to school") +
  ylab("Daily new infections") +
  #geom_line(data = df, aes(x = date, y = cases)) +
  geom_vline(xintercept = natl.lockdown, linetype=4) +
  annotate("text", x = as.Date("2020-03-23"), y = 35000, 
           label = "Nat'l lock-\ndown begins\n(24/03)", size = 3, 
           fontface = 'italic', hjust = 1) +
  geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
  annotate("text", x = as.Date("2020-05-09"), y = 35000, 
           label = "Students return\nto school\n(10/05)", size = 3, 
           fontface = 'italic', hjust = 1) +
  theme(legend.position = "none")  # suppress legend

ggsave("tmp/may_10_school_resumes.png")



