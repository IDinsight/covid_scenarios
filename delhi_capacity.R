# SQUIRE model for NCT Delhi bed capacity

library(squire)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

########################################### SETUP
#################################################

pop_dist <- read.csv("./data/delhi_pop.csv")

pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
  as.character %>% 
  str_remove_all(",") %>% 
  as.integer
state_wise_data <- read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

# Set up data
STATE = 'DL'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))

# Hospital and ICU bed capacities 
# https://cddep.org/wp-content/uploads/2020/04/State-wise-estimates-of-current-beds-and-ventilators_24Apr2020.pdf
hosp_bed <- 39455
ICU_bed <- 1973

################################# CONTACT MATRICES
##################################################

# Original squire matrix
squire_matrix <- as.data.frame(india_params_list$contact_matrix_set)

# Phase 1 matrix (Mar 24 - May 03)
phase1_matrix <- read.csv("matrices/delhi/phase1_matrix.csv")

# Phase 3 matrix (May 03 - May 31)
phase3_matrix <- read.csv("matrices/delhi/phase3_matrix.csv")

#################################### CREATE MODELS
##################################################

# Dates of interventions
phase1 <- "2020-03-24"
phase3 <- "2020-05-03"

# FIRST SCENARIO: 
  # Mar 23 to May 03 with phase1_matrix;
  # May 03 onwards with phase3_matrix;
  # death reporting_fraction = 0.8.

int_unique1 <- list(dates_change = c(phase1, phase3), 
                    matrix_change = c(phase1_matrix, phase3_matrix)
                   )    

out1 <- calibrate(
  reporting_fraction = 0.8,
  data = df,
  R0_min = 1,
  R0_max = 10,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-12",
  day_step = 1,
  replicates = 20,  # Make sure this is 100 if final
  n_particles = 20, # Make sure this is 100 if final
  population = pop_vector,
  forecast = 70,
  baseline_contact_matrix = squire_matrix,
  contact_matrix_set = int_unique$matrix_change,
  date_contact_matrix_set_change = int_unique$dates_change,
  baseline_hosp_bed_capacity = hosp_bed * 0.8, # 80% of total
  baseline_ICU_bed_capacity = ICU_bed * 0.8,   # 80% of total
)

######################################### PLOTTING
##################################################

plot(out1, var_select = "hospital_occupancy", 
          date_0 = max(df$date), x_var = "date") +
      labs(title = "Projection for hospital bed occupancy",
           subtitle = "Assuming lockdown on 24 Mar and mild relaxation on 3 May") +
      ylab("No. of beds") +
      xlab("Date") +
      geom_hline(yintercept = hosp_bed, linetype = 4) + # show bed capacity line
      annotate("text", x = as.Date("2020-03-01"),       # show bed capacity text
               y = hosp_bed + 1800, 
               label = "Hospital bed capacity", size = 3, 
               fontface = 'italic', hjust = 0) +
     geom_vline(xintercept = as.Date("2020-03-24"),    # show lockdown line
             linetype=4) +
     annotate("text", x = as.Date("2020-03-23"),       # show lockdown text
              y = 2000,
              label = "Lockdown 1.0", size = 3,
              fontface = 'italic', hjust = 1) +
     geom_vline(xintercept = as.Date("2020-05-03"),    # show lockdown relaxed line
                linetype=4) +
     annotate("text", x = as.Date("2020-05-02"),       # show lockdown relaxed text
              y = 2000,
              label = "Lockdown 3.0", size = 3,
              fontface = 'italic', hjust = 1) +
      scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",               # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",        # unmarked grid lines for each week
                   ) +
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +
      ylim(0, hosp_bed + 3500) +
      theme(legend.position = "none")                   # suppress legend


ggsave("tmp/hosp_occupancy.png")

plot(out1, var_select = "ICU_occupancy", 
              date_0 = max(df$date), x_var = "date") + 
      labs(title = "Projection for ICU bed occupancy",
           subtitle = "Assuming lockdown on 24 Mar and mild relaxation on 3 May") + 
      ylab("No. of beds") +
      xlab("Date") + 
      geom_hline(yintercept = ICU_bed, linetype = 4) +   # show bed capacity line
      annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
               y = ICU_bed + 100, 
               label = "ICU bed capacity", size = 3, 
               fontface = 'italic', hjust = 0) +
#      geom_vline(xintercept = as.Date("2020-03-24"),    # show lockdown line
#              linetype=4) +    
#      annotate("text", x = as.Date("2020-03-23"),       # show lockdown text
#               y = 2000, 
#               label = "Lockdown 1.0", size = 3, 
#               fontface = 'italic', hjust = 1) +
#      geom_vline(xintercept = as.Date("2020-05-03"),    # show lockdown relaxed line
#                 linetype=4) +    
#      annotate("text", x = as.Date("2020-05-02"),       # show lockdown relaxed text
#               y = 2000, 
#               label = "Lockdown 3.0", size = 3, 
#               fontface = 'italic', hjust = 1) +  
      scale_x_date(date_breaks = "2 week",             # x-tick every 2 weeks
                   date_labels = "%b %d",              # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",       # unmarked grid lines for each week
                   ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, ICU_bed + 200) +
      theme(legend.position = "none",)                 # suppress legend


ggsave("tmp/ICU_occupancy.png")

