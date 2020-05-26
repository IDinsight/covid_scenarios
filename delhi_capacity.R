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

# Setup data
STATE = 'DL'

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))

# Hospital and ICU bed capacities 
# https://cddep.org/wp-content/uploads/2020/04/State-wise-estimates-of-current-beds-and-ventilators_24Apr2020.pdf
hosp_bed <- 24383 
ICU_bed <- 1219 

##################################### CREATE MODEL
##################################################

# Dates of interventions
natl.lockdown <- "2020-03-24"
natl.lockdown.relaxed <- "2020-05-03"

int_unique <- list(dates_change = c(natl.lockdown, natl.lockdown.relaxed), 
                   change = c(0.2, 0.3))

out <- calibrate(
  data = df,
  R0_min = 1,
  R0_max = 10,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-12",
  day_step = 1,
  replicates = 100,
  n_particles = 20,
  population = pop_vector,
  forecast = 70,
  baseline_contact_matrix  = india_params_list$contact_matrix_set,
  R0_change = int_unique$change,
  date_R0_change = int_unique$dates_change,
  baseline_hosp_bed_capacity = hosp_bed,
  baseline_ICU_bed_capacity = ICU_bed,
)

######################################### PLOTTING
##################################################

plot(out, var_select = "hospital_occupancy", 
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
#      geom_vline(xintercept = as.Date("2020-03-24"),    # show lockdown line
#              linetype=4) +    
#      annotate("text", x = as.Date("2020-03-23"),       # show lockdown text
#               y = 2000, 
#               label = "Lockdown\nbegins", size = 3, 
#               fontface = 'italic', hjust = 1) +
#      geom_vline(xintercept = as.Date("2020-04-03"),    # show lockdown relaxed line
#                 linetype=4) +    
#      annotate("text", x = as.Date("2020-04-02"),       # show lockdown relaxed text
#               y = 2000, 
#               label = "Lockdown\nbegins", size = 3, 
#               fontface = 'italic', hjust = 1) +  
      scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",               # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",        # unmarked grid lines for each week
                   ) +
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +
      ylim(0, hosp_bed + 3500) +
      theme(legend.position = "none")                   # suppress legend


ggsave("tmp/hosp_occupancy.png")

plot(out, var_select = "ICU_occupancy", 
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
#               label = "Lockdown\nbegins", size = 3, 
#               fontface = 'italic', hjust = 1) +
#      geom_vline(xintercept = as.Date("2020-04-03"),    # show lockdown relaxed line
#                 linetype=4) +    
#      annotate("text", x = as.Date("2020-04-02"),       # show lockdown relaxed text
#               y = 2000, 
#               label = "Lockdown\nbegins", size = 3, 
#               fontface = 'italic', hjust = 1) +  
      scale_x_date(date_breaks = "2 week",             # x-tick every 2 weeks
                   date_labels = "%b %d",              # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",       # unmarked grid lines for each week
                   ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, ICU_bed + 200) +
      theme(legend.position = "none",)                 # suppress legend


ggsave("tmp/ICU_occupancy.png")

