# SQUIRE model for NCT Delhi bed capacity

library(squire)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

########################################### SETUP
#################################################

set.seed(212)

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

# Hospital and ICU bed capacities; using 80% of total
# https://cddep.org/wp-content/uploads/2020/04/State-wise-estimates-of-current-beds-and-ventilators_24Apr2020.pdf
hosp_bed <- 39455 * 0.8
ICU_bed <- 1973 * 0.8


################################# CONTACT MATRICES
##################################################

# Not using these for now; impossible to get fit
# with just contact matrix manipulation alone.

# Original squire matrix
squire_matrix <- india_params_list$contact_matrix_set[[1]]

# Phase 1 matrix (Mar 24 - May 03)
phase1_matrix <- read.csv("matrices/delhi/phase1_matrix.csv")

# Phase 3 matrix (May 03 - May 31)
phase3_matrix <- read.csv("matrices/delhi/phase3_matrix.csv")


#################################### CREATE MODELS
##################################################

# set up for parallelisation
future::plan(future::multiprocess())

# Dates of interventions
phase1 <- "2020-03-24"
phase3 <- "2020-05-03"

# FIRST MODEL

int_unique <- list(dates_change = c(phase1, phase3), 
                   R0_change = c(0.2, 0.5)) 

# Fit model
out <- calibrate(
  reporting_fraction = 0.8,
  data = df,
  R0_min = 2,
  R0_max = 7,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-12",
  day_step = 1,
  replicates = 10,  # Make sure this is 100 if final
  n_particles = 10, # Make sure this is 100 if final
  population = pop_vector,
  forecast = 0,
  baseline_contact_matrix = squire_matrix,
  date_R0_change = int_unique$dates_change,
  R0_change = int_unique$R0_change,
  baseline_hosp_bed_capacity = hosp_bed, 
  baseline_ICU_bed_capacity = ICU_bed,
)

# SECOND MODEL: 
  # Mar 23 to May 03 with phase1_matrix;
  # May 03 onwards with phase3_matrix;
  # death reporting_fraction = 1.0.
  # As the output of this one doesn't seem to
  # fit the data, we're not using it for now.

int_unique2 <- list(dates_change = c(phase1, phase3), 
                    matrix_change = list(
                      data.matrix(phase1_matrix), 
                      data.matrix(phase3_matrix))
                   )

out2 <- calibrate(
  reporting_fraction = 1,
  data = df,
  R0_min = 1,
  R0_max = 7,
  R0_step = 0.5,
  first_start_date = "2020-03-02",
  last_start_date = "2020-03-12",
  day_step = 1,
  replicates = 20,  # Make sure this is 100 if final
  n_particles = 20, # Make sure this is 100 if final
  population = pop_vector,
  forecast = 14, 
  baseline_contact_matrix = squire_matrix,
  contact_matrix_set = int_unique1$matrix_change,            # Ensure refers to right int_unique!
  date_contact_matrix_set_change = int_unique1$dates_change, # Ensure refers to right int_unique!
  baseline_hosp_bed_capacity = hosp_bed, 
  baseline_ICU_bed_capacity = ICU_bed,   
)

######################################### PLOTTING
##################################################

# Establish relevant  dates for annotating plots.
plotting_dates <- data.frame(date = as.Date(c(phase1, phase3)),
                             event = c("Lockdown 1.0", "Lockdown 3.0")
)

forecast <- 14

ifelse (forecast > 0,
        today <- data.frame(date = Sys.Date(), event = "Today"),
        today <- data.frame(date = as.Date(NA), event = NA)
)

# Plot death estimates with particle fit up until today
model_fit <- plot(out, "deaths", particle_fit = TRUE) +          
                  labs(title = "Model fit to daily death counts to date") + 
                  scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                               date_labels = "%b %d",               # mark x-ticks w/ month, day
                               limits = as.Date(c("2020-03-07", 
                                                  Sys.Date()))) +   # cut off viz at today's date
                  theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                                   hjust = 1)) +    
                  scale_y_continuous() +
                  ylim(c(0, max(out$scan_results$inputs$data$deaths) + 15))  # change vertical limits

model_fit

# Save viz
ggsave("visualisations/model_fit.png")

# Plot projected death counts
plot(out, "deaths", date_0 = max(df$date), 
     x_var = "date") +        
     labs(title = "Projected daily death counts") +
     ylab("Deaths") +
     xlab("Date") +
     scale_x_date(date_breaks = "2 week",             # x-tick every 2 weeks
                 date_labels = "%b %d",               # mark x-ticks w/ month, day
                 date_minor_breaks = "1 week") +      # unmarked grid lines for each week 
     theme(axis.text.x = element_text(angle = 45,     # x-axis on 45 deg angle
                                     hjust = 1)) +    
     scale_y_continuous(labels = comma) +
     geom_vline(xintercept = today$date,              # Plot today's date line
                linetype = 3,
                color = 'darkgray') +
     geom_text(data = today,                          # Annotate today line
               mapping = aes(x = date, y = 1000, label = event), 
               size = 3, angle = 90, vjust = -0.5, hjust = 0, 
               color = 'darkmagenta', alpha = 0.5,
               fontface = 'italic') +
     theme(legend.position = "none")                   # suppress legend


# Save viz
ggsave("visualisations/projected_deaths.png")

# Plot projected hospital bed usage
plot(out, var_select = c("hospital_occupancy"), 
          date_0 = max(df$date), x_var = "date") +
      labs(title = "Projection for hospital bed occupancy") +
      ylab("No. of beds") +
      xlab("Date") +
      geom_hline(yintercept = hosp_bed , linetype = 4) + # show bed capacity line
      annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
               y = hosp_bed * 1.05, 
               label = "80% bed capacity", size = 3, 
               fontface = 'italic', hjust = 0) +
      geom_segment(data = plotting_dates,                # Add lockdown lines
                    mapping = aes(x = date, xend = date,
                                  y = 0, yend = hosp_bed),
                    color = 'darkgrey',
                    alpha = 0.7,
                    size = 1) +
      geom_text(data = plotting_dates,                   # Annotate lockdown lines
                mapping = aes(x = date, 
                              y = hosp_bed * 0.9, 
                              label = event), 
                size = 3, angle = 90, vjust = -0.5, hjust = .9, 
                color = 'darkgrey', alpha = 0.7, 
                fontface = 'bold') +
      geom_segment(data = today,                      # Plot "today" line
                   mapping = aes(x = date, xend = date,
                                 y = 0, yend = hosp_bed),
                   color = 'darkmagenta',
                   alpha = 0.5,
                   size = 1) +
      geom_text(data = today,                          # Annotate "today" line
                mapping = aes(x = date, y = 0, label = event), 
                size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                color = 'darkmagenta', alpha = 0.5,
                fontface = 'italic') +
      scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",               # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",        # unmarked grid lines for each week
                   ) +
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +
      scale_y_continuous(n.breaks = 8, 
                         limits = c(0, hosp_bed * 1.1)) + 
      theme(legend.position = "none")                   # suppress legend

# Save viz
ggsave("visualisations/projected_hosp_occ.png")

# Plot projected ICU bed usage
plot(out, var_select = c("ICU_occupancy"), 
                    date_0 = max(df$date), x_var = "date") +
                    labs(title = "Projection for ICU bed occupancy") +
                    ylab("No. of beds") +
                    xlab("Date") +
                    geom_hline(yintercept = ICU_bed, linetype = 4) +   # show bed capacity line
                    annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
                         y = ICU_bed * 1.05, 
                         label = "80% bed capacity", size = 3, 
                         fontface = 'italic', hjust = 0) +
                    geom_segment(data = plotting_dates,                # Add lockdown lines
                             mapping = aes(x = date, xend = date,
                                           y = 0, yend = ICU_bed),
                             color = 'darkgrey',
                             alpha = 0.8,
                             size = 1) +
                    geom_text(data = plotting_dates,                   # Annotate lockdown lines
                              mapping = aes(x = date, 
                                            y = ICU_bed * 0.9, 
                                            label = event), 
                              size = 3, angle = 90, vjust = -0.5, hjust = 0.9, 
                              color = 'darkgrey', alpha = 0.8, 
                              fontface = 'bold') +
                    geom_segment(data = today,
                                 mapping = aes(x = date, xend = date,
                                               y = 0, yend = ICU_bed),
                                 color = 'darkmagenta',
                                 alpha = 0.3,
                                 size = 1) +
                    geom_text(data = today,                          # Annotate today line
                              mapping = aes(x = date, 
                                            y = 0, 
                                            label = event), 
                              size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                              color = 'darkmagenta', alpha = 0.5,
                              fontface = 'italic') +
                    scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                             date_labels = "%b %d",                   # mark x-ticks w/ month, day
                             date_minor_breaks = "1 week",            # unmarked grid lines for each week
                    ) +
                    theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                                 hjust = 1)) +
                    scale_y_continuous(n.breaks = 8,
                                       limits = c(0, ICU_bed * 1.1)) + 
                    theme(legend.position = "none")                   # suppress legend


# Save viz
ggsave("visualisations/projected_ICU_occ.png")

############################## PROJECTION PLOTTING
##################################################

# input.hosp_bed_capacity_change <- 1.3
# input.ICU_bed_capacity_change <- 1.3
# 
# hosp_bed_capacity_change <- c(1, input.hosp_bed_capacity_change)
# ICU_bed_capacity_change <- c(1, input.ICU_bed_capacity_change)

p <- projections(r = out, 
                 time_period = 35,
                 hosp_bed_capacity_change = 1, 
                 tt_hosp_beds = 0,
                 ICU_bed_capacity_change = 1, 
                 tt_ICU_beds = 0,
                 )

projection_plotting(
      r_list = list(p),
      var_select = c("hospital_occupancy"),
      scenarios = c(""),
      add_parms_to_scenarios = FALSE,
      ci = TRUE,
      date_0 = max(df$date), 
      x_var = "date") +
  labs(title = "Projection for hospital bed occupancy") +
  ylab("No. of beds") +
  xlab("Date") +
  geom_hline(yintercept = hosp_bed , linetype = 4) + # show bed capacity line
  annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
           y = hosp_bed * 1.05, 
           label = "80% bed capacity", size = 3, 
           fontface = 'italic', hjust = 0) +
  geom_segment(data = plotting_dates,                # Add lockdown lines
               mapping = aes(x = date, xend = date,
                             y = 0, yend = hosp_bed),
               color = 'darkgrey',
               alpha = 0.7,
               size = 1) +
  geom_text(data = plotting_dates,                   # Annotate lockdown lines
            mapping = aes(x = date, 
                          y = hosp_bed * 0.9, 
                          label = event), 
            size = 3, angle = 90, vjust = -0.5, hjust = .9, 
            color = 'darkgrey', alpha = 0.7, 
            fontface = 'bold') +
  geom_segment(data = today,                      # Plot "today" line
               mapping = aes(x = date, xend = date,
                             y = 0, yend = hosp_bed),
               color = 'darkmagenta',
               alpha = 0.3,
               size = 1) +
  geom_text(data = today,                          # Annotate "today" line
            mapping = aes(x = date, y = hosp_bed * 0.9, label = event), 
            size = 3, angle = 90, vjust = -0.5, hjust = .9, 
            color = 'darkmagenta', alpha = 0.5,
            fontface = 'italic') +
  scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
               date_labels = "%b %d",               # mark x-ticks w/ month, day
               date_minor_breaks = "1 week",        # unmarked grid lines for each week
  ) +
  theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                   hjust = 1)) +
  scale_y_continuous(n.breaks = 8, 
                     limits = c(0, hosp_bed * 1.1)) + 
  theme(legend.position = "none")                   # suppress legend

