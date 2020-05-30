#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(shiny)

#-----------------------------------------------#
# DO THINGS THAT DO NOT REQUIRE ANY SHINY INPUT #
#-----------------------------------------------#
# non-data inputs
set.seed(212)

# read in data
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

# set up for parallelisation
future::plan(future::multiprocess())

# Dates of interventions
phase1 <- "2020-03-24"
phase3 <- "2020-05-03"

# FIRST MODEL: 
# Mar 23 to May 03 with 0.2 R0;
# May 03 onwards with 0.4 R0;
# death reporting_fraction = 1.0


int_unique <- list(dates_change = c(phase1, phase3), 
                   R0_change = c(0.2, 0.5)) 


#---------#
# R SHINY #
#---------#

# Define the UI
ui = fluidPage(
  titlePanel(
    fluidRow(
      column(6, "Delhi COVID-19 Simulator", style='font-size:100%; font-weight: bold'), # Website title
      column(3, img(height = 100, src = "delhi_govt.png")),
      column(3, img(height = 100, src = "idinsight.png"))
      )
  ),
  sidebarPanel(
    actionButton("run_model", "Run Model", style='padding:10px; font-size:100%; font-weight: bold'),
    tags$hr(),
    dateInput(label = "Date of ICU bed change:",
                              inputId = "date_ICU_bed_capacity_change",
                              format = "yyyy-mm-dd",
                              value = NULL),
                    numericInput(label = "Number of ICU beds after change:",
                                 inputId = "ICU_bed_capacity",
                                 min = 0,
                                 step = 10,
                                 value = as.integer(ICU_bed)),
                    dateInput(label = "Date of hospital bed change:",
                              inputId = "date_hosp_bed_capacity_change",
                              format = "yyyy-mm-dd",
                              value = NULL),
                    numericInput(label = "Number of hospital beds after change:",
                                 inputId = "hosp_bed_capacity",
                                 min = 0,
                                 step = 10,
                                 value = as.integer(hosp_bed)),
                    numericInput(label = "Reporting fraction:",
                                 inputId = "reporting_fraction",
                                 min = 0,
                                 max = 1,
                                 step = 0.01,
                                 value = 0.8),
                    tags$hr(),
                    numericInput(label = "Replicates:",
                                 inputId = "replicates",
                                 min = 0,
                                 step = 1,
                                 value = 100),
                    numericInput(label = "N particles:",
                                 inputId = "n_particles",
                                 min = 0,
                                 step = 1,
                                 value = 100),
                    width = 4),
  mainPanel(
    fluidRow(
      plotOutput(outputId = "plot1"),
      plotOutput(outputId = "plot2"),
      plotOutput(outputId = "plot3")
      )
    )
)



# Define the server (all calculations will take place here)
server = function(input, output) {   
  
  # Re-run the model when the button is pressed 
  model_output <- eventReactive(input$run_model, {
    # Create a Progress object
    showModal(modalDialog("Running model. This will likely take a few minutes.", footer = NULL))
    
    # Basic model; India generic contact matrix
    model <- calibrate(
      reporting_fraction = input$reporting_fraction,
      
      data = df,
      
      R0_min = 2,
      R0_max = 7,
      R0_step = 0.5,
      
      first_start_date = "2020-03-02",
      last_start_date = "2020-03-12",
      day_step = 1,
      
      replicates = input$replicates,  # Make sure this is 100 if final
      n_particles = input$n_particles, # Make sure this is 100 if final
      
      population = pop_vector,
      forecast = 14, # 70 
      baseline_contact_matrix = squire_matrix,
      
      date_R0_change = int_unique$dates_change,
      R0_change = int_unique$R0_change,
      
      baseline_hosp_bed_capacity = hosp_bed, 
      hosp_bed_capacity = input$hosp_bed_capacity,
      date_hosp_bed_capacity_change = input$date_hosp_bed_capacity_change,

      baseline_ICU_bed_capacity = ICU_bed,
      ICU_bed_capacity = input$ICU_bed_capacity,
      date_ICU_bed_capacity_change = input$date_ICU_bed_capacity_change,
    )
    
    removeModal()
    
    return(model)
  })
  
  # Plot outputs
  output$plot1 <- renderPlot({
    plot(model_output(), "deaths", particle_fit = TRUE) +          
      labs(title = "Model fit to daily death counts to date",
           subtitle = "Latest data from 27 May") + 
      scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",               # mark x-ticks w/ month, day
                   limits = as.Date(c("2020-03-07", 
                                      Sys.Date()))) +        # cut off viz at today's date
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +    
      scale_y_continuous(labels = comma) +
      ylim(c(0, 100))                                   # change vertical limits
    
  })
  
  # Plot with particle fit
  output$plot2 <- renderPlot({
    plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                 event=c("Lockdown 1.0", "Lockdown 3.0"))
    
    plot(model_output(), var_select = c("hospital_occupancy"), 
         date_0 = max(df$date), x_var = "date") +
      labs(title = "Projection for hospital bed occupancy",
           subtitle = "Projecting forward from 27 May") +
      ylab("No. of beds") +
      xlab("Date") +
      geom_hline(yintercept = hosp_bed , linetype = 4) + # show bed capacity line
      annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
               y = hosp_bed + 1800, 
               label = "80% bed capacity", size = 3, 
               fontface = 'italic', hjust = 0) +
      geom_segment(data = plotting_dates,
                   mapping = aes(x = date, xend = date,
                                 y = 0, yend = hosp_bed),
                   color = 'darkgrey',
                   alpha = 0.8,
                   size = 1) +
      geom_text(data = plotting_dates, 
                mapping = aes(x = date, y = 1000, label = event), 
                size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                color = 'darkgrey', alpha = 0.8, 
                fontface = 'bold') +
      scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",               # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",        # unmarked grid lines for each week
                   #limits = as.Date(c("2020-03-07", 
                   #                   "2020-06-01"))
      ) +
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +
      #ylim(0, hosp_bed + 4000) +
      scale_y_continuous(n.breaks = 8, 
                         limits = c(0, hosp_bed + 4000)) + 
      theme(legend.position = "none")                   # suppress legend
    
  })  
  
  # Plot Delhi infections
  output$plot3 <- renderPlot({
    plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                 event=c("Lockdown 1.0", "Lockdown 3.0"))
    
    plot(model_output(), var_select = c("ICU_occupancy"), 
         date_0 = max(df$date), x_var = "date") +
      labs(title = "Projection for ICU bed occupancy",
           subtitle = "Projecting forward from 27 May") +
      ylab("No. of beds") +
      xlab("Date") +
      geom_hline(yintercept = ICU_bed, linetype = 4) +   # show bed capacity line
      annotate("text", x = as.Date("2020-03-01"),        # show bed capacity text
               y = ICU_bed + 100, 
               label = "80% bed capacity", size = 3, 
               fontface = 'italic', hjust = 0) +
      geom_segment(data = plotting_dates,
                   mapping = aes(x = date, xend = date,
                                 y = 0, yend = ICU_bed),
                   color = 'darkgrey',
                   alpha = 0.8,
                   size = 1) +
      geom_text(data = plotting_dates, 
                mapping = aes(x = date, y = 100, label = event), 
                size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                color = 'darkgrey', alpha = 0.8, 
                fontface = 'bold') +
      scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                   date_labels = "%b %d",                   # mark x-ticks w/ month, day
                   date_minor_breaks = "1 week",            # unmarked grid lines for each week
      ) +
      theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                       hjust = 1)) +
      ylim(0, 1000) +
      scale_y_continuous(n.breaks = 8) + 
      theme(legend.position = "none")                   # suppress legend
    
  })
}

shinyApp(ui = ui, server = server)