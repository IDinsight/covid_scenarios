#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyBS)

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

# Original squire matrix
squire_matrix <- india_params_list$contact_matrix_set[[1]]

# Hospital and ICU bed capacities; using 80% of total
# https://cddep.org/wp-content/uploads/2020/04/State-wise-estimates-of-current-beds-and-ventilators_24Apr2020.pdf
hosp_bed <- 39455 * 0.8
ICU_bed <- 1973 * 0.8

# set up for parallelisation
future::plan(future::multiprocess())

# Dates of interventions
phase1 <- "2020-03-24"
phase3 <- "2020-05-03"

# Intervention parameters to pass
int_unique <- list(dates_change = c(phase1, phase3), 
                   R0_change = c(0.2, 0.5)) 


#---------#
# R SHINY #
#---------#

# Define the UI
ui = navbarPage("Delhi COVID-19 Simulator", theme = shinytheme("simplex"),
                # Create a separate page for overview/introduction
               tabPanel("Overview",
                        
                        # Page title
                        titlePanel(
                          fluidRow(
                              column(6, "Introduction", style='font-size:100%; font-weight: bold'), 
                              column(3, img(height = 100, src = "delhi_govt.png")),
                              column(3, img(height = 100, src = "idinsight.png"))
                          )),
                        
                        mainPanel(
                          fluidRow(
                            column(4, ""),
                            column(8, "Enter introduction here")
                          )
                        )),
                         
               # Create a separate page for the simulator
               tabPanel("Simulator",
                        
  titlePanel(
    fluidRow(
      column(6, "Simulations", style='font-size:100%; font-weight: bold'),
      column(3, img(height = 100, src = "delhi_govt.png")),
      column(3, img(height = 100, src = "idinsight.png"))
      )
  ),

  # Model parameters we want the user to control:
  # 1. Date of ICU bed change
  # 2. ICU bed capacity
  # 3. Date of hospital bed change
  # 4. Hospital bed capacity
  # 5. Reporting fraction
  # 6. Forecast
  
  # Follow the code below to create input widgets for the user. 
  # Use the bsTooltip function to set variable definitions 
  
  # Solicit user input
  sidebarPanel("Model Parameters", style = 'font-size:100%; font-weight: bold',
                  dateInput(label = "Date of ICU bed change:",
                           inputId = "date_ICU_bed_capacity_change",
                           format = "yyyy-mm-dd",
                           max = "2021-01-01",
                           value = NULL),
                  bsTooltip("date_ICU_bed_capacity_change", "Date on which ICU bed capacity changed or is anticipated to change",
                           placement = "right", trigger = "hover"),
                  numericInput(label = "Number of ICU beds after change:",
                               inputId = "ICU_bed_capacity",
                               min = 0,
                               step = 10,
                               value = as.integer(ICU_bed)), 
                  bsTooltip("ICU_bed_capacity", "Absolute count of new ICU beds after change",
                           placement = "right", trigger = "hover"),
                  dateInput(label = "Date of hospital bed change:",
                            inputId = "date_hosp_bed_capacity_change",
                            format = "yyyy-mm-dd",
                            max = "2021-01-01",
                            value = NULL),
                  bsTooltip("date_hosp_bed_capacity_change", "Date on which hospital bed capacity changed or is anticipated to change",
                            placement = "right", trigger = "hover"),
                  numericInput(label = "Number of hospital beds after change:",
                               inputId = "hosp_bed_capacity",
                               min = 0,
                               step = 10,
                               value = as.integer(hosp_bed)), 
                  bsTooltip("hosp_bed_capacity", "Absolute count of new hospital beds after change",
                            placement = "right", trigger = "hover"),
                  numericInput(label = "Reporting fraction:",
                               inputId = "reporting_fraction",
                               min = 0,
                               max = 1,
                               step = 0.01,
                               value = 0.8),
                  bsTooltip("reporting_fraction", "Proportion of true deaths represented by reported deaths (0.0-1.0)",
                            placement = "right", trigger = "hover"),
                  numericInput(label = "Number of days to forecast",
                               inputId = "forecast",
                               min = 0,
                               max = 14,
                               step = 1,
                               value = 0),
                  bsTooltip("forecast", "Days to project into the future",
                            placement = "right", trigger = "hover"),
                  tags$hr(),
               
          actionButton("run_test_model", "Run test", style ='padding:10px; font-size:100%; font-weight: bold'),
          bsTooltip("run_test_model", "Run with partial processing power (faster results)",
                    placement = "right", trigger = "hover"),
          tags$hr(),
          actionButton("run_model", "Run", style='padding:10px; font-size:100%; font-weight: bold'),
          bsTooltip("run_model", "Run with full processing power (more accurate results)",
                    placement = "right", trigger = "hover"),
                          width = 4),

  mainPanel(
    fluidRow(
      plotOutput(outputId = "plot1"),
      plotOutput(outputId = "plot2"),
      plotOutput(outputId = "plot3")
      )
    )
)
)



# Define the server (all calculations will take place here)
server = function(input, output) {   
  
  # Create reactive output that updates based on which button is pressed
  model_output <- reactiveValues(model = NULL)
  
  # THIS SECTION IS NEW AS OF 2020.06.01 - HS
  # There are no changes to the model or plot code, just changes in formatting and Shiny processes. 
  
  # Create two models and a set of plots for each model
  # Code structure:
  # 1. Create a model for the full model button 
  # 2. Create plots for the full model buttong
  # 3. Create a model for the test model button
  # 4. Create plots for the test model button 
  
  # 1. Run FULL MODEL when button is pressed
  observeEvent(input$run_model, { # THIS IS NEW AS OF 2020.06.01 - HS
    
    # Create a Progress object
    showModal(modalDialog("Running model. This may take a few minutes.", footer = NULL))
    
 
    model_output$model <- calibrate (
      reporting_fraction = input$reporting_fraction,
      
      data = df,
      
      R0_min = 2,
      R0_max = 7,
      R0_step = 0.5,
      
      first_start_date = "2020-03-02",
      last_start_date = "2020-03-12",
      day_step = 1,
      
      replicates = 100,  
      n_particles = 100, 
      
      population = pop_vector,
      forecast = input$forecast, 
      baseline_contact_matrix = squire_matrix,
      
      date_R0_change = int_unique$dates_change,
      R0_change = int_unique$R0_change,
      
      baseline_hosp_bed_capacity = hosp_bed, 
      hosp_bed_capacity = input$hosp_bed_capacity,
      date_hosp_bed_capacity_change = input$date_hosp_bed_capacity_change,
      
      baseline_ICU_bed_capacity = ICU_bed,
      ICU_bed_capacity = input$ICU_bed_capacity,
      date_ICU_bed_capacity_change = input$date_ICU_bed_capacity_change
    )
    
    #----------------------------------#
    # PLOT OUTPUTS (FULL MODEL BUTTON) #
    #----------------------------------#
    
    # Plot outputs
    output$plot1 <- renderPlot({
      
      plot(model_output$model, var_select = c("deaths"), particle_fit = TRUE) +          
        labs(title = "Model fit to daily death counts to-date") + 
        scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                     date_labels = "%b %d",               # mark x-ticks w/ month, day
                     limits = as.Date(c("2020-03-07", 
                                        Sys.Date()))) +   # cut off viz at today's date
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +    
        scale_y_continuous(labels = comma) +
        ylim(c(0, max(model_output$model$scan_results$inputs$data$deaths) + # change vertical limits proportionally
                 max(model_output$model$scan_results$inputs$data$deaths) * 0.15))   
    })
    
    # Plot hospital bed projections
    output$plot2 <- renderPlot({
      
      # Set up dates to annotate plot
      
      plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                   event=c("Lockdown 1.0", "Lockdown 3.0"))
      
      ifelse (input$forecast > 0, # If forecast == 0, no annotation
              today <- data.frame(date = Sys.Date(), event = "Today"),
              today <- data.frame(date = as.Date(NA), event = NA)
      )
      
      plot(model_output$model, var_select = c("hospital_occupancy"), 
           date_0 = max(df$date), x_var = "date") +
        labs(title = "Projection for hospital bed occupancy") +
        ylab("No. of beds") +
        xlab("Date") +
        geom_hline(yintercept = hosp_bed, linetype = 4) +  # show bed capacity line
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
                     date_minor_breaks = "1 week"         # unmarked grid lines for each week
        ) +
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +
        scale_y_continuous(n.breaks = 8, 
                           limits = c(0, hosp_bed * 1.1)) + 
        theme(legend.position = "none")                   # suppress legend
      
    })  
    
    # Plot ICU bed projections
    output$plot3 <- renderPlot({
      
      # Set up dates to annotate plot
      
      plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                   event=c("Lockdown 1.0", "Lockdown 3.0"))
      
      ifelse (input$forecast > 0, # If forecast == 0, no annotation
              today <- data.frame(date = Sys.Date(), event = "Today"),
              today <- data.frame(date = as.Date(NA), event = NA)
      )
      
      plot(model_output$model, var_select = c("ICU_occupancy"), 
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
                     alpha = 0.5,
                     size = 1) +
        geom_text(data = today,                          # Annotate today line
                  mapping = aes(x = date, 
                                y = 0, 
                                label = event), 
                  size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                  color = 'darkmagenta', alpha = 0.5,
                  fontface = 'italic') +
        scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                     date_labels = "%b %d",               # mark x-ticks w/ month, day
                     date_minor_breaks = "1 week"         # unmarked grid lines for each week
        ) +
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +
        scale_y_continuous(n.breaks = 8,
                           limits = c(0, ICU_bed * 1.1)) + 
        theme(legend.position = "none")                   # suppress legend
      
    })
    
    removeModal()
  }
  )
  
  # 2. Run TEST MODEL when button is pressed
  observeEvent(input$run_test_model, { # THIS SECTION IS NEW AS OF 2020.06.01 - HS
    
    # Create a Progress object
    showModal(modalDialog("Running model. This may take a few seconds.", footer = NULL))
    
    model_output$model <- calibrate (
      reporting_fraction = input$reporting_fraction,
      
      data = df,
      
      R0_min = 2,
      R0_max = 7,
      R0_step = 0.5,
      
      first_start_date = "2020-03-02",
      last_start_date = "2020-03-12",
      day_step = 1,
      
      replicates = 10,  
      n_particles = 10, 
      
      population = pop_vector,
      forecast = input$forecast, 
      baseline_contact_matrix = squire_matrix,
      
      date_R0_change = int_unique$dates_change,
      R0_change = int_unique$R0_change,
      
      baseline_hosp_bed_capacity = hosp_bed, 
      hosp_bed_capacity = input$hosp_bed_capacity,
      date_hosp_bed_capacity_change = input$date_hosp_bed_capacity_change,
      
      baseline_ICU_bed_capacity = ICU_bed,
      ICU_bed_capacity = input$ICU_bed_capacity,
      date_ICU_bed_capacity_change = input$date_ICU_bed_capacity_change
    )
    
    #-----------------------------#
    # PLOT OUTPUTS FOR TEST MODEL #
    #-----------------------------#
    
    # Plot outputs
    
    output$plot1 <- renderPlot({
      
      plot(model_output$model, var_select = c("deaths"), particle_fit = TRUE) +          
        labs(title = "Model fit to daily death counts to-date") + 
        scale_x_date(date_breaks = "1 week",              # x-tick every 2 weeks
                     date_labels = "%b %d",               # mark x-ticks w/ month, day
                     limits = as.Date(c("2020-03-07", 
                                        Sys.Date()))) +   # cut off viz at today's date
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +    
        scale_y_continuous(labels = comma) +
        ylim(c(0, max(model_output$model$scan_results$inputs$data$deaths) + # change vertical limits proportionally
                 max(model_output$model$scan_results$inputs$data$deaths) * 0.15))   
    })
    
    # Plot hospital bed projections
    output$plot2 <- renderPlot({
      
      # Set up dates to annotate plot
      
      plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                   event=c("Lockdown 1.0", "Lockdown 3.0"))
      
      ifelse (input$forecast > 0, # If forecast == 0, no annotation
              today <- data.frame(date = Sys.Date(), event = "Today"),
              today <- data.frame(date = as.Date(NA), event = NA)
      )
      
      plot(model_output$model, var_select = c("hospital_occupancy"), 
           date_0 = max(df$date), x_var = "date") +
        labs(title = "Projection for hospital bed occupancy") +
        ylab("No. of beds") +
        xlab("Date") +
        geom_hline(yintercept = hosp_bed, linetype = 4) +  # show bed capacity line
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
                     date_minor_breaks = "1 week"         # unmarked grid lines for each week
        ) +
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +
        scale_y_continuous(n.breaks = 8, 
                           limits = c(0, hosp_bed * 1.1)) + 
        theme(legend.position = "none")                   # suppress legend
      
    })  
    
    # Plot ICU bed projections
    output$plot3 <- renderPlot({
      
      # Set up dates to annotate plot
      
      plotting_dates <- data.frame(date=as.Date(c(phase1, phase3)),
                                   event=c("Lockdown 1.0", "Lockdown 3.0"))
      
      ifelse (input$forecast > 0, # If forecast == 0, no annotation
              today <- data.frame(date = Sys.Date(), event = "Today"),
              today <- data.frame(date = as.Date(NA), event = NA)
      )
      
      plot(model_output$model, var_select = c("ICU_occupancy"), 
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
                     alpha = 0.5,
                     size = 1) +
        geom_text(data = today,                          # Annotate today line
                  mapping = aes(x = date, 
                                y = 0, 
                                label = event), 
                  size = 3, angle = 90, vjust = -0.5, hjust = 0, 
                  color = 'darkmagenta', alpha = 0.5,
                  fontface = 'italic') +
        scale_x_date(date_breaks = "2 week",              # x-tick every 2 weeks
                     date_labels = "%b %d",               # mark x-ticks w/ month, day
                     date_minor_breaks = "1 week"         # unmarked grid lines for each week
        ) +
        theme(axis.text.x = element_text(angle = 45,      # x-axis on 45 deg angle
                                         hjust = 1)) +
        scale_y_continuous(n.breaks = 8,
                           limits = c(0, ICU_bed * 1.1)) + 
        theme(legend.position = "none")                   # suppress legend
      
    })
    
    removeModal()
  }
  )
  
}

shinyApp(ui = ui, server = server)