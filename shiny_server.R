# This file contains function to setup the shiny server

library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyBS)

#' Main function to create the server function. 
#' It returns a function with params in its context. 
#' 
#' @param params The parameters used to calibrate the model
#' @retun the server function
get_server_func <- function(params) {
  
  server <- function(input, output) {
    
    # Create reactive output that updates based on which button is pressed
    model_output <- reactiveValues(model = NULL)
    dates_to_annotate <- data.frame(date=as.Date(params[['date_R0_change']]),
                                    event=c("Lockdown 1.0", "Lockdown 3.0", "Unlock 1.0"))
    
    
    observeEvent(input$run_model, { 
      
      showModal(modalDialog("Running model. This may take a few minutes.", footer = NULL))
      
      params[['replicates']] <- 100
      params[['n_particles']] <- 100
      
      model <- get_calibrated_model(params)
      projection_params <- list(r = model, time_period = input$forecast)
      forecast <- do.call(squire::projections, projection_params)
      
      # Plot outputs
      output$plot1 <- renderPlot({
        get_daily_death_plot(model)
      })

      output$plot2 <- renderPlot({
        get_hospital_bed_plot(forecast, max(params$data$date), dates_to_annotate)
      })  
      
      output$plot3 <- renderPlot({
        get_icu_plot(forecast, max(params$data$date), dates_to_annotate)
      })
      
      removeModal()
      
    })
    
    # 2. Run TEST MODEL when button is pressed
    observeEvent(input$run_test_model, { # THIS SECTION IS NEW AS OF 2020.06.01 - HS
      
      # Create a Progress object
      showModal(modalDialog("Running model. This may take a few minutes", footer = NULL))
      
      params[['replicates']] <- 10
      params[['n_particles']] <- 10
      
      model <- get_calibrated_model(params)
      projection_params <- list(r = model, time_period = input$forecast)
      forecast <- get_projection(projection_params)
      
      # Plot outputs
      output$plot1 <- renderPlot({
        get_daily_death_plot(model)
      })
      
      output$plot2 <- renderPlot({
        get_hospital_bed_plot(forecast, max(params$data$date), dates_to_annotate)
      })  
      
      output$plot3 <- renderPlot({
        get_icu_plot(forecast, max(params$data$date), dates_to_annotate)
      })
      
      removeModal()
    })
  }
  
  return(server)
}