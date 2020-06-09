#-----------------#
# READ IN PACKAGES#
#-----------------#

library(tidyverse)
library(shiny)
library(squire)

#---------------------------------------------#
#DO THINGS THAT DO NOT REQUIRE ANY SHINY INPUT#
#---------------------------------------------#
# read in data
pop_dist <- read.csv("./data/delhi_pop.csv")

pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
  as.character %>% 
  str_remove_all(",") %>% 
  as.integer

state_wise_data <- read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
india_params_list <- parameters_explicit_SEEIR('India')

#NV1: should there be an input for STATE in Shiny?
STATE <- "DL"

state_data <- state_wise_data[, c("Date", "Status", STATE)] %>%
  pivot_wider(names_from = Status, values_from = eval(STATE))

state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
names(state_data) <- c("date", "cases", "recovered", "deaths")
df <- state_data[c("date", "deaths", "cases")]
df['X'] <- seq_len(nrow(df))

#NV1: should this list use the inputs for lockdown
int_unique <- list(dates_change = c("2020-03-17", "2020-03-24", "2020-05-03"), # Day of lockdown
                   change = c(0.5, 0.2, 0.3))

#NV1: Should any of these be inputs in Shiny
# Note date of national lockdown
natl.lockdown <- as.numeric(as.Date("2020-03-24"))
natl.lockdown.relaxed <- as.numeric(as.Date("2020-05-03"))


#-------#
#R SHINY#
#-------#

# Define the UI
ui = navbarPage("Delhi - COVID-19 Simiulator", # Website title
                tabPanel("Interface 1", # Page title
                         titlePanel(
                           fluidRow(
                             column(2, img(height = 200, width = 200, src = "delhi_govt.png")),
                             )),
                         sidebarPanel(
                           actionButton("run_model", "Run Model"),
                           numericInput(label = "R0 Min:",
                                        inputId = "R0_min",
                                        min = 0,
                                        value = 0.5),
                           numericInput(label = "R0 Max:",
                                        inputId = "R0_max",
                                        min = 0,
                                        value = 10),
                           numericInput(label = "R0 Step:",
                                        inputId = "R0_step",
                                        min = 0,
                                        value = 0.5),
                           dateInput(label = "First Start Date:",
                                     inputId = "first_start_date",
                                     format = "yyyy-mm-dd",
                                     min = "2020-03-02",
                                     value = "2020-03-02"),
                           dateInput(label = "Last Start Date:",
                                     inputId = "last_start_date",
                                     format = "yyyy-mm-dd",
                                     min = "2020-03-02",
                                     value = "2020-03-12"),
                           numericInput(label = "Day Step:",
                                        inputId = "day_step",
                                        min = 0,
                                        step = 1,
                                        value = 1),
                           numericInput(label = "Replicates:",
                                        inputId = "replicates",
                                        min = 0,
                                        step = 1,
                                        value = 20),
                         ),             
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
  
  # Re-run the model when the button is pressed 
  model_output <- eventReactive(input$run_model, {
    # Create a Progress object
    showModal(modalDialog("Running model. This will likely take a few minutes.", footer=NULL))
    
    # Basic model; India generic contact matrix
    model <- calibrate(
      data = df,
      R0_min = input$R0_min,
      R0_max = input$R0_max,
      R0_step = input$R0_step,
      first_start_date = input$first_start_date,
      last_start_date = input$last_start_date,
      day_step = input$day_step,
      replicates = input$replicates,
      n_particles = 20,
      population = pop_vector,
      forecast = 14,
      baseline_contact_matrix  = india_params_list$contact_matrix_set,
      R0_change = int_unique$change,
      date_R0_change = int_unique$dates_change
    )
    
    removeModal()
    
    return(model)
  })
  
  # Plot outputs
  output$plot1 <- renderPlot({
    plot(model_output()$scan_results)
  })
  
  # Plot with particle fit
  output$plot2 <- renderPlot({
    plot(model_output(), 'deaths', particle_fit = TRUE) + 
      ggtitle(label = "Delhi: Deaths per day") +
      geom_vline(xintercept = natl.lockdown, linetype=4) +
      annotate("text", x = as.Date("2020-03-23"), y = 10, 
               label = "Nat'l lock-\ndown begins", size = 3, 
               fontface = 'italic', hjust = 1) +
      geom_vline(xintercept = natl.lockdown.relaxed, linetype=4) +
      annotate("text", x = as.Date("2020-05-01"), y = 10, 
               label = "Nat'l lock-\ndown relaxed", size = 3, 
               fontface = 'italic', hjust = 1) 
  })  
  
  # Plot Delhi infections
  output$plot3 <- renderPlot({
    plot(model_output(), 'infections', date_0 = max(df$date), x_var = "date") + 
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
  })
}

shinyApp(ui = ui, server = server)