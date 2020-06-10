# This file contains function to setup the shiny user interface
# `get_ui` is the main function to be called. It calls all the remaining
# functions in the module

library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyBS)

#' Main function to return a ui object to setup the ap
#' 
#' @param name The name to be used for the app
#' @retun ui object
get_ui <- function(name) {
  intro_text = "./shiny_scripts/shiny_assets/introduction.html" 
  tabs <- list(get_overview_tab(intro_text), get_simulator_tab())
  ui <- get_tabbed_ui(name, tabs)
  
  return(ui)
  
}


#' Create a tabbed ui based on the title and the list of 
#' tabbed pages
#' 
#' @param title title of the navigation bar
#' @return a tabbed ui object
get_tabbed_ui <- function(title, tab_list) {
  ui <- do.call(navbarPage, 
                c(list(title = title, theme = shinytheme("simplex")), tab_list))
  return(ui)
}


#' Create the overview tab
#' 
#' @param intro_text the text to be rendered on the intro page
#' @return the overview tabbed object
get_overview_tab <- function(intro_text) {
  tab <- tabPanel("Overview",
           # Page title
           titlePanel(
             fluidRow(
               column(4, "Introduction", style='font-size:100%; font-weight: bold'), 
             )),
           
           mainPanel(
             fluidRow(
               column(10, includeHTML(intro_text))
             )
           )
        )
  return(tab)
}


#' Create the simulator tab. It has two components:
#' - the input panel
#' - the plots panel
#' 
#' @return simulator tab
get_simulator_tab <- function() {
  tab <- tabPanel("Simulator",
           
           titlePanel(
             fluidRow(
               column(6, "Simulations", style='font-size:100%; font-weight: bold'),
             )
           ),
           
           get_simulator_input_panel(),
           
           mainPanel(
             fluidRow(
               plotOutput(outputId = "plot1"),
               plotOutput(outputId = "plot2"),
               plotOutput(outputId = "plot3")
             )
           )
        )
  return(tab)
}


#' Create the simulator input panel
#' 
#' @return simulator input panel
get_simulator_input_panel <- function() {
  side_panel <- sidebarPanel("Model Parameters", style = 'font-size:100%; font-weight: bold',
               # numericInput(label = "Days until ICU bed change (if applicable):",
               #              inputId = "tt_ICU_beds",
               #              min = 0,
               #              step = 1,
               #              value = 0.0),
               # bsTooltip("tt_ICU_beds", "Days until ICU bed capacity is anticipated to change (e.g. 7, 21). If no anticipated change, leave at 0.",
               #           placement = "right", trigger = "hover"),
               # numericInput(label = "Ratio increase/decrease in ICU beds after change (if applicable):",
               #              inputId = "ICU_bed_capacity_change",
               #              min = 0,
               #              step = 0.01,
               #              value = 1.0), 
               # bsTooltip("ICU_bed_capacity_change", "If beds to increase by 10%, enter 1.1; if beds to decrease by 40%, enter 0.6. If no anticipated change, leave at 1.0.",
               #           placement = "right", trigger = "hover"),
               # numericInput(label = "Days until hospital bed change (if applicable):",
               #              inputId = "tt_hosp_beds",
               #              min = 0,
               #              step = 1,
               #              value = 0.0),
               # bsTooltip("tt_hosp_beds", "Days until hospital bed capacity is anticipated to change (e.g. 7, 21). If no anticipated change, leave at 0.",
               #           placement = "right", trigger = "hover"),
               # numericInput(label = "Ratio increase/decrease in hospital beds after change (if applicable):",
               #              inputId = "hosp_bed_capacity_change",
               #              min = 0,
               #              step = 0.01,
               #              value = 1.0), 
               # bsTooltip("hosp_bed_capacity_change", "If beds to increase by 10%, enter 1.1; if beds to decrease by 40%, enter 0.6. If no anticipated change, leave at 1.0.",
               #           placement = "right", trigger = "hover"),
               # numericInput(label = "Reporting fraction:",
               #              inputId = "reporting_fraction",
               #              min = 0,
               #              max = 1,
               #              step = 0.01,
               #              value = 0.8),
               # bsTooltip("reporting_fraction", "Proportion of true deaths represented by reported deaths (0.0-1.0)",
               #           placement = "right", trigger = "hover"),
               numericInput(label = "Number of days to forecast",
                            inputId = "forecast",
                            min = 2,
                            max = 14,
                            step = 1,
                            value = 2),
               numericInput(label = "Days until next lock/unlock phase (if any):",
                            inputId = "tt_R0",
                            min = 0, 
                            max = 21,
                            step = 1,
                            value = NULL),
               bsTooltip("tt_R0", "Number of days until change in lockdown conditions",
                         placement = "right", trigger = "hover"),
               numericInput(label = "R0 ratio during next lock/unlock phase (if any):",
                            inputId = "R0_change_future",
                            min = 0.0,
                            max = 1.0,
                            step = 0.05,
                            value = NULL),
               bsTooltip("R0_change_future", "Estimate how the next phase will change R0. E.g., if R0 will reduce to 20% of unabated value, enter 0.2.",
                         placement = "right", trigger = "hover"),
               numericInput(label = "Reporting fraction:",
                            inputId = "reporting_fraction",
                            min = 0,
                            max = 1,
                            step = 0.01,
                            value = 0.8),
               bsTooltip("forecast", "Days to project into the future (e.g., 7, 14)",
                         placement = "right", trigger = "hover"),
               tags$hr(),
               
               actionButton("run_test_model", "Run test", style ='padding:10px; font-size:100%; font-weight: bold'),
               bsTooltip("run_test_model", "Run with partial processing power (faster results)",
                         placement = "right", trigger = "hover"),
               tags$hr(),
               actionButton("run_model", "Run", style='padding:10px; font-size:100%; font-weight: bold'),
               bsTooltip("run_model", "Run with full processing power (more accurate results)",
                         placement = "right", trigger = "hover"),
               width = 4)
  
  return(side_panel)
  
}
