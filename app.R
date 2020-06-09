# This is the main script for running the shiny app
# It uses the same backend functions as main.R


#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)
library(shiny)

source("./load_data.R")
source("./shiny_scripts/shiny_ui.R")
source("./shiny_scripts/shiny_server.R")
source("./squire_scripts/squire_backend.R")

#-----------------------------------------------#
# Setup environment and data                    #
#-----------------------------------------------#

set.seed(212)
future::plan(future::multiprocess())

R0_changes <- list("2020-03-24" = 0.5,
                   "2020-05-03" = 0.7,
                   "2020-06-06" = 0.8)

params <- get_model_params(R0_changes, 'DL')

#---------#
# R SHINY #
#---------#

ui <- get_ui("Delhi COVID-19 Simulator")
server <- get_server_func(params)

shinyApp(ui = ui, server = server)


