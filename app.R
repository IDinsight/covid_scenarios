# This is the main script for running the shiny app
# It uses the same backend functions as main.R


#------------------#
# READ IN PACKAGES #
#------------------#

library(squire)
library(tidyverse)
library(shiny)

source("./data.R")
source("./shiny_harness.R")
source("./shiny_server.R")
source("./squire_backend.R")

#-----------------------------------------------#
# Setup environment and data                    #
#-----------------------------------------------#

set.seed(212)
future::plan(future::multiprocess())

R0_changes <- list("2020-03-24" = 0.2,
                   "2020-05-03" = 0.5)
params <- get_model_params(R0_changes, 'DL')

#---------#
# R SHINY #
#---------#

ui <- get_ui("Delhi COVID-19 Simulator")
server <- get_server_func(params)

shinyApp(ui = ui, server = server)


