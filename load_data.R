# This file define support functions for the shiny app. They can be run as stand-alone
# or may be called from the app

#' Get a vector of population number for the state
#' bucketed in 5-year groups. Note that 17 groups (last groung being 80+)
#' are returned.
#' 
#' @param state the abbreviation of the state in uppercase
#' @return a vector of population by age
get_population_by_age_vector <- function(state = 'DL') {
  
  if (state != 'DL') {
    stop("Only Delhi data available") 
  }

  pop_dist <- read.csv("./data/delhi_pop.csv")
  
  pop_vector <- pop_dist$total_persons[-18] %>%  # exclude "age not stated" in row 18
    as.character %>% 
    str_remove_all(",") %>% 
    as.integer

  return(pop_vector)
}


#' Get a tibble of cases and deaths in the format
#' requred by `squire::calibrate`
#' 
#' @param state the abbreviation of the state in uppercase
#' @return tibble of case data
get_deaths_cases_data <- function(state = 'DL') {
  
  state_wise_data <- read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
  state_data <- state_wise_data[, c("Date", "Status", state)] %>%
    pivot_wider(names_from = 'Status', values_from = eval(state))
  
  state_data$Date <- as.Date(state_data$Date, "%d-%B-%y")
  names(state_data) <- c("date", "cases", "recovered", "deaths")
  df <- state_data[c("date", "deaths", "cases")]
  df['X'] <- seq_len(nrow(df))
  
  return(df)
  
}


#' Return a contact matrix for the state. At this stage,
#' all states use the same contact matrix - the default one built
#' into squire
#' 
#' @param state the abbreviation of the state in uppercase
#' @return a contact matrix
get_contact_matrix <- function(state = 'DL') {
  india_params_list <- parameters_explicit_SEEIR('India')
  return (india_params_list$contact_matrix_set[[1]])
}


#' Return model parameters to be used for the squire
#' model. hospital and ICU bed capacity from here can be found here:
#' https://cddep.org/wp-content/uploads/2020/04/State-wise-estimates-of-current-beds-and-ventilators_24Apr2020.pdf
#' 
#' @param state the abbreviation of the state in uppercase
#' @return list of parameters
get_model_params <- function(R0_changes, state = 'DL') {
  
  if (state != 'DL') {
    stop("Only Delhi params available") 
  }
  
  params <- list()
  
  params[['population']] = get_population_by_age_vector(state)
  params[['baseline_contact_matrix']] = get_contact_matrix(state)
  params[['data']] <- get_deaths_cases_data()
  params['baseline_hosp_bed_capacity'] <- 39455 * 0.8
  params['baseline_ICU_bed_capacity'] <- 1973 * 0.8
  
  params[['reporting_fraction']] <- 1.0
  params[['R0_min']] <- 2
  params[['R0_max']] <- 7
  params[['R0_step']] <- 0.5
  
  params[['first_start_date']] = "2020-03-02"
  params[['last_start_date']] = "2020-03-12"
  
  params[['day_step']] = 1
  
  params[['replicates']] = 10
  params[['n_particles']] = 10

  params[['date_R0_change']] = names(R0_changes)
  params[['R0_change']] = unlist(R0_changes, use.names = F)
  
  return(params)
}



#' Take a parameters list and returns the intervention list
#' as required by `squire::calibrate`. 
#' 
#' @param parameters A list of parameters
#' @return A list with "dates_change" and "R0_change"
get_unique_interventions <- function(parameters) {
  R0_change_list <- parameters[['R0_changes']]
  list(dates_change = names(R0_change_list), 
       R0_change = unlist(R0_change_list, use.names = F))
}
