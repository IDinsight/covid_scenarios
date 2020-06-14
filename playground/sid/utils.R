make_scenario_plots <- function(scenario_list) {
  
  for (scenario in names(scenario_list)) {
    
    projection_params <- all_scenarios[[scenario]]
    future_preds <- get_projection(projection_params)
    
    projections_df <- future_preds$output
    max_proj_date <- max(rownames(projections_df))
    death_cols <- paste0('D[',seq(1:17), ']')
    
    total_deaths_sims <- colSums(projections_df[max_proj_date, death_cols, ])
    death_quantiles <- quantile(total_deaths_sims, c(0.025, 0.5, 0.975))
    
    deaths_annotation <- sprintf("Total deaths by %s: %s [%s - %s]", 
                                 max_proj_date,
                                 comma(death_quantiles[["50%"]], accuracy = 1),
                                 comma(death_quantiles[["2.5%"]], accuracy = ),
                                 comma(death_quantiles[["97.5%"]], accuracy = 1))
    
    R0_changes <- projection_params$R0_change * 5
    
    death_title <- paste(c("Deaths based on (", paste0(R0_changes, collapse= ", ")
                           , ")"), collapse = "")
    hospital_title <- paste(c("Hospital Demand based on (", 
                              paste0(R0_changes, collapse= ", ")
                              , ")"), collapse = "")
    
    # make plots
    plot_deaths <- get_daily_death_projections(future_preds, max(params$data$date),
                                               plotting_dates, death_title, deaths_annotation)
    plot_hospital <- get_hospital_bed_plot(future_preds, max(params$data$date), 
                                           plotting_dates, 
                                           title = hospital_title) +
      geom_point(data = delhi_govt_proj, aes(x = Dates, y = beds), color='red')
    
    # save plots
    base_path = "./data/02_model_output/"
    ggsave(paste0(base_path, scenario, "_deaths.png"), plot_deaths)
    ggsave(paste0(base_path, scenario, "_hosp_beds.png"), plot_hospital)
    
    print(paste0("Done: ", scenario))
  }
  
}