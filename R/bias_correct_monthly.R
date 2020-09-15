
#--------------------------------------------------------------------------------------------------------------
#######                                  MONTHLY BIAS CORRECTION
#
#
# This function reads in observed and simulated streamflow inputs and bias correct the simulated
# streamflow using the quantile mapping method.
# The output is in the monthly time step.
#--------------------------------------------------------------------------------------------------------------

#' @title  Bias correction of monthly streamflow
#'
#' @description  This function uses the quantile mapping technique to calculate monthly bias-corrected data
#'
#' @param observed Observed time series.
#' @param simulated Simulated time series.
#' @param start_date_observed Start date of observed time series. For example: "1929-01-01".
#' @param end_date_observed End date of observed time series. For example: "1929-01-01".
#' @param start_date_simulated Start date of simulated time series. For example: "1929-01-01".
#' @param end_date_simulated End date of simulated time series. For example: "1929-01-01".
#' @param time_step Time step of the observed and simulated time-series. Can be "day" or "month".
#' @param date_type Water year (WY) or Julian Year (JY).
#'
#'
#' @export
#'
#'@import RANN

# library(class)
# library(sf)
# library(RANN)
# library(nabor)

#library("RANN")

bias_correct_monthly<-function(observed,                # Observed streamflow
                             simulated,               # Simulated streamflow e.g., "2006-12-31"
                             start_date_observed,     # Start date of observed flow e.g., "2006-12-31"
                             end_date_observed,       # End date of observed flow
                             start_date_simulated,    # Start date of simulated flow
                             end_date_simulated,      # End date of simulated flow
                             time_step,               # Timestep = "month" or "day"
                             date_type                # Water year (WY) or Julian Year (JY)
                             ){

  #-- Create the observed quantiles

  # Generate a date time series for the observed streamflow.
  inds_obs <- seq(as.Date(start_date_observed), as.Date(end_date_observed), by = time_step)
  datetxt <- as.Date(inds_obs)
  date_obs <- data.frame(date = datetxt,
                         month = as.numeric(format(datetxt, format = "%m")),
                         day = as.numeric(format(datetxt, format = "%d")),
                         year = as.numeric(format(datetxt, format = "%Y")))

  # Calculate the number of years based on water year vs. calender year
  if(date_type== "WY"){
    n_year<-length(unique(date_obs[,4]))-1
  } else {
    n_year<-length(unique(date_obs[,4]))
  }
  df_observed<-cbind(date_obs, observed)


  # Create observed quantiles
  obs_quantile_table<-create_quantiles(df_observed, n_year)


  #-- Create the simulated quantiles

  # Generate a date time series for the simulated streamflow.
  inds_sim <- seq(as.Date(start_date_simulated), as.Date(end_date_simulated), by = time_step)
  datetxt <- as.Date(inds_sim)
  date_sim <- data.frame(date = datetxt,
                         month = as.numeric(format(datetxt, format = "%m")),
                         day = as.numeric(format(datetxt, format = "%d")),
                         year = as.numeric(format(datetxt, format = "%Y")))

  df_sim<-cbind(date_sim, simulated)
  # if (time_step=="day"){
  m_sim<-aggregate(df_sim[,5], by=list(df_sim[,2], df_sim[,4]), mean)
  # } else{
  #   m_sim<-df_sim
  # }

  m_bc<-cbind(m_sim , m_sim[,3])
  m_bc[,4]=0
  names(m_bc)<-c("month","year", "simulated","bias_corrected")

  if(date_type== "WY"){
    n_year_f<-length(unique(date_sim[,4]))-1
  } else {
    n_year_f<-length(unique(date_sim[,4]))
  }

  # Create simulated quantiles
  sim_quantile<-create_quantiles(df_sim, n_year_f)


  # Map quantiles of simulated flow into observed quantile
  for (i_rec in 1:length(m_sim[,1])){
    row_sim<-which(m_bc[i_rec,3]==sim_quantile[,m_bc[i_rec,1]+1])
    row_obs<-nn2(obs_quantile_table[,1], sim_quantile[row_sim,1])$nn.idx[1]
    m_bc[i_rec,4]=obs_quantile_table[row_obs,m_bc[i_rec,1]+1]
  }

  inds_out <- seq(as.Date(start_date_simulated), as.Date(end_date_simulated), by = "month")
  m_bc<-cbind(inds_out, m_bc)

  # Return bias-corrected monthly streamflow
  return(m_bc)
}

