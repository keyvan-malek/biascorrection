

#-----------------------------------------------------------------------------------------------------
#######                                  DISAGGREGATE MONTHLY DATA TO DAILY
#
#
# This function bias corrects the streamflow in daily time step. To do that the code reads in
# the observed and simulated streamflow, then it calls the monthly function, the annual correction and
# then disaggregate them to daily
# The output data frame is a daily data frame
#-----------------------------------------------------------------------------------------------------

#' @title  Bias correction of daily streamflow
#'
#' @description  This function first applies monthly and annual bias correction and then disaggregates to daily
#'
#' @param observed Observed time series
#' @param simulated Simulated time series
#' @param start_date_observed Start date of observed time series. For example: "1929-01-01"
#' @param end_date_observed End date of observed time series. For example: "1929-01-01"
#' @param start_date_simulated Start date of simulated time series. For example: "1929-01-01"
#' @param end_date_simulated Start date of simulated time series. For example: "1929-01-01"
#' @param time_step Time step of the observed and simulated time-series. Can be "day" or "month"
#' @param date_type Water year (WY) or Julian Year (JY)
#' @param disaggregation_method Can be "knn" or "scaling_coeff"
#'
#'
#' @export
#'
#'@import RANN
#'@import nabor


# library("RANN")
# library("nabor")


bias_correct_daily<-function(observed,                # Observed streamflow
                             simulated,               # Simulated streamflow e.g., "2006-12-31"
                             start_date_observed,     # Start date of observed flow e.g., "2006-12-31"
                             end_date_observed,       # End date of observed flow
                             start_date_simulated,    # Start date of simulated flow
                             end_date_simulated,      # End date of simulated flow
                             time_step,               # Timestep = "month" or "day"
                             date_type,               # Water year (WY) or Julian Year (JY)
                             disaggregation_method    # can be scaling_coeff or knn
){


  # Keyvan Malek 09142020

  # Create a data frame
  #df_bc_sim_daily<-data.frame(matrix(nrow = length(WRF_NoGW_in[,1]), ncol = 2))
  #names(df_bc_sim_daily)=c("date", "daily_sim_streamflow")

  # Generate a date time series for the observed streamflow.
  inds_obs <- seq(as.Date(start_date_observed), as.Date(end_date_observed), by = "day")
  datetxt <- as.Date(inds_obs)
  date_obs <- data.frame(date = datetxt,
                         month = as.numeric(format(datetxt, format = "%m")),
                         day = as.numeric(format(datetxt, format = "%d")),
                         year = as.numeric(format(datetxt, format = "%Y")))

  observed_in_date<-cbind(date_obs, observed)

  # Aggregate the daily data to monthly
  observed_month<-aggregate(observed_in_date[,5], by=list(observed_in_date[,2],observed_in_date[,4]), mean)

  # Calculate bias-corrected flow and put the output into a data frame
  df_bc<-bias_correct_monthly(observed, simulated, start_date_observed, end_date_observed, start_date_simulated, end_date_simulated, time_step, date_type)



  # Annual bias correction of streamflow
  df_bc<-cbind(df_bc, 0) # To add a column for the annual bias corrected values
  df_bc[,6]=bias_correct_annual(df_bc$simulated, df_bc$bias_corrected, start_date_simulated, end_date_simulated)

  # To add the daily correction coeffients
  df_bc_D<-cbind(df_bc, 0)
  df_bc_D[,4:6][df_bc_D[,4:6] ==0] <- 1
  df_bc_D[,7]=df_bc_D[,4]/df_bc_D[,6]
  names(df_bc_D)<-c("date", "month", "year", "simulated", "bias_corrected", "annual_bias_corrected", "correction_coef")



  inds_sim <- seq(as.Date(start_date_simulated), as.Date(end_date_simulated), by = "day")
  datetxt <- as.Date(inds_sim)
  date_sim <- data.frame(date = datetxt,
                         month = as.numeric(format(datetxt, format = "%m")),
                         day = as.numeric(format(datetxt, format = "%d")),
                         year = as.numeric(format(datetxt, format = "%Y")))
  sim_in_date<-cbind(date_sim, simulated)

  sim_in_date<-cbind(sim_in_date, 0)
  names(sim_in_date)<-c("date", "month", "day", "year", "simulated", "bias_corrected_daily")

  if(disaggregation_method=="scaling_coeff"){

    # Apply the bias corrected values to each month
    for (i_month in 1: length(df_bc_D[,6])){

      row_list<-which(sim_in_date[,2]==df_bc_D[i_month,2] & sim_in_date[,4]==df_bc_D[i_month,3])
      sim_in_date[row_list,6]=sim_in_date[row_list,5]/df_bc_D[i_month,7]

    } # i_month

  }


  if(disaggregation_method=="knn"){

    # Use the K-nearest Neighbors method to find the colsest month in the observed record
    for (i_month in 1: length(df_bc_D[,6])){

      closest_month<-knn(data = observed_month$x, query = df_bc_D[i_month,6], k=1)

      month_to_find<-observed_month[closest_month$nn.idx,1]
      year_to_find<-observed_month[closest_month$nn.idx,2]

      row_list_knn<-which(observed_in_date[,2]==month_to_find & observed_in_date[,4]==year_to_find)
      sim_in_date[row_list,6]=observed_in_date[row_list_knn,5]

    } # i_mont

  }




return(sim_in_date)
}
