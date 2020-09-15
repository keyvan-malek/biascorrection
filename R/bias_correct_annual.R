
#-----------------------------------------------------------------------------------------------------
#######                                  ANNUAL BIAS CORRECTION
#
#
# This function multiply the monthly bias corrected flow by a coefficient to match it with the simulated
# The assumption is that the hydrological models are able to simulate annual flow reasonably well
# The output dataframe is monthly and inputs are also monthly
#-----------------------------------------------------------------------------------------------------

#' @title  Annual correction of monthly bias-corrected data
#'
#' @description  This function makes sure that the magnitude of annual simulated streamflow is consistent with the simulations
#'
#' @param simulated Simulated time series
#' @param bias_corrected Monthly bias-corrected time series
#' @param start_date_simulated Start date of simulated time series. For example: "1929-01-01"
#' @param end_date_simulated End date of simulated time series. For example: "1929-01-01"
#'
#'
#' @export
#'@import RANN

#library("RANN")

bias_correct_annual<-function(
                    simulated,               # simulated streamflow e.g., "2006-12-31"
                    bias_corrected,          # monthly bias corrected streamflow using quantile mapping
                    start_date_simulated,    # start date of simulated flow
                    end_date_simulated       # end date of simulated flow
){

  start_year<-as.numeric(format(as.Date(start_date_simulated, '%Y-%m-%d'),'%Y'))
  last_year<-as.numeric(format(as.Date(end_date_simulated, '%Y-%m-%d'),'%Y'))
  nyear<-last_year-start_year+1

  m_bc_annual<-0


  for (i in 1:nyear){

    start_point<-(i-1)*12+1
    sim_year_i<-simulated[start_point:(start_point+11)]
    bc_year_i<-bias_corrected[start_point:(start_point+11)]

    # calculates adjustment coeficient
    F=sum(sim_year_i)/sum(bc_year_i)

    #print(paste("The scaling coefficient in year = ", i, "equals",  F))

    m_bc_annual[start_point:(start_point+11)]=bc_year_i*F
  }

  return(m_bc_annual)
}
