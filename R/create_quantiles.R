
#-----------------------------------------------------------------------------------------------------------
#######                                  CREATE MONTHLY QUANTILES
#
#
#
# This function is called from the "bias_correct_monthly" functions and converst
# a data frame of input data into its monthly quantiles.
#----------------------------------------------------------------------------------------------------------


#' @title  This function is called to create monthly quantiles
#'
#' @description  This function is called from the "bias_correct_monthly" functions and converst a data frame of input data into its monthly quantiles.
#'
#' @param df_input
#' @param n_year
#'


################## Function to create quantiles

create_quantiles<-function(df_input,     # Input data, the format needs to be R dataframe
                           n_year        # Number of years
                           ){

  # Create a dataframe to store monthly quantiles
  df_quntiles<- data.frame(matrix(ncol = 13, nrow = n_year))
  # Assign names to the columns.
  names(df_quntiles)<-c("quantile", "month_1", "month_2","month_3", "month_4", "month_5", "month_6", "month_7", "month_8", "month_9", "month_10", "month_11", "month_12")

  for (i_month in 1:12){

    month_idx=i_month
    if(i_month==1){
      # Calculate percentile levels
      df_quntiles[, 1]<-seq(1,n_year)/n_year
    }

    # Aggregate the daily data into monthly
    m_df_obs<-aggregate(df_input[which(df_input[,2]==month_idx),5], by=list(df_input[which(df_input[,2]==month_idx),2], df_input[which(df_input[,2]==month_idx),4]) , mean)
    # Sort the data
    #1:length(m_df_obs[,1])
    #m_df_obs=m_df_obs[1:n_year,]
    df_quntiles[, i_month+1]<-sort(m_df_obs$x, decreasing = T)
  } # i_month

  return(df_quntiles)
}



