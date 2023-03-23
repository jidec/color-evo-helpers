#' Scale specific columns (or all numeric cols) in a dataframe
#'
#' @param df dataframe
#' @param colnames character vector of column names to scale
#' @param all_numerics whether to just scale all the numeric cols
#' @return scaled df
#'
scaleDf <- function(df,colnames,all_numerics=FALSE){
    library(dplyr)
    if(all_numerics){
        # scale all numeric columns
        df <- df %>% mutate(across(where(is.numeric), scale))
    }
    else{
        df <- as.data.frame(lapply(df[, colnames], scale))
    }
    return(df)
}
