
# scale every numeric column in a dataframe and return
scaleNumeric <- function(df){
    return(df %>% mutate(across(where(is.numeric), scale)))
}
