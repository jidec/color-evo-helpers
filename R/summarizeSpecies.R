
summarizeSpecies <- function(d){
    library(dplyr)
    d <- d %>%
        group_by(species) %>%
        summarize(across(where(is.numeric), mean, na.rm = TRUE),
                  across(where(~!is.numeric(.)), Mode),
                  n=n())
    return(d)
}
