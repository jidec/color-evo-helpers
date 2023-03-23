#' Print a full summary of a model
#'
#' @description Print summary, vifs, and r2_nakagawa if pglmm
#' @param cellsize_km the distance between each tile to grab
#' @return nothing

fullSummary <- function(model){
    library(car)
    library(performance)
    print(summary(model))
    print("VIFS:")
    print(vif(model))
    if(class(model) == "lmerModLmerTest"){
        library(performance)
        r2_nakagawa(model)
    }
}
