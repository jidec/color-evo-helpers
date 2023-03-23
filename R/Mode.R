#' Get the mode of a numeric
#'
#' @param x

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
