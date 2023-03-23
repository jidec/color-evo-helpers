#' Create and plot a logistic regression model
#'
#' @description
#' @param df to use columns from
#' @param response name of response column
#' @param formula string containing formula i.e. lat + temp + precip
#' @return the model

getPlotLogReg  <- function(df, response, formula){
    library(rsq)
    library(sjPlot)
    library(pROC)
    m <- eval(parse(text=paste0("glm(",response,"~",formula,", data = df, family=binomial)")))
    print(summary(m))
    #print("RSQ:")
    #print(rsq(m))

    # plot the coefficients and their confidence intervals
    plot_model(m, type = "est", vline.color = "gray")

    # predict response probabilities for the training data
    #train_prob <- predict(m, type = "response")

    #length(train_prob)
    #length(df[,response])

    # create a ROC curve object
    #roc_data <- roc(df[,response], train_prob)

    # plot the ROC curve
    #plot(roc_data, print.auc = TRUE, legacy.axes = TRUE)

    return(m)
}

# data(iris)
# iris$BigSepalL <- as.factor(iris$Sepal.Length > mean(iris$Sepal.Length))
# iris$BigSepalW <- as.factor(iris$Sepal.Width > mean(iris$Sepal.Width))
# df <- iris
# response <- "BigSepalL"
# formula <- "Sepal.Width + Petal.Length + Petal.Width"

