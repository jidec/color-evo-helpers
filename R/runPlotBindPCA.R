# run a PCA, plot it, and bind it to the dataframe
# returns a tuple of the DF with PCs bound at [[1]], the rotation at [[2]] and the loadings at [[3]]
# pca_columns is a vector
runPlotBindPCA <- function(df,pca_columns,colour_column=NULL){
    library(dplyr)
    library(ggfortify)
    pca <- prcomp(dplyr::select(df,pca_columns))
    print(summary(pca))
    rotation <- pca$rotation
    print("Rotation:")
    print(rotation)
    df <- cbind(df,pca$x)
    plot(autoplot(pca,data=df,colour=colour_column,loadings=TRUE,loadings.label=TRUE))
                  #frame=TRUE,frame_type='norm'))
    loadings <- pca$loadings
    return(list(df,rotation,loadings))
}

#data(iris)
#iris2 <- runPlotBindPCA(iris,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),colour_column = 'Species')
#head(iris)


