# run a PCA, plot it, and bind it to the dataframe
# returns a tuple of the DF with PCs bound at [[1]] and the PCA loadings at [[2]]
# pca)columns is a vector
runPlotBindPCA <- function(df,pca_columns){
    pca <- prcomp(df[,pca_columns])
    print(summary(pca))
    df <- cbind(df,pca$x)
    loadings <- pca$rotation
    return(list(df,loadings))
}
