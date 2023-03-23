#' Create and plot a lmer or pglmm
#'
#' @description
#' @param df to use columns from
#' @param response name of response column
#' @param formula string containing formula i.e. lat + temp + precip
#' @return the model

getPlotLM <- function(df,model_type="lmer",response,formula,scale=TRUE,plotmodel_terms=c(),save_name=NULL,phy=NULL,log_transf_colnames=NULL){
    library(phyr)
    library(lme4)
    library(dplyr)
    library(sjPlot)

    # log transform
    if(!is.null(log_transf_colnames)){
        for(c in log_transf_colnames){
            df[,c] <- log(df[,c])
        }
    }
    # scale
    if(scale){
        df <- scaleDf(df,all_numerics = TRUE)
    }

    if(model_type == "lmer"){
        m <- eval(parse(text=paste0("lmer(",response,"~",formula,", data = df)"))) # create model using response and formula
        plot(plot_model(m)) # plot it
        for(t in plotmodel_terms){ # plot it by each of its terms
            plot(plot_model(m,type="pred",terms=t))
        }
    }
    if(model_type == "pglmm"){
        m <- eval(parse(text=paste0("pglmm(",response,"~",formula,", data = df, cov_ranef = list(species=phy), bayes=T)")))
        plot(plot_bayes(m))
    }

    # save
    if(is.null(save_name)){
        save_name <- paste(deparse(substitute(df)),response,formula,".rds",sep='_')
    }
    saveRDS(m,file=paste0("saved/models/",save_name))

    fullSummary(m)

    return(m)
}
