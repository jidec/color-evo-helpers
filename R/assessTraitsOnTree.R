#' Plot phytools contMaps of one or two traits, their phylogenetic signature,
#' their phylo correlation, and their diversification rate
#'
#' @description Load a phylo tree, trim the df to the tree,
#' match the order of the tips to the trait, plot contMap and compute phylo sig
#' @param df df containing trait
#' @param trait_colname colname for the trait
#' @param tree_location file loc of the tree (read using ape::read.tree)
#' @param replace_underscores whether underscores should be replaced with spaces in the loaded tree
#'
#' @return nothing

# tree_location = "data/odonata.tre"
# df = wings_sp
# df$flight_type[is.na(df$flight_type)] <- "intermediate"
# trait_colname = "brown"
# trait_colname2 = "black"
# replace_underscores=T
# remove_commas=T
# remove_first_split=F
#
# assessTraitsOnTree(df,trait_colname="black",trait_colname2="all_fore_pc1",tree_location="data/odonata.tre")
# df$all_fore_pc1[is.nan(df$all_fore_pc1)] <- 0

assessTraitsOnTree <- function(df,trait_colname,trait_colname2, tree_location,replace_underscores=T,remove_commas=T,remove_first_split=F){
    library(ape)
    library(phytools)
    library(dplyr)

    # get tips
    tips <- tree$tip.label

    trimmed_tuple <- trimDfToTree(df,tree_location,replace_underscores,remove_commas,remove_first_split)

    df <- trimmed_tuple[[1]]
    tree <- trimmed_tuple[[2]]

    t1map <- plotOnTree(df,trait_colname)
    if(!is.null(trait_colname2)){
        t2map <- plotOnTree(df,trait_colname2)
        par(mfrow=c(1,2))
        plot(t1map,ftype="off",lwd=2)

        plot(t2map,direction="leftwards",ftype="off",lwd=2)

        t <- cbind(df[,trait_colname],df[,trait_colname2])
        rownames(t) <- df$clade

        if(TRUE){ #class(t[,1]) == "numeric" & class(t[,2]) == "numeric"

            library(nlme)
            spp<-rownames(t)

            # create a correlation structure that defines the distr of the residuals
            # to be in accordance with our phylogeny
            corBM <- corBrownian(phy=tree,form=~spp)

            # run pgls for cor between traits
            m <- eval(parse(text=paste0("gls(",trait_colname,"~",trait_colname2,", data = df)")))
            #m <- gls(black ~ brown,data=df,)
            print(summary(m))

            #obj<-phyl.vcv(t,vcv(tree),1)
            ## correlation between x & y
            #r.xy<-cov2cor(obj$R)

            ## t-statistic & P-value
            #t.xy<-r.xy*sqrt((Ntip(phy2)-2)/(1-r.xy^2))
            #P.xy<-2*pt(abs(t.xy),df=Ntip(phy2)-2,lower.tail=F)
        }
    }
}

# returns the trait, either discrete or continuous, modeled on the phylo
plotOnTree <- function(df,trait_colname){

    # prep traits for asr
    trait <- as.vector(df[,trait_colname]) #change this to meanMD, meanHour, propNight etc
    trait <- unlist(trait)
    names(trait) <- df$species

    # unlist tree tips
    tree$tip.label <- unlist(tree$tip.label)

    #makes trait indices match order of tree tips
    matchOrder <- function(tree, traitvect)
    {
        ordered <- traitvect
        for(i in 1:length(tree$tip.label))
        {
            tipstr <- tree$tip.label[i]
            for(ii in 1:length(traitvect))
            {
                if(tipstr == names(traitvect[ii]))
                {
                    ordered[i] <- traitvect[ii]
                    names(ordered)[i] <- tipstr
                }
            }
        }
        return(ordered)
    }

    trait <- matchOrder(tree, trait)

    # replace NaN edge lengths with 0
    tree$edge.length <- ifelse(is.nan(tree$edge.length),0, tree$edge.length)
    tree$edge.length[tree$edge.length == 0] <- 0.1

    # if numeric
    if(class(trait) == "numeric"){
        # create contMap for all genera
        obj <- contMap(tree,trait,method="anc.ML",fsize = 0)#,lims=c(-0.15,0.4874))
        #errorbar.contMap(obj)

        cex <- 1
        #if(!tipnames){
        #    cex <- 0
        #}
        plot(obj,type="fan",legend=0.7*max(nodeHeights(tree)), fsize=c(0.6,0.9),cex=0.5,show.tip.label=FALSE)

        # compute phylogenetic signal
        physig <- phylosig(tree, trait, method="K", test=TRUE, nsim=1000, se=NULL, start=NULL,
                           control=list())
        physigL <- phylosig(tree, trait, method="lambda", test=FALSE, nsim=1000, se=NULL, start=NULL,
                            control=list())
        print(physig)
        plot(physig,las=1,cex.axis=0.9)
        print(physigL)

        return(obj)
    }
    # if factor
    else{
        smap.trees <- make.simmap(tree,trait,model="ER",
                                  nsim=10)
        cols<-setNames(c("green","#E4D96F","darkgreen"),
                       levels(trait))

        plot(smap.trees[[1]],cols,fsize=0.5,ftype="i",outline=TRUE)
        add.simmap.legend(colors=cols,prompt=FALSE,x=0,y=-0.5,
                          vertical=FALSE)

        print(describe.simmap(smap.trees))
        return(smap.trees[[1]])
    }
}
