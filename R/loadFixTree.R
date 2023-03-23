
loadFixTree <- function(tree_location,replace_underscores=T,remove_commas=T,remove_first_split=F){
    library(stringr)
    library(ape)
    tree <- ape::read.tree(tree_location) # read in the tree
    if(replace_underscores){
        tree$tip.label <-  str_replace(tree$tip.label,"_"," ")
    }
    if(remove_first_split){
        tree$tip.label <- str_split_fixed(tree$tip.label," ",2)[,2] #
    }
    if(remove_commas){
        tree$tip.label <- str_replace(tree$tip.label,"'","")
    }
    return(tree)
}
