
# trim traits and tree, keeping only obs in each that occur in both
# tree tips must be of the form "Genus sp" or "Genus sp subsp" or "Clade"
# tips must have spaces instead of '_' or '-"
# data must have a column named "clade"
# returns a tuple (list) where [[1]] is the trimmed traits and [[2]] is the trimmed tree

trimDfToTree <- function(df,tree_location,replace_underscores=T,remove_commas=T,remove_first_split=F){
    tree <- loadFixTree(tree_location,replace_underscores,remove_commas,remove_first_split)

    tree <- drop.tip(tree,tree$tip.label[!tree$tip.label %in% unique(df$clade)])
    df <- df[df$clade %in% tree$tip.label,]
    tree$edge.length[which(is.na(tree$edge.length))] <- 0
    return(list(df,tree))
}
