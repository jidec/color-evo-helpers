
# trim traits and tree, keeping only obs in each that occur in both
# tree tips must be of the form "Genus sp" or "Genus sp subsp" or "Clade"
# tips must have spaces instead of '_' or '-"
# data must have a column named "clade"
# returns a tuple (list) where [[1]] is the trimmed traits and [[2]] is the trimmed tree
trimTraitsToTree <- function(df,tree_location){
    # read in the tree
    tree <- ape::read.tree(tree_location)
    tree$tip.label <- str_split_fixed(tree$tip.label," ",2)[,2]
    tree$tip.label <- str_replace(tree$tip.label,"'","")
    tree <- drop.tip(tree,tree$tip.label[!unique(df$clade) %in% tree$tip.label])
    df <- df[df$clade %in% tree$tip.label,]
    tree$edge.length[which(is.na(tree$edge.length))] <- 0
    return(list(df,tree))
}
