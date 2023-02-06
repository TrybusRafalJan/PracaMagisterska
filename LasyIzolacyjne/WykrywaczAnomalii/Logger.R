# Plot single decision tree from isolation forest
# Args:   tree - iTree to plot
# Returns: NA
plotTree <- function(tree, precision = "2"){
  SetGraphStyle(tree, rankdir = "TB")
  SetEdgeStyle(tree, label = function(x) sprintf(paste("%s %.", precision, "f", sep=""), x$sign, x$parent$split_value))
  SetNodeStyle(tree, 
               style = "filled,rounded", shape = "box",
               fillcolor = function(x)   if(x$isLeaf) {
                                           sprintf("LightBlue")
                                         }else{
                                           sprintf("White")
                                         },
               label = function(x)  if(x$isLeaf) {
                                      sprintf("%s examples ", x$examples_count)
                                    }else{
                                      sprintf("att[%s] \n%s examples ", x$split_attribute, x$examples_count)
                                    },
               fontcolor = "Black"
               )
  plot(tree)
}