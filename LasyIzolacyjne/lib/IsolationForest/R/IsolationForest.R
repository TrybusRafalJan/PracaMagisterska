# Creates a single tree
# Args:   X - data
#         e - current tree height
#         l - height limit
# Returns: data.tree tree of nodes
iTree <- function(X, e, l){
    if(e > l || is.null(dim(X)) || dim(X)[1] <= 1 || dim(X)[2] <= 1){
      tree <- Node$new(sprintf("c%s", sample(1:9999999999, 1))) #Random ID
      if(is.null(dim(X))){
        tree$examples_count <- NCOL(X)
      }else{
        tree$examples_count <- NROW(X)
      }
    }else{
      # Number of attributes (length of the row)
      Q <- dim(X)[2] - 2
      # Random attribute
      q = sample(Q, 1) + 1
      # Generate random split point between min and max values from selected attribute values
      p = runif(1, min(as.numeric(X[,q])), max(as.numeric(X[,q])))
      # Split data into left and right branches
      X_left = X[as.numeric(X[,q]) < p,]
      X_right = X[as.numeric(X[,q]) >= p,]
      # Create root node
      tree <- Node$new(sprintf("c%s", sample(1:9999999999, 1))) #Random ID
      # Initialize parameters based on calculated values
      tree$examples_count <- dim(X)[1]
      tree$split_attribute <- q
      tree$split_value <- p
      # Create subtrees in left and right nodes
      left_tree = iTree(X_left, e+1, l)
      right_tree = iTree(X_right, e+1, l)
      # Assign comparison signs
      left_tree$sign <- "<"
      tree$AddChildNode(left_tree)
      right_tree$sign <- ">="
      tree$AddChildNode(right_tree)
    }
    return(tree)
}


# Creates a forest of iTrees
# Args:   X - data
#         t - number of trees
#         ss - subsampling size
# Returns: iForest object -> list of iTrees
iForest <- function(X, t, ss){
  # Setting height limit l
  l = ceiling(log2(ss))
  # Initializing empty forest vector
  forest <- vector("list", t)
  # Loop through the number of trees
  for(i in 1:t) {
    # Sample ss random examples from dataset
    x <- X[sample(nrow(X), ss),]
    # Construct a tree
    tree <- list(iTree(x, 0, l))
    # Append the tree to the forest
    forest[i] <- tree
  }
  # Return the whole forest
  return(forest)
}



# Average path length given n points
cn <- function(n) {
  if (n == 2) {
    return(1)
  } else if (n < 2) {
    return(0)
  } else {
    H = log(n - 1) + 0.5772156649 # Euler's constant
    return(2 * H - (2*(n - 1)/n))
  }
}


# Calculates anomaly score
# Args:   example -> example for caluclating anomaly score
#         forest -> isolation forest (root)
# Returns: float -> anomaly score
GetAnomalyScore <- function(example, forest){
  E <- (GetMeanOfPathLengths(example, forest))
  c <- cn(forest[[1]]$examples_count)
  return(2^(-(E/c)))
}


# Calculates mean of path lengths throughout the forest
# Args:   example -> example for caluclating path lengths
#         forest -> isolation forest (root)
# Returns: float -> path length average
GetMeanOfPathLengths <- function(example, forest){
  avg <- 0.0
  for(tree in forest){
    pl <- PathLength(tree, example, 0)
    avg <- avg + pl
  }
  avg = avg / length(forest)
  return(avg)
}


# Recursively calculateds path length of given example expanded by theretical cn value depending on examples in given leaf
# Args:   node - forest node (root initially, then recursively passed further nodes)
#         example -> example for caluclating path length
#         e -> current depth
# Returns: float -> path length + cn(examples_count)
PathLength <- function(node, example, e) {
  if(node$isLeaf){
    # Return depth + cn(number of examples used for trainging the tree in this leaf)
    return(e) + cn(node$examples_count)
  }else{
    # Select splits
    split_att <- node$split_attribute
    split_val <- node$split_value

    children <- node$children

    # Compare value based on split and go to the left or right child based on that
    isValLess <- (example[split_att] < split_val)
    if(children[[1]]$sign == "<"){
      if(isValLess){
        return(PathLength(children[[1]], example, e+1))
      }else{
        return(PathLength(children[[2]], example, e+1))
      }
    }else{
      if(isValLess){
        return(PathLength(children[[2]], example, e+1))
      }else{
        return(PathLength(children[[1]], example, e+1))
      }
    }
  }
}


# Obtains automatically calculated threshold to determine whether
#   example is representing fault or regular features
# Args:   refs - reference data anomaly scores calculated from isolation forest
#         faults - fault data anomaly scores calculated from isolation forest
# Returns: float -> threshold
GetThreshold <- function(refs, faults){
  minFault = min(faults)
  maxRef = mean(refs)
  threshold = mean(c(minFault, maxRef))
  return(threshold)
}


# Predicts whether example is faulty or not depending on forest and threshold
# Args:   example -> example for testing
#         forest -> isolation forest
#         threshold -> float, obtained from GetThreshold()
# Returns:  "Fault" if anomaly determined
#           "Regular" if no anomaly determined
Predict <- function(example, forest, threshold){
  anomalyScore <- GetAnomalyScore(example, forest)
  if(anomalyScore >= threshold){
    return("Fault")
  }else{
    return("Regular")
  }
}

