
GetData <- function(data_path, hist_type = 'RGB', hist_eq = TRUE, data_class = 1, bins_number = 300){
  # Load images
  reference_images <- load_images(data_path)
  # Color mode choice: HSL or RGB
  reference_data <- images_to_tab_data(images_list = reference_images,h_bins_nr = bins_number, hist_type = hist_type, histEq = hist_eq, data_class=data_class)
  # Dataset cleanup
  file_name_ref <- reference_data[1]
  class_only_ref <- reference_data[length(reference_data)]
  reference_data <- reference_data[-1]
  reference_data <- reference_data[-length(reference_data)]
  reference_data <- cbind(file_name_ref,reference_data,class_only_ref)
  return(reference_data)
}

GetDataHog <- function(data_path, cells_number = 30, orientations = 6, class = 0){
  res = HOG_apply(data_path, cells = cells_number, orientations = orientations, threads = 30)
  hog <- as.data.frame(res$hog)
  cls <- rep(list(c(class)), nrow(hog))
  hog$class <- cls
  return(hog)
}

GetIsolationForest <- function(reference_data, number_of_trees = 100, subsampling_size = 15){
  # Create an isolation forest with these parameters
  forest <- iForest(reference_data, number_of_trees, subsampling_size)
  return(forest)
}

PlotIsolationTree <- function(isolation_tree, precision = 2){
  plotTree(isolation_tree, precision)
}

GetAnomalyScores <- function(reference_data, faults_data, isolation_forest){
  ref_avg <- 0
  refs <- integer(nrow(reference_data))
  for (n in 1:nrow(reference_data)) {
    refs[n] <- as.numeric(GetAnomalyScore(reference_data[n,],isolation_forest))
    ref_avg <- ref_avg + refs[n]
  }
  ref_avg <- ref_avg / (nrow(reference_data))

  # For faulty examples
  flts_avg <- 0
  faults <- integer(nrow(faults_data))
  for (n in 1:nrow(faults_data)) {
    faults[n] <- as.numeric(GetAnomalyScore(faults_data[n,],isolation_forest))
    flts_avg <- flts_avg + faults[n]
  }
  flts_avg <- flts_avg / (nrow(faults_data))

  return(list(refs, faults))
}

PlotAnomalyScores <- function(refs, faults){
  PlotRefsAndFaults(refs, faults)
}

PlotROC <- function(refs, faults){
  auc_roc <- CalculateAUC_ROC(refs, faults)
  return(auc_roc)
}

PlotPR <- function(refs, faults){
  auc_pr <- CalculateAUC_PR(refs, faults)
  return(auc_pr)
}
