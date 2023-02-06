
# Calculate measures
# Args:   refs - Anomaly Scores for reference examples
#         faults - Anomaly Scores for faulty examples
# Returns: list of precision, recall and F1 measures
CalculateRecallPrecisionAndF1 <- function(refs, faults){
  # Get automatically calculated threshold
  threshold <- GetThreshold(refs, faults)
  # Merge into 1 dataframe
  merged <- data.frame(c(refs, faults))
  names(merged) <- c("AnomalyScore")
  merged["TrueClass"] <- c(rep("Ref", length(refs)), rep("Fault", length(faults)))
  merged$Predicted <- ifelse(merged$AnomalyScore > threshold, "Fault", "Ref")
  # Calculate measures
  TP <- nrow(merged[merged$TrueClass == "Fault" & merged$Predicted == "Fault",])
  TN <- nrow(merged[merged$TrueClass == "Ref" & merged$Predicted == "Ref",])
  FP <- nrow(merged[(merged$TrueClass == "Ref" & merged$Predicted == "Fault"),])
  FN <- nrow(merged[(merged$TrueClass == "Fault" & merged$Predicted == "Ref"),])

  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- (2*precision*recall) / (precision + recall)

  return(c(precision, recall, F1))
}



# Plot reference and faulty anomaly scores + measures
# Args:   refs - Anomaly Scores for reference examples
#         faults - Anomaly Scores for faulty examples
# Returns: NA
PlotRefsAndFaults <- function(refs, faults){

  # Prepare data for plotting
  refs_faults = integer(length(refs) + length(faults))
  for (n in 1:length(refs)) {
    refs_faults[n] = 1
  }

  # Caluclate threshold and measures
  threshold = GetThreshold(refs, faults)
  measures <- CalculateRecallPrecisionAndF1(refs, faults)

  # Plot anomaly scores
  plot( c(refs, faults), refs_faults,
        type="p", col=c(rep("blue", times=length(refs)), rep("red", times=length(faults))),
        main="Wykres anomalii", xlab="Miara anomalii [0-1]", ylab="", yaxt="none"
  )
  axis(2, at=c(0,1), labels = FALSE)
  text(y = c(0,1), par("usr")[1] - 0.002, labels = c("Anomalia", "Przykłady referencyjne"), srt = 70, pos = 2, xpd = TRUE)
  par(mar=c(5,6,4,1)+.1)

  # Plot threshold
  abline(v = threshold, col="grey50")
  text(threshold,0.5, "granica anomalii", col = "grey50", adj = c(-.1, 0))
  text(threshold,0.5, paste("v = ", format(round(threshold, 2), nsmall = 2)), col = "grey50", adj = c(-.2, 1.5))

  maxX <- max(c(refs, faults))
  minX <- min(c(refs, faults))
  rangeX <- abs(maxX - minX)

  # Write measures on plot
  text(maxX - rangeX/4,0.75, paste("Precision = ", format(round(measures[1], 2), nsmall = 2)), col = "grey50", adj = c(0.0, 0.0))
  text(maxX - rangeX/4,0.65, paste("Recall = ", format(round(measures[2], 2), nsmall = 2)), col = "grey50", adj = c(0.0, 0.0))
  text(maxX - rangeX/4,0.55, paste("F1 = ", format(round(measures[3], 2), nsmall = 2)), col = "grey50", adj = c(0.0, 0.0))
}


# Plot ROC and calculate AUC
# Args:   refs - Anomaly Scores for reference examples
#         faults - Anomaly Scores for faulty examples
# Returns: AUC ROC
CalculateAUC_ROC <- function(refs, faults){
  merged <- data.frame(c(refs, faults))
  names(merged) <- c("AnomalyScore")
  merged["TrueClass"] <- c(rep(0, length(refs)), rep(1, length(faults)))
  pred <- prediction(merged$AnomalyScore, merged$TrueClass)
  perf <- performance(pred,measure = "tpr", x.measure = "fpr");
  plot(perf,
       colorize=TRUE,
       avg="threshold",
       lwd=3,
       main="Wykres ROC")
  abline(a=0, b=1, col="grey50")

  auc.tmp <- performance(pred,"auc");
  auc <- as.numeric(auc.tmp@y.values)
  return(auc)
}


# Plot PR and calculate AUC
# Args:   refs - Anomaly Scores for reference examples
#         faults - Anomaly Scores for faulty examples
# Returns: AUC PR
CalculateAUC_PR <- function(refs, faults){
  merged <- data.frame(c(refs, faults))
  names(merged) <- c("AnomalyScore")
  merged["TrueClass"] <- c(rep(0, length(refs)), rep(1, length(faults)))
  pred <- prediction(merged$AnomalyScore, merged$TrueClass)
  perf <- performance(pred,measure = "prec", x.measure = "rec");
  plot(perf,
       colorize=TRUE,
       avg="threshold",
       lwd=3,
       ylim=c(0,1),
       main="Wykres PR")
  abline(a=0, b=1, col="grey50")

  auc.tmp <- performance(pred,"aucpr");
  auc <- as.numeric(auc.tmp@y.values)
  return(auc)
}

