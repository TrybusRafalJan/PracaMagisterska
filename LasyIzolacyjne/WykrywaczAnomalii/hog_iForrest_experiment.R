# Source other files
if(!exists("calculate_rgb_histogram", mode="function")) source("HistogramCalculations.R")
if(!exists("cropHistogramRange", mode="function")) source("batchProcesses.R")
if(!exists("GetAnomalyScore", mode="function")) source("IsolationForest.R")
if(!exists("plotTree", mode="function")) source("Logger.R")
if(!exists("CalculateAUC_PR", mode="function")) source("Measures.R")

library(OpenImageR)

start_time <- Sys.time()

# Import and process dataset
FOLDER_path_nok = "imgs/left/nok/"
res_nok = HOG_apply(FOLDER_path_nok, cells = 30, orientations = 6, threads = 30)

hog_nok <- as.data.frame(res_nok$hog)
cls <- rep(list(c(0)), nrow(hog_nok))
hog_nok$class <- cls

FOLDER_path_ok = "imgs/left/ok/"
res_ok = HOG_apply(FOLDER_path_ok, cells = 30, orientations = 6, threads = 30)

hog_ok <- as.data.frame(res_ok$hog)
cls <- rep(list(c(1)), nrow(hog_ok))
hog_ok$class <- cls

hog_data <- rbind(hog_ok,hog_nok) 

# Set isolation forest parameters
numberOfTrees = 3000
subsamplingSize = 15

# Create forest using isolation forest algorithm
forest <- iForest(hog_ok, numberOfTrees, subsamplingSize)

# Any tree from the forest may be plotted as an example
#plotTree(forest[[3]], 7)

# Calculate and print anomaly scores for information purposes
# For reference examples
print("Ref:")
ref_avg <- 0
refs <- integer(nrow(hog_ok))
for (n in 1:nrow(hog_ok)) {
  refs[n] <- as.numeric(GetAnomalyScore(hog_ok[n,],forest))
  ref_avg <- ref_avg + refs[n]
}
ref_avg <- ref_avg / (nrow(hog_ok))
print("avg score on ref: ")
print(ref_avg)

# For faulty examples
print("Faults:")
flts_avg <- 0
faults <- integer(nrow(hog_nok))
for (n in 1:nrow(hog_nok)) {
  faults[n] <- as.numeric(GetAnomalyScore(hog_nok[n,],forest))
  flts_avg <- flts_avg + faults[n]
}
flts_avg <- flts_avg / (nrow(hog_nok))
print("avg score on flts: ")
print(flts_avg)

# Merge and label anomaly scores dataframe
refs_faults = integer(length(refs) + length(faults))
for (n in 1:length(refs)) {
  refs_faults[n] = 1
}

# Calculate and plot different measures
# 1. Plot anomaly scores and measures
PlotRefsAndFaults(refs, faults)
# 2. Print AUC and plot ROC
aur_roc <- CalculateAUC_ROC(refs, faults)
print(aur_roc)
# 2. Print AUC and plot PR
auc_pr <- CalculateAUC_PR(refs, faults)
print(auc_pr)

end_time <- Sys.time()