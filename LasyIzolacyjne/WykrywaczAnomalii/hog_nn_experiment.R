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

data <- hog_data[sample(1:nrow(hog_data)), ]