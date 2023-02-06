# Source other files
if(!exists("calculate_rgb_histogram", mode="function")) source("HistogramCalculations.R")
if(!exists("cropHistogramRange", mode="function")) source("batchProcesses.R")
if(!exists("GetAnomalyScore", mode="function")) source("IsolationForest.R")
if(!exists("plotTree", mode="function")) source("Logger.R")
if(!exists("CalculateAUC_PR", mode="function")) source("Measures.R")


## DLA POJEDYNCZEJ PŁYTKI - DEMONSTRACJA DZIAŁANIA + PLOT HISTOGRAMU
# im <- load.image("partimg.png")
# RGB.row <- calculate_rgb_histogram(im,20)
# hsv <-calculate_hsv_histogram(im,20)
# rgb <-calculate_rgb_histogram(im,20)



# Import and process dataset
reference_path = "imgs/left/ok"
faults_path = "imgs/left/nok"

# Load images
reference_images <- load_images(reference_path)
faults_images <- load_images(faults_path)

# Nth picture preview
#inspect_image(images_list = reference_images, im_idx = 2, h_bins_nr = 30,hist_type='RGB')

# Color mode choice: HSL or RGB
reference_data <- images_to_tab_data(images_list = reference_images,h_bins_nr = 100, hist_type = 'HSL', histEq = FALSE, data_class=1)
faults_data <- images_to_tab_data(images_list = faults_images,h_bins_nr = 100, hist_type = 'HSL', histEq = FALSE, data_class=0)

# Dataset cleanup
file_name_ref <- reference_data[1]
file_name_flts <- faults_data[1]

class_only_ref <- reference_data[length(reference_data)]
class_only_flts <- faults_data[length(faults_data)]

reference_data <- reference_data[-1]
faults_data <- faults_data[-1]

reference_data <- reference_data[-length(reference_data)]
faults_data <- faults_data[-length(faults_data)]





# Histogram cropping
#reference_data <- cropHistogramRange(reference_data,c(0,175),c(0,255),c(180,255))
#faults_data <- cropHistogramRange(faults_data,c(0,175),c(0,255),c(180,255))

reference_data <- cbind(file_name_ref,reference_data,class_only_ref)
faults_data <- cbind(file_name_flts,faults_data,class_only_flts)
#reference_data
#write.csv(reference_data,"images_as_table.csv", row.names = FALSE,col.names = FALSE)


# Create an isolation forest with these parameters
numberOfTrees = 3000
subsamplingSize = 15
forest <- iForest(reference_data, numberOfTrees, subsamplingSize)

# Plot sample decision tree from forest
#plotTree(forest[[3]])

# Calculate and print anomaly scores for information purposes
# For reference examples
print("Ref:")
ref_avg <- 0
refs <- integer(nrow(reference_data))
for (n in 1:nrow(reference_data)) {
  refs[n] <- as.numeric(GetAnomalyScore(reference_data[n,],forest))
  ref_avg <- ref_avg + refs[n]
}
ref_avg <- ref_avg / (nrow(reference_data))
print("avg score on ref: ")
print(ref_avg)

# For faulty examples
print("Faults:")
flts_avg <- 0
faults <- integer(nrow(faults_data))
for (n in 1:nrow(faults_data)) {
  faults[n] <- as.numeric(GetAnomalyScore(faults_data[n,],forest))
  flts_avg <- flts_avg + faults[n]
}
flts_avg <- flts_avg / (nrow(faults_data))
print("avg score on flts: ")
print(flts_avg)

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