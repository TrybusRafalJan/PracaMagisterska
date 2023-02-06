library(neuralnet)
library(caret) 

### IMPORT SKRYPTU Z FUNKCJAMI DLA PROCESOWANIA ZBIORU
if(!exists("fun", mode="function")) source("BatchProcesses.R")
### IMPORT SKRYPTU Z FUNKCJAMI LICZĄCYMI HISTOGRAMY
if(!exists("fun", mode="function")) source("HistogramCalculations.R")

### DEFINICJE ZMIENNYCH PROJEKTOWYCH
path_to_dataset_ref = "imgs/left/ok"
path_to_dataset_faults = "imgs/left/nok"
nr_of_bins = 100

### Wczytanie zdjęć
images_list_ref <- load_images(path_to_dataset_ref)
data_ref <- images_to_tab_data(images_list = images_list_ref,h_bins_nr = nr_of_bins, hist_type = 'RGB', histEq = FALSE, data_class="OK")


images_list_faults <- load_images(path_to_dataset_faults)
data_faults <- images_to_tab_data(images_list = images_list_faults,h_bins_nr = nr_of_bins, hist_type = 'RGB', histEq = FALSE, data_class="NOK")
data <- rbind(data_ref,data_faults)


data <- data[-1] ### do SVM'a pozbywamy się nazwy zdjęcia

### W naszym przypadku nie potrzebne bo klasy od razu są 0 i 1 ale gdyby były opsiowe to tutaj ich kodowanie
#data$class <- factor(data$class, levels = c(0, 1))

#Przetasowanie danych
data <- data[sample(1:nrow(data)), ]

indexes=createDataPartition(data$class, p=.60, list = F) # indexes for test and train

train_ = data[indexes, ]
train_[-length(data)] <- as.data.frame(lapply(train_[-length(data)], as.numeric))

test_ = data[-indexes, ]
test_[-length(data)] <- as.data.frame(lapply(test_[-length(data)], as.numeric))

xtest_ = test_[, -length(data)]
ytest_ = test_[, length(data)] 

nnet_=neuralnet(class~., train_, hidden = c(length(data),3), linear.output = FALSE)

#plot(nnet_) 

ypred_ = neuralnet::compute(nnet_, xtest_) 
yhat_ = ypred_$net.result
print(yhat_)

yhat_=data.frame("yhat_"=ifelse(max.col(yhat_[ ,1:2])==1, "NOK", "OK"
                              ))

cm=confusionMatrix(as.factor(ytest_), as.factor(yhat_$yhat_))
print(cm) 

