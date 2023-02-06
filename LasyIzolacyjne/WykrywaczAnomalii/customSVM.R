### IMPORT SKRYPTU Z FUNKCJAMI DLA PROCESOWANIA ZBIORU
if(!exists("fun", mode="function")) source("BatchProcesses.R")
### IMPORT SKRYPTU Z FUNKCJAMI LICZĄCYMI HISTOGRAMY
if(!exists("fun", mode="function")) source("HistogramCalculations.R")

### DEFINICJE ZMIENNYCH PROJEKTOWYCH
path_to_dataset_ref = "imgs/wszystkie/reference"
path_to_dataset_faults = "imgs/wszystkie/faults"
nr_of_bins = 20


### Wczytanie zdjęć
images_list_ref <- load_images(path_to_dataset_ref)
data_ref <- images_to_tab_data(images_list = images_list_ref,h_bins_nr = nr_of_bins, hist_type = 'RGB',histEq = TRUE,data_class = 1)

images_list_faults <- load_images(path_to_dataset_faults)
data_faults <- images_to_tab_data(images_list = images_list_faults,h_bins_nr = nr_of_bins, hist_type = 'RGB',histEq = TRUE, data_class = 0)

data <- rbind(data_ref,data_faults)


data <- data[-1] ### do SVM'a pozbywamy się nazwy zdjęcia

data_classOnly <- data[length(data)]
data <- data[-length(data)]

#data <- cropHistogramRange(data,c(0,175),c(0,255),c(180,255))

data <-cbind(data,data_classOnly)

### W naszym przypadku nie potrzebne bo klasy od razu są 0 i 1 ale gdyby były opsiowe to tutaj ich kodowanie
data$class <- factor(data$class, levels = c(0, 1))

#Przetasowanie danych
data <- data[sample(1:nrow(data)), ]






# SVM
#install.packages('e1071')
library(e1071)
library(caret)

svm_algorithm <- "cclass"
#svm_algorithm <- "oneclass"

switch(svm_algorithm, 
       cclass={
               
               # Potrzebna libka
               #install.packages('caTools')
               library(caTools)
               
               # Podział na dane treningowe i testowe
               set.seed(123)
               split = sample.split(data$class, SplitRatio = 0.6) # to zwraca wektor zawierające TRUE i FALSE #0.75
               training_set = subset(data, split == TRUE)
               test_set = subset(data, split == FALSE)
               
               training_set[-length(training_set)] <- as.data.frame(lapply(training_set[-length(training_set)], as.numeric))
               test_set[-length(test_set)] <- as.data.frame(lapply(test_set[-length(test_set)], as.numeric))
               
               training_set[-length(training_set)] <- scale(training_set[-length(training_set)])
               test_set[-length(test_set)] <- scale(test_set[-length(test_set)])
               
               classifier = svm(formula = class ~ .,
                                data = training_set,
                                type = 'C-classification',
                                kernel = 'linear')
               
               # predykcja
               y_pred = predict(classifier, newdata = test_set[-length(test_set)])
               
               # Making the Confusion Matrix
               cm = table(test_set[, length(test_set)], y_pred)
               
               confusionMatrix(cm)
               
               
       },
       oneclass={
               
               data_1c <- data # data dla 1c (one_class)
               
               
               ### dodajemy kolumnę 'class_fault' TRUE dla klasy fault, którą chcemy wykryć
               data_1c$class_fault[data_1c$class=="0"] <- "TRUE" 
               data_1c$class_fault[data_1c$class!="0"] <- "FALSE" 
               
               ### Wydzielamy zbiory trenujący (tylko wadliwe [TRUE] - jak dla drzewa) i testujący (tylko dobre[FALSE])
               trainPositive<-subset(data_1c,class_fault=="TRUE") # tu zmiana na 0 # do trenowana u mnie falutsy - 0
               testnegative<-subset(data_1c,class_fault=="FALSE") # 1 # do testowania u mnie refferenc - 1
               inTrain<-createDataPartition(1:nrow(trainPositive),p=0.6,list=FALSE) # indeksy do trenowania
               
               trainpredictors<-trainPositive[inTrain,1:30] # ekstrakcja samych cech
               trainLabels<-trainPositive[inTrain,32] # ekstrakcja amych etykiet
               
               testPositive<-trainPositive[-inTrain,]
               testPosNeg<-rbind(testPositive,testnegative)
               
               testpredictors<-testPosNeg[,1:30]
               testLabels<-testPosNeg[,32]
               
               trainpredictors <- as.data.frame(lapply(trainpredictors, as.numeric))
               testpredictors <- as.data.frame(lapply(testpredictors, as.numeric))
               
               svm.model<-svm(trainpredictors,y=NULL,
                              type='one-classification',
                              nu=0.10,
                              scale=TRUE,
                              kernel="radial")
               
               svm.predtrain<-predict(svm.model,trainpredictors)
               svm.predtest<-predict(svm.model,testpredictors)
               
               confTrain<-table(Predicted=svm.predtrain,Reference=trainLabels)
               confTest<-table(Predicted=svm.predtest,Reference=testLabels)
               
               confusionMatrix(confTest,positive='TRUE')
               
               print(confTrain)
               print(confTest)
               
               
               
               
               
               
       },
       {
               print('zly parametr svm_algorithm')
       }
)



