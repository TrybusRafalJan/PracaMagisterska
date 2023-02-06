# This function is responsible for geting the data in the format suitable for SVM classifier. 
# \param hist_type - defines the color space for feature extraction.  ['RGB','HSL']
# \param his_eq - defines whether the histograms should be equalized [TRUE,FALSE]
# \param bins_nr - defines the number of histogram bins per channel. [Any int value]
GetDataSVM <- function(ref_datapath, faults_datapath, hist_type = 'RGB', hist_eq = TRUE, data_class_true = 1, data_class_false = 0, bins_number = 20){
  ### Wczytanie zdjęć
  images_list_ref <- load_images(ref_datapath)
  data_ref <- images_to_tab_data(images_list = images_list_ref,h_bins_nr = bins_number, hist_type = hist_type, histEq = hist_eq,data_class = data_class_true)

  images_list_faults <- load_images(faults_datapath)
  data_faults <- images_to_tab_data(images_list = images_list_faults,h_bins_nr = bins_number, hist_type = hist_type, histEq = hist_eq, data_class = data_class_false)
  data <- rbind(data_ref,data_faults)


  data <- data[-1] ### do SVM'a pozbywamy się nazwy zdjęcia
  data_classOnly <- data[length(data)]
  data <- data[-length(data)]
  data <-cbind(data,data_classOnly)

  ### W naszym przypadku nie potrzebne bo klasy od razu są 0 i 1 ale gdyby były opsiowe to tutaj ich kodowanie
  data$class <- factor(data$class, levels = c(0, 1))

  #Przetasowanie danych
  data <- data[sample(1:nrow(data)), ]

  return(data)
}
# This function runs the SVM eperiment. Training the SVM with radial kernel and printing out the classification metrix. 
# \param svm_class - defines the SVM approach. Mult-class or one-class classification   ['cclass','oneclass']
RunSVMTest <- function(data, svm_class = "cclass"){
  switch(svm_class,
         cclass={
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
                            kernel = 'radial')

           # predykcja
           y_pred = predict(classifier, newdata = test_set[-length(test_set)])

           # Making the Confusion Matrix
           cm = table(test_set[, length(test_set)], y_pred)

           cc <- confusionMatrix(cm)
           print(cc)

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

           bins_per_class <- length(trainPositive) - 2
           trainpredictors<-trainPositive[inTrain,1:bins_per_class] # ekstrakcja samych cech
           trainLabels<-trainPositive[inTrain,length(trainPositive)] # ekstrakcja amych etykiet

           testPositive<-trainPositive[-inTrain,]
           testPosNeg<-rbind(testPositive,testnegative)

           testpredictors<-testPosNeg[,1:bins_per_class]
           testLabels<-testPosNeg[,length(trainPositive)]

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
           #print(confTrain)
           #print(confTest)
           cc <- confusionMatrix(confTest,positive='TRUE')
           print(cc)
         },
         {
           print('zly parametr svm_algorithm')
         }
  )
}
