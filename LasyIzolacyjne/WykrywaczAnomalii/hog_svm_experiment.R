library(OpenImageR)

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

data <- hog_data

data$class <- factor(data$class, levels = c(0, 1))

data <- data[sample(1:nrow(data)), ]


# SVM
#install.packages('e1071')
library(e1071)
library(caret)

#svm_algorithm <- "cclass"
svm_algorithm <- "oneclass"

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
         
         
         
         confusionMatrix(confTest,positive='TRUE')
         
         print(confTrain)
         print(confTest)
         
         
         
         
         
         
       },
       {
         print('zly parametr svm_algorithm')
       }
)



