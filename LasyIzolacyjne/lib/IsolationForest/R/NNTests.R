# This function is responsible for geting the data in the format suitable for NN classifier. 
# \param hist_type - defines the color space for feature extraction.  ['RGB','HSL']
# \param his_eq - defines whether the histograms should be equalized [TRUE,FALSE]
# \param bins_nr - defines the number of histogram bins per channel. [Any int value]
GetDataNN <- function(ref_datapath, faults_datapath, hist_type = 'RGB', hist_eq = TRUE, data_class_true = "OK", data_class_false = "NOK", bins_number = 20){
  images_list_ref <- load_images(ref_datapath)
  data_ref <- images_to_tab_data(images_list = images_list_ref,h_bins_nr = bins_number, hist_type = hist_type, histEq = hist_eq, data_class=data_class_true)

  images_list_faults <- load_images(faults_datapath)
  data_faults <- images_to_tab_data(images_list = images_list_faults,h_bins_nr = bins_number, hist_type = hist_type, histEq = hist_eq, data_class=data_class_false)
  data <- rbind(data_ref,data_faults)

  data <- data[-1] ### do SVM'a pozbywamy się nazwy zdjęcia

  ### W naszym przypadku nie potrzebne bo klasy od razu są 0 i 1 ale gdyby były opsiowe to tutaj ich kodowanie
  #data$class <- factor(data$class, levels = c(0, 1))

  #Przetasowanie danych
  data <- data[sample(1:nrow(data)), ]
  return(data)
}

# This function runs the NN eperiment. Training the NN and printing out the classification metrix. 

RunNNTest <- function(data, data_class_true = "OK", data_class_false = "NOK"){
  indexes=createDataPartition(data$class, p=.60, list = F) # indexes for test and train

  train_ = data[indexes, ]
  train_[-length(data)] <- as.data.frame(lapply(train_[-length(data)], as.numeric))

  test_ = data[-indexes, ]
  test_[-length(data)] <- as.data.frame(lapply(test_[-length(data)], as.numeric))

  xtest_ = test_[, -length(data)]
  ytest_ = test_[, length(data)]

  nnet_=neuralnet(class~., train_, hidden = c(length(data),3), linear.output = FALSE)

  ypred_ = neuralnet::compute(nnet_, xtest_)
  yhat_ = ypred_$net.result
  print(yhat_)

  yhat_=data.frame("yhat_"=ifelse(max.col(yhat_[ ,1:2])==1, data_class_false, data_class_true
  ))

  cm=confusionMatrix(as.factor(ytest_), as.factor(yhat_$yhat_))
  print(cm)
}
