### IMPORT SKRYPTU Z FUNKCJAMI LICZĄCYMI HISTOGRAMY
if(!exists("fun", mode="function")) source("HistogramCalculations.R")

#Przycięcie histogramu dla zdefiniowanych okien
cropHistogramRange <- function(histogram,ch1Range,ch2Range,ch3Range){
  
  ch1_start_val <- ch1Range[1]
  ch1_stop_val <- ch1Range[2]
  
  ch2_start_val <- ch2Range[1]
  ch2_stop_val <- ch2Range[2]
  
  ch3_start_val <- ch3Range[1]
  ch3_stop_val <- ch3Range[2]
  
  nrOfBins <- ncol(histogram) - 1
  
  if(ch1_start_val != ch1_stop_val){
    start_bin_ch1 <- round(nrOfBins/3 * ch1_start_val / 255)
    stop_bin_ch1 <- round(nrOfBins/3 * ch1_stop_val / 255)-1
  }else{
    start_bin_ch1 <- 0
    stop_bin_ch1 <- 0
  }
  
  if(ch2_start_val != ch2_stop_val){
    start_bin_ch2 <- 1 * round(nrOfBins / 3) + round(nrOfBins/3 * ch2_start_val / 255)
    stop_bin_ch2 <- 1 * round(nrOfBins / 3) + round(nrOfBins/3 * ch2_stop_val / 255)-1
  }else{
    start_bin_ch2 <- 0
    stop_bin_ch2 <- 0
  }
  
  if(ch3_start_val != ch3_stop_val){
    start_bin_ch3 <- 2 * round(nrOfBins / 3) + round(nrOfBins/3 * ch3_start_val / 255)
    stop_bin_ch3 <- 2 * round(nrOfBins / 3) + round(nrOfBins/3 * ch3_stop_val / 255)-1
  }else{
    start_bin_ch3 <- 0
    stop_bin_ch3 <- 0
  }
  
  
  print(paste('Zakres na kanale 1:   ', start_bin_ch1,'-',stop_bin_ch1, ' '))
  print(paste('Zakres na kanale 2:   ', start_bin_ch2,'-',stop_bin_ch2, ' '))
  print(paste('Zakres na kanale 3:   ', start_bin_ch3,'-',stop_bin_ch3, ' '))
  
  histogram <- histogram[-1]
  
  if(start_bin_ch1 != stop_bin_ch1){
    histogram_ch1 <- subset(histogram, select = seq(start_bin_ch1,stop_bin_ch1,1))
  }else{
    histogram_ch1 <- histogram[0]
  }
  if(start_bin_ch2 != stop_bin_ch2){
    histogram_ch2 <- subset(histogram, select = seq(start_bin_ch2,stop_bin_ch2,1))
  }else{
    histogram_ch2 <- histogram[0]
  }
  if(start_bin_ch3 != stop_bin_ch3){
    histogram_ch3 <- subset(histogram, select = seq(start_bin_ch3,stop_bin_ch3,1))
  }else{
    histogram_ch3 <- histogram[0]
  }
  
  histogram <- cbind(histogram_ch1,histogram_ch2,histogram_ch3)
  
  return(histogram)
  
  
}

## Wczytuje pliki z katalogu podanego jako paramter
load_images <- function(path_to_dataset){
  
  images <- list.files(path_to_dataset, pattern="*.jpg", full.names = T)
  
  print(images)
  print(sprintf("Zaladowano %i zdjec z: %s",length(images), path_to_dataset))
  
  return(images)
}


images_to_tab_data <- function(images_list,h_bins_nr,hist_type, histEq, data_class){
  
  switch(hist_type, 
         RGB={
           
           header_R <- NULL
           header_G <- NULL
           header_B <- NULL
           
           for(i in 1:h_bins_nr){
             header_R <- c(header_R,paste("R", i, sep="_"))
             header_G <- c(header_G,paste("G",i,sep="_"))
             header_B <- c(header_B,paste("B",i,sep="_"))
           }
           
           header <- c("img_path",header_R,header_G,header_B,"class")
           dataTable <- data.frame(setNames(rep(list(0), length(header)), header))
           
           
           i <- 0
           
           for (image in images_list) {
             
             i <- i + 1
             im <- load.image(image)
             row <- calculate_rgb_histogram(im,h_bins_nr,histEq)
             row <- append(row,data_class)
             row <- c(image,row)
             
             rbind(dataTable, i = row) -> dataTable
           }
           
           
           dataTable<-dataTable[!(dataTable$R_1==0),]
           
           
           
           
         },
         HSL={
           
           
           header_H <- NULL
           header_S <- NULL
           header_V <- NULL
           
           for(i in 1:h_bins_nr){
             header_H <- c(header_H,paste("H", i, sep="_"))
             header_S <- c(header_S,paste("S",i,sep="_"))
             header_V <- c(header_V,paste("V",i,sep="_"))
           }
           
           header <- c("img_path",header_H,header_S,header_V,"class")
           dataTable <- data.frame(setNames(rep(list(0), length(header)), header))
           
           
           i <- 0
           
           for (image in images_list) {
             
             i <- i + 1
             im <- load.image(image)
             row <- calculate_hsv_histogram(im,h_bins_nr, histEq)
             row <- append(row,data_class)
             row <- c(image,row)
             
             rbind(dataTable, i = row) -> dataTable
           }
           
           
           dataTable<-dataTable[!(dataTable$H_1==0),]
           .
           
            
         },
         {
           
           
           
           
         }
  )
  
  
  
  
  return(dataTable)
}

## Podgląd zdjęcia i histogramu dla określonego typu (hist_type) i liczby bins (h_bins_nr)
inspect_image <- function(images_list,im_idx, h_bins_nr,hist_type){
  
  print(sprintf("Inspekca zdjecia nr. : %i - %s  ",im_idx, images_list[im_idx]))
  im <- load.image(images_list[im_idx])
  plot(im)
  
  switch(hist_type, 
         RGB={
           RGB.row <- calculate_rgb_histogram(im,h_bins_nr)
         },
         HSL={
           .
           RGB.row <- calculate_hsv_histogram(im,h_bins_nr)  
         },
         {
           print('zly parametr hist_type')
         }
  )
}