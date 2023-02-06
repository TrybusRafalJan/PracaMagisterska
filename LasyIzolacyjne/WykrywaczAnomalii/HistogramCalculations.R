library(imager)
library(ggplot2)
library(dplyr)
library(plyr)
library(miscTools)

hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))

calculate_rgb_histogram <- function(im,nrOfBins,histEq){
  
  if(isTRUE(histEq)){
    cn <- imsplit(im,"c")
    cn_eq <- map_il(cn,hist.eq)
    im <- imappend(cn_eq,"c")
  }
  
  bdf <- as.data.frame(im)
  #head(bdf,3)
  bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
  
  #print (
  #  ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=nrOfBins)+facet_wrap(~ channel)
  #)
  print("...Policzono histogram RGB")
  
  im_R <- channel(im,1)
  histogram_R <- hist(im_R,main="Histogram R",
                      breaks=seq(min(im_R), max(im_R), length.out = nrOfBins+1))
  
  im_G <- channel(im,2)
  histogram_G <- hist(im_G,main="Histogram G",
                      breaks=seq(min(im_G), max(im_G), length.out = nrOfBins+1))
  
  im_B <- channel(im,3)
  histogram_B <- hist(im_B,main="Histogram B",
                      breaks=seq(min(im_B), max(im_B), length.out = nrOfBins+1))
  
  
  ## KONKATENACJA WARTOŚCI KANAŁÓW R,G,B
  RGB.row <- c(histogram_R$counts, histogram_G$counts,histogram_B$counts)
  
  return(RGB.row)
}

calculate_hsv_histogram <- function(im,nrOfBins,histEq){
  
  if(isTRUE(histEq)){
    cn <- imsplit(im,"c")
    cn_eq <- map_il(cn,hist.eq)
    im <- imappend(cn_eq,"c") %>% plot(main="All channels equalised")
    
  }
  
  ### ZAMIANA RGB NA HSV PRZY UŻYCIU BIBLIOTEKI IMAGER
  im.hsl <- RGBtoHSL(im)
  chan <- channels(im.hsl) ## WYCIĄGNIĘCIE KANAŁÓW
  names(chan) <- c("H","S","L")
  
  #Plot
  layout(matrix(1:3,1,3))
  l_ply(names(chan),function(nm) plot(chan[[nm]],main=nm))
  
  ### OBLICZENIE HISTOGRAMU
  bdf <- as.data.frame(im.hsl)
  #head(bdf,3)
  bdf <- mutate(bdf,channel=factor(cc,labels=c('H','S','L')))
  #print (
  #  ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=nrOfBins)+facet_wrap(~ channel)
  #)
  print("...Policzono histogram HSV")
  
  
  im_H <- channel(im.hsl,1)
  histogram_H <- hist(im_H,main="Histogram H",
                      seq(min(im_H), max(im_H), length.out = nrOfBins+1))
  im_S <- channel(im.hsl,2)
  histogram_S <- hist(im_S,main="Histogram S",
                      seq(min(im_S), max(im_S), length.out = nrOfBins+1))
  im_V <- channel(im.hsl,3)
  histogram_V <- hist(im_V,main="Histogram V",
                      seq(min(im_V), max(im_V), length.out = nrOfBins+1))
  
  ## KONKATENACJA WARTOŚCI KANAŁÓW R,G,B
  HSV.row <- c(histogram_H$counts, histogram_S$counts,histogram_V$counts)
  
  return(HSV.row)
}


