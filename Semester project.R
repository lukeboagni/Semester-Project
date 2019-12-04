#read in libraries

library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)

#list files
a = list.files('fermentation data/',full = T)

#adply
fermply = adply(a, .margins = 1, function(file){
  
  #read the data
  d = read_excel(file,5)
  
  return(d)
}, .inform = T)

#make X1 numberic

fermply$X1 = as.numeric(fermply$X1)

#ddply to change x1 to concentration

fermdd = ddply(.data = fermply, .variables = "X1", function(x){
  
  z = unique(x$X1) 
  
  
  concentration = function(y){
    if(y==1)
      q = 0
    else if(y==2)
      q = .625
    else if(y==3)
      g= 1.25
    else
      q = 2.5
  }
  
  x$concentration = concentration(y = z)
  
  return(x)
  
}, .inform = T, .progress = "test")

#subset data



#plotting graph

ggplot(data = fermdd, aes(x = fermdd$`Time [h]`, y = fermdd$`Lignin [g/l]`)) + geom_line() +
  facet_grid(fermdd$concentration~.) + xlab("Time [h]") +
  ylab("Lignin [g/L]") + ggtitle("Lignin consumption of E. coli") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Relative humidity [%]"))

