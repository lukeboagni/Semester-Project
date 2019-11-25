#read in libraries

library(tidyverse)
library(plyr)
library(readxl)

#list files
a = list.files('fermentation data/',full = T)

#adply
fermply = adply(a, .margins = 1, function(file){
  
  #read the data
  d = read_excel(file,5)
  
  return(d)
}, .inform = T)

#ddply to change x1 to concentration