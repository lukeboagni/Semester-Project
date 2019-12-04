#read in libraries

library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggmap)
library(osmdata)

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

write.table(fermdd, "ferm.txt", sep="\t")

#subset, index, order, and summarize

sub1 = subset(fermdd,subset = fermdd$concentration > 0)
value = sub1$`Lignin [g/l]`[1]
hightolow = arrange(sub1, -concentration)
sum_sub = sub1 %>% group_by(concentration) %>% summarize(mean = mean(sub1$`Lignin [g/l]`,na.rm = T),
                                                         sd = sd(sub1$`Lignin [g/l]`, na.rm = T))
#histogram
  
gghisto = ggplot(data = fermdd, aes(x = fermdd$`Volume [L]`)) + geom_histogram() + 
  xlab("Volume [L]") + ylab("Distribution")
gghisto
#plotting line graph

ggline = ggplot(data = fermdd, aes(x = fermdd$`Time [h]`, y = fermdd$`Lignin [g/l]`,color = concentration)) 
  + geom_line() +scale_colour_gradientn(colours = terrain.colors(10)) +
  facet_grid(fermdd$concentration~.) + xlab("Time [h]") + 
  ylab("Lignin [g/L]") + ggtitle("Lignin consumption in E. coli") +
  theme_dark()
ggline

#ggmap
potsdam = NA
potsdam = as.data.frame(potsdam)
potsdam$lat = NA
potsdam$lon = NA
potsdam$lat[1] = 52.3 
potsdam$lon[1] = 13.0

GER = c(left = 6, bottom = 48, right = 15, top = 54) 
map1 = get_stamenmap(bbox = GER, zoom = 7, 
maptype = "terrain") %>% ggmap() + geom_point(data = potsdam,aes(x = lon, y = lat))
map1
      
ggsave("potsdam.tiff")



