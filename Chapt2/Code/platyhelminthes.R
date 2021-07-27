library(taxize)
library(worrms)
library(tidyverse)

just_worms_records <- function(x,y) {
  #Pull worms species names
  worms <- do.call(rbind, downstream(x, db = 'worms', downto = 'species', rank = y)) #pulls worms species names out
  
  
  #Finally call the full worms record out
  i <- do.call(rbind, lapply(worms$id, function(i){
    wm_record(i)
  }))
  
  #save in file
  write_csv(i, file = paste("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/Animalia sets/chordata_",x,".csv", sep=""))
  
}


Platyhelminthes<- just_worms_records("Platyhelminthes", 'phylum')