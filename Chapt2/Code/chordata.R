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
  write_csv(i, file = paste("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/chordata_",x,".csv", sep=""))
  
}


#Children of Phylum Chordata
#Chordata <- taxize::children(c('Chordata'), db = 'worms')[[1]]

#Chordata<- just_worms_records('Cephalochordata', 'subphylum')
#Chordata<- just_worms_records('Tunicata', 'subphylum')

#Vertebrata <- taxize::children(c('146419'), db = 'worms')[[1]] #vertebrata
#Gnathostomata <- taxize::children(c('1828'), db = 'worms')[[1]] #Gnathostomata

#vertebrata::Agnatha::Cyclostomi (only superclass)
#Chordata<- just_worms_records(c('Cyclostomi'), 'superclass') 
#vertebrata::Gnathostomata::Chondrichthyes
#Chordata<- just_worms_records(c('Chondrichthyes'), 'parvphylum') 
#vertebrata::Gnathostomata::tetropoda
#Chordata<- just_worms_records(c('Tetrapoda'), 'megaclass') 

#vertebrata::Gnathostomata::Osteichthyes
#Osteichthyes <- children(c('Osteichthyes'), db = 'worms')[[1]] 
#vertebrata::Gnathostomata::Osteichthyes::Actinopterygii/Sarcopterygii
#Actinopterygii <- just_worms_records(c('Actinopterygii'), 'Gigaclass')

#Osteichthyes <- children(c('Osteichthyes'), db = 'worms')[[1]] 
Actinopterygii <- just_worms_records(c('Actinopterygii'), 'Gigaclass')

#Let's bring it together into one chordata
Chordata <- as_tibble(list.files('C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data',
                       pattern = "chordata_", full.names = TRUE))
            
Chordata <- bind_rows(apply(Chordata, 1, read_csv)) 

write.csv(Chordata, 'C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/Chordata.csv')


