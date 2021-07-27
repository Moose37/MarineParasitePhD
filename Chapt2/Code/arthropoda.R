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
  write_csv(i, file = paste("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/Animalia sets/arthropoda_",x,".csv", sep=""))
  
}

#Arthropoda<- just_worms_records(c('Arthropoda'), 'phylum')

#Arthropoda <- taxize::children(c('Arthropoda'), db = 'worms')[[1]]

#Chelicerata <- just_worms_records('Chelicerata', 'subphylum')
#Hexapoda <- just_worms_records('Hexapoda', 'subphylum')
#Myriapoda <- just_worms_records('Myriapoda', 'subphylum')

#Arthropoda::Crustacea
#Crustacea <- taxize::children(c('Crustacea'), db = 'worms')[[1]]
#Branchiopoda <- just_worms_records('Branchiopoda', 'subphylum')
#Cephalocarida <- just_worms_records('Cephalocarida', 'subphylum')
#Oligostraca <- just_worms_records('Oligostraca', 'subphylum')
#Remipedia <- just_worms_records('Remipedia', 'subphylum')

#Arthropoda::Crustacea::Multicrustacea
#Multicrustacea <- just_worms_records('Multicrustacea', 'subphylum')
#Multicrustacea <- taxize::children(c('Multicrustacea'), db = 'worms')[[1]]
#Thecostraca <- just_worms_records('Thecostraca', 'class')
#Hexanauplia <- just_worms_records('Hexanauplia', 'class')

#Arthropoda::Crustacea::Multicrustacea::Malacostraca
#Malacostraca <- taxize::children(c('Malacostraca'), db = 'worms')[[1]]
Eumalacostraca <- just_worms_records('Eumalacostraca', 'subclass')
Hoplocarida <- just_worms_records('Hoplocarida', 'subclass')
Phyllocarida <- just_worms_records('Phyllocarida', 'subclass')

#Let's bring it all together
Arthropoda <- as_tibble(list.files('C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Animalia sets',
                                 pattern = "arthropoda_", full.names = TRUE))

Arthropoda <- bind_rows(apply(Arthropoda, 1, read_csv)) 

write.csv(Arthropoda, 'C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Animalia sets/Arthropoda.csv')
