library(taxize)
library(worrms)
library(tidyverse)

#worms_records(clade_name, clade_level, GBIF_level)
#e.g.: worms_records("Rhizocephala", "superorder", "order")

worms_records <- function(x,y,z) {
  #First we need to download both GBIF and WoRMS ID's for the species. 
  #We can evaluate WoRMS taxonomy to match GBIF and then go from there
  #This is an if else statement to either give me the matched taxon names, or find ones that work with GBIF
  gbif <- if(z == y) {x}
  else if(z != y) {name <- do.call(rbind, downstream(x, db = 'worms', downto = z, rank = y))
  name$name}
  #Use evaluated taxa names
  gbif <- do.call(rbind, downstream(gbif, db = 'gbif', downto = 'species', rank = y)) # pulls GBIF species out
  wm_id <- get_wormsid(gbif$name, searchtype = "scientific", 
                       marine_only = FALSE, accepted = FALSE, messages = FALSE, ask = FALSE) # Pulls wormsid per gbif species
  wm_id <- as.numeric(wm_id)
  worms <- do.call(rbind, downstream(x, db = 'worms', downto = 'species', rank = y)) #pulls worms species names out
  
  #Join GBIF and WoRMS together and remove any NA's from the joined dataset
  
  gbif <- cbind(gbif, wm_id) %>%
    select(id = wm_id, name, rank, key) %>%
    filter(!is.na(id))
  join <- full_join(worms,gbif, by = c('name', 'rank', 'id')) 
  
  #Finally call the full worms record out
  i <- do.call(rbind, lapply(join$id, function(i){
    wm_record(i)
    }))
  
  #save in file
  write_csv(i, file = paste("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/",x,".csv", sep=""))

}

just_worms_records <- function(x,y) {
  #Pull worms species names
  worms <- do.call(rbind, downstream(x, db = 'worms', downto = 'species', rank = y)) #pulls worms species names out
  
  
  #Finally call the full worms record out
  i <- do.call(rbind, lapply(worms$id, function(i){
    wm_record(i)
  }))
  
  #save in file
  write_csv(i, file = paste("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/",x,".csv", sep=""))
  
}

#Acanthocephala <- worms_records("Acanthocephala", 'phylum', 'phylum')
#Annelida <- worms_records("Annelida", 'phylum', 'phylum')
#Brachiopoda<- worms_records("Brachiopoda", 'phylum', 'phylum') 
#Bryozoa<- worms_records("Bryozoa", 'phylum', 'phylum')
#Cephalorhyncha<- worms_records("Cephalorhyncha", 'phylum', 'phylum')
#Chaetognatha<- worms_records("Chaetognatha", 'phylum', 'phylum')
####Chordata<- worms_records("Chordata", 'phylum', 'phylum')
####Cnidaria<- worms_records("Cnidaria", 'phylum', 'phylum')
#Ctenophora<- worms_records("Ctenophora", 'phylum', 'phylum') #needs to just go through worms
#Cycliophora<- just_worms_records("Cycliophora", 'phylum')
#Dicyemida<- worms_records("Dicyemida", 'phylum', 'phylum')
#Echinodermata<- worms_records("Echinodermata", 'phylum', 'phylum')
#Entoprocta<- just_worms_records("Entoprocta", 'phylum')
#Gastrotricha<- worms_records("Gastrotricha", 'phylum', 'phylum')
#Gnathifera<- worms_records("Gnathifera", 'phylum', 'phylum')
#Gnathostomulida<- worms_records("Gnathostomulida", 'phylum', 'phylum')
#Hemichordata<- worms_records("Hemichordata", 'phylum', 'phylum')
#Kinorhyncha<- just_worms_records("Kinorhyncha", 'phylum')
#Loricifera<- just_worms_records("Kinorhyncha", 'phylum')
####Mollusca<- worms_records("Mollusca", 'phylum', 'phylum') #This is too big and has dead ends ... see below
####Nematoda<- worms_records("Nematoda", 'phylum', 'phylum')
#Nematomorpha<- worms_records("Nematomorpha", 'phylum', 'phylum')
#Nemertea<- worms_records("Nemertea", 'phylum', 'phylum')
#Orthonectida<- worms_records("Orthonectida", 'phylum', 'phylum')
#Phoronida<- worms_records("Phoronida", 'phylum', 'phylum')
#Placozoa<- worms_records("Placozoa", 'phylum', 'phylum')
####Platyhelminthes<- worms_records("Platyhelminthes", 'phylum', 'phylum')
#Porifera<- worms_records("Porifera", 'phylum', 'phylum')
#Rotifera<- worms_records("Rotifera", 'phylum', 'phylum')
#Sipuncula<- worms_records("Sipuncula", 'phylum', 'phylum')
#Tardigrada<- worms_records("Tardigrada", 'phylum', 'phylum')
#Xenacoelomorpha<- worms_records("Xenacoelomorpha", 'phylum', 'phylum')

