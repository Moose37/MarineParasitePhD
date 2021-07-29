###########################################

########### Full Animalia list ############

###########################################

# This chunk of code is to bring in the entire Worms Animalia dataset together
# Through various other code in this folder I called in various Phyla of Animalia, I bring it together here
# I then remove a few non-Animalia phyla and clean it for duplicates, non-marine, extinct, and pull out accepted spp

# Finally, I identify and remove all parasites from the dataset so I have parasitic, and non-parasitic sets.

library(taxize)
library(worrms)
library(tidyverse)


#1) I brought in a list of Animalia phylum

Animalia <- children('Animalia', db = 'worms')

#2) Looking at worms I broke it down into each phylum with > 10 000 species
source(c(Arthropoda, Cnidaria, Chordata, Mollusca, Nematoda, Platyhelminthes))
# I ran these as concurrent jobs ...

#The rest I ran through here
source(Animalia.job)


#3) Let's bring them all together

###can I bring this in from Git Hub ???

#This calls all .csv files from the data file
Animalia <- as_tibble(list.files('C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data',
                           pattern = ".csv", full.names = TRUE)) %>%
  filter(!str_detect(value, 'arthropoda_')) %>%
  filter(!str_detect(value, 'chordata_')) %>%
  filter(!str_detect(value, c('FullAnimalia', 'FullParasite'))) 
  

#colnames(Animalia)

#now that we have the files, let's bring in the datasets into one holistic set
Animalia <- bind_rows(apply(Animalia, 1, read_csv)) %>%
  #We have a few Phyla that have snuck in from other kingdoms
  filter(!phylum %in% c("Ciliophora", "Myzozoa", 'Tracheophyta','Euglenozoa', 'Ochrophyta', 'Rhodophyta') | is.na(phylum)) %>%
  #We also have a few repetitions
  distinct()

# here is where we call all extant, accepted marine animalia species
Animalia.final <- Animalia %>%
  #Keep only "accepted"
  filter(status %in% "accepted") %>%
  #dplyr::distinct(valid_name, .keep_all = TRUE) %>%
  #filter(valid_name == scientificname) %>%
  #Remove extinct species
  filter(!(isExtinct == 1) | is.na(isExtinct)) %>%
  #Remove animals that ONLY occur in Terrestrial and Freshwater
  filter(isMarine %in% 1 | isBrackish %in% 1)
  
write_csv(Animalia.final, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/FullAnimalia.csv.gz")

#########################################

#### Lets pull out Parasitic species ####

#########################################

#Above we have downloaded all the accepted, marine, extant species in WoRMS
#Now I want to filter out all the Parasitic species from this dataset.

#We'll do this by major parasitic phyla and then get pretty specific.

### Helminths

Helminths <- bind_rows(
  #Platyhelminths
  filter (Animalia.final, class %in% c('Cestoda', 'Trematoda', 'Monogenea')),
  filter (Animalia.final, order %in% 'Fecampiida'),
  #Annelida
  filter (Animalia.final, order %in% c("Rhynchobdellida", "Arhynchobdellida")),
  #Nematoda
  filter (Animalia.final, phylum %in% 'Nematoda'),
  #Acanthocephala
  filter (Animalia.final, class %in% c("Palaeacanthocephala", "Eoacanthocephala"))
)

### Crustacea

Crustacea <- bind_rows(
  #Amphipoda
  filter (Animalia.final, family %in% c(downstream(1205, downto = 'family', db = 'worms')[[1]]$name, "Cyamidae")),
  #Ascothoracida
  filter (Animalia.final, order %in% c("Dendrogastrida", "Laurida")),
  #Branchiura
  filter (Animalia.final, family %in% "Argulidae"),
  #Copepoda
  filter (Animalia.final, family %in% c("Archinodephyidae", "Ascidicolidae", "Mantridae", "Notodelphyidae")),
  filter (Animalia.final, order %in% c("Monstrilloida", "Siphonostomatoida")),
  #Isopoda
  filter (Animalia.final, family %in% c(downstream("Cymothooidea", downto = 'family', db = 'worms')[[1]]$name)),
  filter (Animalia.final, family %in% c(downstream("Cryptoniscoidea", downto = 'family', db = 'worms')[[1]]$name)),
  filter (Animalia.final, family %in% c(downstream("Bopyroidea", downto = 'family', db = 'worms')[[1]]$name)),
  #Pentastomida
  filter (Animalia.final, order %in% c("Cephalobaenida", "Porocephalida", "Raillietiellida", "Reighardiida" )),
  #Rhizocephala
  filter (Animalia.final, order %in% c("Akentrogonida", "Kentrogonida")),
  #Tantulocardia
  filter (Animalia.final, family %in% c("Basipodellidae", "Cumoniscidae", "Doryphallophoridae", "Microdajidae")),
  #Thoracica
  filter (Animalia.final, genus %in% c('Rhizolepas', 'Anelasma'))
)
  
### Mollusca
  
Mollusca <- filter(Animalia.final, family %in% 
                     c("Eulimidae", "Pyramidellidae", "Ovulidae", "Epitoniidae", "Triviidae",
                            "Architectonicidae", "Cypraeidae", "Colubrariidae", "Cancellariidae", "Marginellidae"))

### Minor Groups

MinorGroups <- bind_rows(
  #Acari
  filter (Animalia.final, order %in% c('Astigmata', 'Prostigmata', 'Mesostigmata', 'Ixodida')),
  #Chordata
  filter (Animalia.final, order %in% 'Petromyzontiformes'),
  filter (Animalia.final, genus %in% c('Encheliophis', 'Simenchelys')),
  #Dicyemida
  filter (Animalia.final, phylum %in% 'Dicyemida'),
  #Insecta
  filter(Animalia.final, order %in% 'Phthiraptera'),
  #Cnidaria
  filter (Animalia.final, class %in% 'Myxozoa'),
  #Orthonectida
  filter (Animalia.final, phylum %in% 'Orthonectida')
)

Parasite.final <- bind_rows(
  Crustacea,
  Helminths, 
  Mollusca,
  MinorGroups
)

write_csv(Parasite.final, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/FullParasite.csv.gz")

