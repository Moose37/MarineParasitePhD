###########################################

########### Full Animalia list ############

###########################################

library(taxize)
library(worrms)
library(tidyverse)

# this chunk of code is to bring in the entire Worms Animalia dataset
# to do this, we needed to figure some stuff out...
#1) I brought in a list of Animalia phylum

Animalia <- children('Animalia', db = 'worms')

#2) Looking at worms I broke it down into each phylum with > 10 000 species
source(c(Arthropoda, Cnidaria, Chordata, Mollusca, Platyhelminthes))
# I ran these as concurrent jobs ...

#The rest I ran through here
source(Animalia.job)


#3) Let's bring them all together
Animalia.check <- children('Animalia', db = 'worms')[[1]]
Animalia <- as_tibble(list.files('C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Animalia sets',
                         pattern = ".csv", full.names = TRUE)) %>%
  filter(!str_detect(value, 'arthropoda_')) %>%
  filter(!str_detect(value, 'chordata_'))

Animalia <- bind_rows(apply(Animalia, 1, read_csv)) 

Animalia.final <- Animalia %>%
  filter(status %in% 'accepted')

write_csv(Animalia.final, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/WormsSpeciesList4.0.csv")
