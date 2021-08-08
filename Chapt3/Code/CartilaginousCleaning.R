#############################################################
################## Bring it all together ####################
#############################################################
#1) Pull data from LAWRENCE R. PENNER PARASITOLOGY COLLECTION, Shark References, Global Cestode Database, [london]
#2) Run this set through worms to get a greater pull of Hosts (Outside Catilagenous fish)
#3) There are a few very specific issues that need to be cleaned within the dataset
#4) 

#Loading the rvest package
library('rvest')
library('tidyverse')
library('worrms')
library('taxize')
rm(list = ls())


#############################################################
#SharkWoRMS. csv = LRP parasite collection webscrape + worms attributes
#SharkRefscrape.csv = Shark References webscrape NB!!! this must stay as is ... so lets create something new
#webscrapeCestodeDB.csv = Global Cestode database
#
################################################################
######################################################
#Lets bring all these things together...
LRP <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkLRP.csv")

SharkRefscrape <- read_csv("C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkRefscrape.csv")

Cest <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/webscrapeCestodeDB.csv")

FinalSet <- union(LRP, SharkRefscrape, Cest) %>%
  union(., Cest)

#######################################################
#Let's pull out WoRMS attributes
# Collection continued. Let's see what WoRMS holds...
# Pull attributes off WoRMS for parasites. See if we can increase Host/Parasite list.
Psite <- select(FinalSet, Parasite) %>%
  distinct()
Psite <- Psite$Parasite

attributes <- wm_attr_data_(id = as.numeric(get_wormsid(Psite)), searchtype = "scientific", accepted = TRUE, ask = FALSE)
att<-do.call(rbind, attributes$children)
atta<- filter(att, measurementTypeID %in% '58')
attb<-do.call(rbind, att$children)
attb<- filter(attb, measurementTypeID %in% '58')
attcomb<-rbind(atta,attb)
aphialist <- cbind.data.frame(Host = attcomb$measurementValue, Parasite = attcomb$AphiaID)

#housekeeping
#rm(attributes, att, atta, attb, attcomb)

#I have a list of Host AphiaID's and Parasite AphiaID's, Now to populate the dataframe.
Host <- as.numeric(as.character(aphialist$Host))
Psite <- as.numeric(as.character(aphialist$Parasite))

Host <- lapply(Host, function(i) {
  wm_record(i)
})

Host <- do.call(rbind, Host)
Hosttest <- Host %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearH = `parse_number(valid_authority)`) %>%
  select(c(AphiaIDH = 'valid_AphiaID', Host = 'valid_name', authorityH = "valid_authority", 'YearH', phylumH = 'phylum', classH = 'class', orderH = 'order', familyH = 'family', genusH = 'genus'))

Psite <- lapply(Psite, function(i) {
  wm_record(i)
})

Psite <- do.call(rbind, Psite)
Psitetest <- Psite %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearP = `parse_number(valid_authority)`) %>%
  select(c(AphiaIDP = 'valid_AphiaID', Parasite = 'valid_name',authorityP = "valid_authority", 'YearP', phylumP = 'phylum', classP = 'class', orderP = 'order', familyP = 'family', genusP = 'genus'))

SharkWorms <- bind_cols(Hosttest, Psitetest)

#There we go. This is the hosts according to worms of parasites called out from Uconn and SharkRefs
write_csv(SharkWorms, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkWoRMS.csv")
rm(list = ls(all.names = TRUE))

#################################################################
#Lets see What this dataset is composed of and clean out any specific issues 

SharkWoRMS <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkWoRMS.csv")%>%
  select(.,"Host","YearH",'phylumH','classH','orderH',
         'familyH','genusH',"Parasite","YearP",'phylumP',
         'classP','orderP','familyP', 'genusP')

#bring in all the datasets and see if there are differences
SharkRef <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkFull3.0.csv") %>%
  select(.,"Host","YearH",phylumH = "phylumH",classH = "classH",orderH = "orderH",
         familyH = "familyH",genusH = "genusH","Parasite","YearP",phylumP = "phylumP",
         classP = "classP",orderP = "orderP",familyP = "familyP", genusP = "genusP")

LRP <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkLRP.csv")
SharkRefscrape <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkRefscrape.csv")
Cest <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/webscrapeCestodeDB.csv")
FinalSet <- union(LRP, SharkRefscrape, Cest, SharkWoRMS)


#Bring together old and new
Finaladjusted <- union(SharkRef, FinalSet)

#Remove ROws with NA Hosts/Species
Finaladjusted <-   filter(Finaladjusted, !is.na(Host))
Finaladjusted <-   filter(Finaladjusted, !is.na(Parasite))



#FINALLY, there are both parasites and hosts with variable genus names which creates repetition in set ... 
#Lets see what they are ... and fix it below
see <- dplyr::select(Finaladjusted, Host, genusH, Parasite, genusP) %>%
  dplyr::count(., Host, Parasite) %>%
  arrange(desc(n))

#am I able to remove duplicate records. i.e: we only want each interaction once
Finaladjusted <- unique(Finaladjusted)

###So, I'll just pull out the genus value in the species name, and make that the new genus column ... and remove repetitions
Finaladjusted$genusH <- str_extract(Finaladjusted$Host, '[[:alpha:]]+')
Finaladjusted$genusP <- str_extract(Finaladjusted$Parasite, '[[:alpha:]]+')


#There are issues with the taxonomy of some species. This is finescale direct work. 
Finaladjusted$classP [Finaladjusted$classP %in% "Copepoda"] <- "Hexanauplia"
Finaladjusted$phylumP [Finaladjusted$classP %in% "Myxosporea" & Finaladjusted$phylumP %in%'Myxozoa'] <- "Cnidaria"
Finaladjusted$classP [Finaladjusted$classP %in% "Myxosporea" & Finaladjusted$phylumP %in%'Cnidaria'] <- "Myxozoa"
Finaladjusted$YearP [Finaladjusted$Parasite %in% "Tetragonocephalum aetobatidis"] <- "1906"
Finaladjusted$familyH [Finaladjusted$genusH %in% c("Bathyraja", "Atlantoraja")] <- "Arhynchobatidae"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Urobatis"] <- "Urotrygonidae"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Aptychotrema"] <- "Rhinobatidae"
Finaladjusted$familyP [Finaladjusted$Parasite %in% "Anthobothrium quadribothria"] <- "Tetraphyllidea incertae sedis"	
Finaladjusted$Host [Finaladjusted$Host %in% "Rhinobatos thouin"]	<- "Glaucostegus thouin"
Finaladjusted$genusH [Finaladjusted$Host %in% "Glaucostegus thouin"]	<- "Glaucostegus"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Glaucostegus"] <- "Glaucostegidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Glaucostegus"] <- "Rhinopristiformes"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Galeorhinus"] <- "Triakidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Galeorhinus"] <- "Carcharhiniformes"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Mustelus"] <- "Triakidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Mustelus"] <- "Carcharhiniformes"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Carcharodon"] <- "Lamniformes"	
Finaladjusted$familyH [Finaladjusted$genusH %in% "Carcharodon"] <- "Lamnidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% c("Carcharhinus", "Prionace", "Rhizoprionodon")] <- "Carcharhiniformes"	
Finaladjusted$familyH [Finaladjusted$genusH %in% c("Carcharhinus", "Prionace", "Rhizoprionodon")] <- "Carcharhinidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Dasyatis"] <- "Myliobatiformes"	
Finaladjusted$familyH [Finaladjusted$genusH %in% c("Dasyatis", "Styracura")] <- "Dasyatidae"
Finaladjusted$orderH [Finaladjusted$genusH %in% "Narcine"] <- "Torpediniformes"	
Finaladjusted$familyH [Finaladjusted$genusH %in% "Narcine"] <- "Narcinidae"
Finaladjusted$orderP [Finaladjusted$genusP %in% "Scalithrium"] <- "Rhinebothriidea"	
Finaladjusted$familyP [Finaladjusted$genusP %in% "Scalithrium"] <- "Rhinebothriidae"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Hyperoglyphe"] <- "Centrolophidae" 
Finaladjusted$familyH [Finaladjusted$genusH %in% "Lutjanus"] <- "Lutjanidae"
Finaladjusted$familyH [Finaladjusted$genusH %in% "Squalus"] <- "Squalidae"

#There are parasites in the Host column ... 
Finaladjusted <- filter(Finaladjusted, !phylumH %in% "Platyhelminthes") %>%
  filter(., !Parasite %in% "Coelorinchus caelorhincus") %>%
  filter(., !Parasite %in% "Christianella trygonbrucco") %>%
  filter(., !Parasite %in% "Dibothriorhynchus maccallumi") %>%
  filter(., !Parasite %in% "Eimeria kayarensis") 

#There are hosts with just genus names as their Host species name ... lets remove them
Finaladjusted <- mutate(Finaladjusted, test = str_detect(Finaladjusted$Host, "[[:blank:]]")) %>%
  filter(., test %in% TRUE)

#Adding some specific rows that I think need to be in there :) 
x <- c('Simenchelys parasitica',  'Chordata', 'Actinopterygii', 'Anguilliformes', 'Synaphobranchidae', 'Simenchelys', '1879',
       'Hypertrema ambovatum', 'Platyhelminthes', 'Trematoda', 'Plagiorchiida', 'Fellodistomidae', 'Hypertrema','1960')

Finaladjusted <- rbind(Finaladjusted, x)
rm(x)

# Final distinct (...to remove repetition)
Finaladjusted <- distinct(Finaladjusted)
Finaladjusted <- Finaladjusted %>% distinct(Host, Parasite, .keep_all = TRUE) %>%
  select(-test)

write_csv(Finaladjusted, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkFull3.0.csv")

################################################################
# Let's give this set some functional and other grouping variables ...
FinalSet <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkFull3.0.csv") %>%
  filter(!phylumP %in% c('Ciliophora', 'Euglenozoa', 'Microsporidia', 'Myzozoa'))

#Parasite discovery by taxon and functional groups dataset:
#first split them into groups

dplyr::count(FinalSet, phylumP, classP)

#Taxon/functional (phylum) Group
FinalSet <- FinalSet %>%
  mutate(group = fct_collapse(phylumP,
                              Helminths = c('Platyhelminthes', 'Acanthocephala', 'Annelida', 'Nematoda'),
                              Arthropoda = c('Arthropoda'),
                              MinorGroups = c('Chordata','Cnidaria')
  ))

#Simple/ complex (class) Lifestyle
FinalSet <- FinalSet %>%
  mutate(lifestyle = fct_collapse(classP,
                                  direct = c('Clitellata', 'Hexanauplia', 'Malacostraca', 'Ostracoda', 'Monogenea', 
                                             'Petromyzonti', 'Actinopterygii', 'Adenophorea', 'Chromadorea', 'Enoplea',
                                             'Secernentea', 'Thecostraca'),
                                  indirect = c('Palaeacanthocephala', 'Cestoda', 'Trematoda', 'Myxozoa')
  ))

#endo/ecto (class) Habitat
FinalSet <- FinalSet %>%
  mutate(habitat = fct_collapse(classP,
                                endo = c('Palaeacanthocephala', 'Cestoda', 'Adenophorea', 'Chromadorea', 'Enoplea', 
                                         'Trematoda', 'Petromyzonti', 'Actinopterygii', 'Myxozoa', 'Secernentea', 'Thecostraca'),
                                ecto = c('Clitellata', 'Hexanauplia', 'Malacostraca', 
                                         'Monogenea', 'Ostracoda', 'Petromyzonti')
                                
  ))


write_csv(FinalSet, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkFull3.0.csv")

###How about some host variables
################################################################
library(rfishbase)
# Let's give this set some functional and other grouping variables ...
#source()...download full worms sharklist.
#################################################
#This is something that needs to be done....
##################################################
#sharkvars <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/FullSharklist.csv")

#this is the downloaded chondrichthyses dataset off worms...
sharkvars <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/Animalia sets/chordata_Chondrichthyes.csv")

#Lets see what pops out.
#FishBase works off a bunch of relational databases. 
#Therefore, you need to call the values you want from the database and bring it together

#fields<-list_fields(server = sealifebase)

#callor<-diet("Rhincodon typus")

#Now that we have identified what we want... Lets run it for all shark species.
sharkvars<- validate_names(sharkvars, server = 'fishbase')
sharkvars<- species(sharkvars, fields = c("Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", 
                                    "Length", "Weight", "Vulnerability"), server = 'fishbase')
sharkdiet<-diet(sharkvars$Species) %>%
  dplyr::select(Species, Troph, seTroph) %>%
  group_by(Species) %>%
  summarise(Trophic = mean(Troph), seTroph = mean(seTroph))

sharkvars <- left_join(sharkvars, sharkdiet, by = ("Species"))

#sharkst<- stocks(shark, fields = c("Species", "IUCN_Code","IUCN_DateAssessed","Protected"), server = 'fishbase')
#shark<- full_join(sharksp, sharkst, by = "Species")

#...and for all species that aren't fish...
FinalSet <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkFull3.0.csv") %>%
  dplyr::count(Host)

nonshark<- validate_names(FinalSet$Host, server = 'sealifebase')
nonshark<- species(nonshark, fields = c("Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", 
                                          "Length", "Weight", "Vulnerability"), server = 'sealifebase')

nonsharkdiet<-diet(nonshark$Species) %>%
  dplyr::select(Species, Troph, seTroph) %>%
  group_by(Species) %>%
  summarise(Trophic = mean(Troph), seTroph = mean(seTroph))

nonshark <- left_join(nonshark, nonsharkdiet, by = ("Species"))

#nonsharkst<- stocks(nonshark, fields = c("Species", "IUCN_Code","IUCN_DateAssessed","Protected"), server = 'sealifebase')
#nonshark<- full_join(nonsharksp, nonsharkst, by = "Species") %>%
#  distinct()

#bring it back together
fullset <- bind_rows(sharkvars, nonshark) %>%
  distinct()
#there are a few specific issues
fullset$IUCN_Code [fullset$Species %in% "Pristis microdon" & fullset$Protected %in% 1] <- "CR"
fullset$IUCN_DateAssessed [fullset$Species %in% "Pristis microdon" & fullset$Protected %in% 1] <- "2013-03-01" 
fullset <- filter(fullset, !(IUCN_Code == "N.A."))

#There are duplicate records ... mainly surrounding IUCN assessments. 
#Need to pull them out of the original set, clear the duplicates, and bring them back together
dups <- dplyr::count(fullset, Species) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  left_join(., fullset, by = 'Species') %>%
  filter(., !(IUCN_Code == "EN" & Species == "Balaenoptera musculus")) %>%
  filter(., !(IUCN_Code == "EX" & Species == "Acipenser nudiventris")) %>%
  filter(., !((str_detect(IUCN_DateAssessed, "^2014") & Species == "Arctocephalus pusillus"))) %>%
  filter(., !(IUCN_Code == "EX" & Species == "Huso huso")) %>%
  filter(., !(IUCN_Code == "N.A."))%>%
  filter(!(is.na(IUCN_DateAssessed))) %>%
  select(-n)

#remove all duplicate species and bring in the cleaned duplicate records.
fullset <- filter(fullset, !(Species %in% dups$Species))%>%
  union(., dups)

#Lets keep this info
write_csv(fullset, "C:/Users/Mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkVars.csv")


play <- filter(fullset, is.na(Length))
