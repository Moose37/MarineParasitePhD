#############################################################
##########Bring it all together with web scraping############
#############################################################
#1) Pull data from http://lrpennerdb.uconn.edu/index.php/parasites/specimen_results/1
#1) Pull data from http://www.tapewormdb.uconn.edu/index.php/parasites/species_results/1
#2) Pull all of Chondrichthys parasite attributes from WoRMS
#3) Pull all of them together and bring in a bunch of attributes

#Loading the rvest package
library('rvest')
library('tidyverse')
library('worrms')
library('taxize')
rm(list = ls())


#############################################################
#1) UConn dataset
#Specifying the url for desired website to be scraped
url <- 1:163 
url <- paste0("http://lrpennerdb.uconn.edu/index.php/parasites/specimen_results/",url,"") #creates a character string of all the website possibilities

url <- 1:250
url <-paste0("http://www.tapewormdb.uconn.edu/index.php/parasites/species_results/",url,"")

#We have it working for one page, can we make it run for all of them...
table_data <- lapply(url, function(i) {
  webpage <- read_html(i)
  table_html <- html_nodes(webpage, 'table')
  table_data <- html_table(table_html, fill = TRUE)
  table_data[[1]]
})

table_data <- do.call(rbind, table_data)

############################################################
##### DON'T RUN IF BELOW WORKS #####
#Lets work the single

#url <- "http://lrpennerdb.uconn.edu/index.php/parasites/specimen_results/1"
#webpage <- read_html(url)
#table_html <- html_nodes(webpage, 'table')
#table_data <- html_table(table_html, fill = TRUE)
#table_data <- table_data[[1]]

############################################################
#Lets clean this up
#There are 2 Uconn datasets: one off the website (new) and one downloaded in July 2019 (old)
#I want to see if they are similar/different in anyway.

# Lets clean up the first (New) table 
UConnNew <- table_data %>%
  select(c('ParasiteGenus', 'ParasiteSpecies', "Host Genus", "Host Species")) %>%
  filter(!is.na(ParasiteSpecies)) %>%
  unite(., 'ParasiteGenus', 'ParasiteSpecies', col = 'Parasite', sep = " ")%>%
  unite("Host Genus", "Host Species", col = 'Host', sep = " ") %>%
  distinct()

#Lets split parasites and hosts, run a name resolver and then bring back together.
Psite <- select(UConnNew, Parasite) %>% distinct()
Psite <- sapply(Psite$Parasite, function(i) {
  gnr_resolve(i, data_source_ids = c(1,9,11), canonical = TRUE, http = "post")
})

Psite <- do.call(rbind, Psite)
Psite <- select(Psite, -data_source_title) %>%
  distinct()

Host <- select(UConnNew, Host) %>% distinct()
Host <- sapply(Host$Host, function(i) {
  gnr_resolve(i, data_source_ids = c(1,9,11), canonical = TRUE, http = "post")
})

Host <- do.call(rbind, Host)
Host <- select(Host, -data_source_title) %>%
  distinct()

#now bring it back together
UConnNew <- left_join(UConnNew, Host, by = c('Host'='submitted_name')) %>% 
  select(Parasite, Host = 'matched_name2') %>%
  left_join(., Psite, by = c("Parasite"="submitted_name")) %>% 
  select(Host, Parasite = 'matched_name2') %>%
  filter(!is.na(Parasite)) %>%
  filter(!is.na(Host)) %>%
  distinct()

# There is an issue in the new dataset where there are only Genus names in both the host and parasite columns. 
#Let's remove them.
UConnNew <- separate(UConnNew, Host, into = c('Hx', 'Hy')) %>%
  separate(., Parasite, into = c('Px', 'Py')) %>%
  filter(!is.na(Py)) %>%
  filter(!is.na(Hy)) %>%
  unite(., Host, 'Hx', 'Hy', sep = " ") %>%
  unite(., Parasite, 'Px', 'Py', sep = " ")

###########################################################
# Now lets clean the older dataset
UConnOld <- read_csv("C:/Users/Mooseface/Google Drive/Publications/Mark idea's/Mark data/tapewormspecies.csv") %>%
  select(c('Genus', 'Species', 'Taxonomic Status', 'Valid Name', 'Type Host(valid) Genus', 'Type Host(valid) Species', 'Additional Hosts')) %>%
  filter(!is.na(Species)) %>%
  #filter(is.na(Taxonomic Status)| Taxonomic Status) %>%
  unite(., 'Genus', 'Species', col = 'Parasite', sep = " ")%>%
  unite(.,'Type Host(valid) Genus', 'Type Host(valid) Species', col = 'Host', sep = " ") %>%
  unite(.,'Host', 'Additional Hosts', col = 'Host', sep = ",")

#There is a column of "Addtional Hosts". I want to pull those out and add them onto the bottom wih corresponding parasites.
UConnOld <- separate_rows(UConnOld, 'Host', sep = '[[:punct:]]') %>%
  filter(Host != "NA NA") %>%
  filter(Host != "NA") %>%
  filter(Host != "") %>%
  filter(Host != "none") %>%
  mutate_at(vars(Host), funs(gsub("[[:punct:]]", '', Host))) %>%
  select('Parasite', 'Host')

# There is an issue in the old dataset where there are only Genus names in both the host and parasite columns. 
#Let's remove them.
UConnOld <- separate(UConnOld, Host, into = c('Hx', 'Hy')) %>%
  separate(., Parasite, into = c('Px', 'Py')) %>%
  filter(!is.na(Py)) %>%
  filter(!is.na(Hy)) %>%
  unite(., Host, 'Hx', 'Hy', sep = " ") %>%
  unite(., Parasite, 'Px', 'Py', sep = " ")

#Lets bring together the old and the new...
UConn <- union(UConnNew, UConnOld) %>%
  mutate_all(.,funs(gsub("[^[:alnum:]]", " ", .)))

write_csv(Uconn, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkUConn.csv")
######################################################
SharkRef <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/webscrapeSharkRef.csv") %>%
  select(Host = "host", Parasite = "parasite")

#Now we have both UConn (old and new) and SharkRef (webscrape)
#Bring it together into one holistic dataset

FinalSet <- dplyr::union(SharkRef, UConn)

######################################################
#Let's pull out WoRMS attributes
# Collection continued. Let's see what WoRMS holds...
# SharkRef<-read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/FullSharklist.csv") %>%
  
  
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
rm(attributes, att, atta, attb, attcomb)

#I have a list of Host AphiaID's and Parasite AphiaID's, Now to populate the dataframe.
Host <- as.numeric(as.character(aphialist$Host))
Psite <- as.numeric(as.character(aphialist$Parasite))

Hosttest <- lapply(Host, function(i) {
  wm_record(i)
})

Host <- do.call(rbind, Hosttest)
Hosttest <- Host %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearH = `parse_number(valid_authority)`) %>%
  select(c(valid_AphiaID, Host = 'valid_name', authorityH = "valid_authority", 'YearH', phylumH = 'phylum', classH = 'class', orderH = 'order', familyH = 'family', genusH = 'genus'))

Psite <- lapply(Psite, function(i) {
  wm_record(i)
})

Psite <- do.call(rbind, Psitetest)
Psitetest <- Psite %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearP = `parse_number(valid_authority)`) %>%
  select(c(valid_AphiaID, Parasite = 'valid_name',authorityP = "valid_authority", 'YearP', phylumP = 'phylum', classP = 'class', orderP = 'order', familyP = 'family', genusP = 'genus'))

SharkWorms <- bind_cols(Hosttest, Psitetest)

#There we go. This is the hosts according to worms of parasites called out from Uconn and SharkRefs
write_csv(SharkWorms, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkWoRMS.csv")

see <- count(SharkRef2.0 , phylumH, classH)

#There are issues, as there are some genus names in species name (Host) column & some repetition. 
#str <- str_detect(SharkRef2.0$Host, " ") #check if logical works
SharkRef2.0 <- filter(SharkRef2.0, str_detect(Host, " ") == TRUE) %>%
  distinct()  
################################################################
#Lets see What this dataset is composed of and work from there
FinalSet <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkWoRMS.csv")%>%
  select(.,"Host","YearH",'phylumH','classH','orderH',
       'familyH','genusH',"Parasite","YearP",'phylumP',
       'classP','orderP','familyP', 'genusP')
#First and foremost, there are some missing Parasite/host values. (this is probably due to genus only values)
FinalSetadjusted <- filter(FinalSet, !is.na(Parasite)) %>%
  filter(., !is.na(Host)) %>%
  filter(!phylumH %in% "Platyhelminthes") 
#Secondly, There are a few species that shouldnt be there ... but they are. 

#bring in the old set and see if there are differences
SharkRef <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv") %>%
  select(.,"Host","YearH",phylumH = "PhylumH",classH = "ClassH",orderH = "OrderH",
       familyH = "FamilyH",genusH = "GenusH","Parasite","YearP",phylumP = "PhylumP",
       classP = "ClassP",orderP = "OrderP",familyP = "FamilyP", genusP = "GenusP")

Finaladjusted <- union(SharkRef, FinalSet)

#nana<- filter(Finaladjusted, is.na(YearP))
Finaladjusted <-   filter(Finaladjusted, !is.na(Host))
Finaladjusted <-   filter(Finaladjusted, !is.na(Parasite))

#FINALLY, there are both parasites and hosts with variable genus names which creates repetition in set???
see <- dplyr::select(Finaladjusted, Host, genusH, Parasite, genusP) %>%
  count(., Host, Parasite) %>%
  arrange(desc(n))

###So, I'll just pull out the genus value in the species name, and make that the new genus column ... and remove repetitions
Finaladjusted$genusH <- str_extract(Finaladjusted$Host, '[[:alpha:]]+')
Finaladjusted$genusP <- str_extract(Finaladjusted$Parasite, '[[:alpha:]]+')


#Hexanauplia to copepoda and other fine scale corrections
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

Finaladjusted <- filter(Finaladjusted, !phylumH %in% "Platyhelminthes") %>%
  filter(., !Parasite %in% "Coelorinchus caelorhincus") %>%
  filter(., !Parasite %in% "Christianella trygonbrucco") %>%
  filter(., !Parasite %in% "Dibothriorhynchus maccallumi") %>%
  filter(., !Parasite %in% "Eimeria kayarensis") 

Finaladjusted <- mutate(Finaladjusted, test = str_detect(Finaladjusted$Host, "[[:blank:]]")) %>%
  filter(., test %in% TRUE)

x <- c('Simenchelys parasitica', '1879',  'Chordata', 'Actinopterygii', 'Anguilliformes', 'Synaphobranchidae', 'Simenchelys',
'Hypertrema ambovatum', '1960', 'Platyhelminthes', 'Trematoda', 'Plagiorchiida', 'Fellodistomidae', 'Hypertrema')

Finaladjusted <- rbind(Finaladjusted, x)
rm(x)

#Final distinct
Finaladjusted <- distinct(Finaladjusted)

Finaladjusted <- Finaladjusted %>% distinct(Host, Parasite, .keep_all = TRUE) %>%
  select(-test)

write_csv(Finaladjusted, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkFull3.0.csv")

################################################################
# Let's give this set some functional and other grouping variables ...
FinalSet <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkFull3.0.csv")

#Parasite discovery by taxon and functional groups dataset:
#first split them into groups

count(FinalSet,phylumP,classP)

#Taxon/functional (phylum) Group
FinalSet <- FinalSet %>%
  mutate(group = fct_collapse(phylumP,
                               Platyhelminthes = c('Platyhelminthes'),
                               Arthropoda = c('Arthropoda'),
                               MinorGroups = c('Acanthocephala', 'Annelida', 'Chordata', 'Ciliophora', 'Cnidaria',
                                               'Euglenozoa', 'Microsporidia', 'Myzozoa', 'Nematoda')
                              ))

#Simple/ complex (class) Lifestyle
FinalSet <- FinalSet %>%
  mutate(lifestyle = fct_collapse(classP,
                              direct = c('Clitellata', 'Hexanauplia', 'Malacostraca', 'Ostracoda', 'Monogenea', 
                                         'Petromyzonti', 'Actinopterygii'),
                              indirect = c('Palaeacanthocephala', 'Adenophorea', 'Chromadorea', 'Enoplea',
                                           'Secernentea', 'Cestoda', 'Trematoda', 'Myxozoa', 'Oligohymenophorea', 'Kinetoplastea', 
                                           'Microsporea', 'Conoidasida')
                              ))

#endo/ecto (class) Habitat
FinalSet <- FinalSet %>%
  mutate(habitat = fct_collapse(classP,
                              ecto = c('Clitellata', 'Hexanauplia', 'Malacostraca', 'Ostracoda', 'Monogenea', 
                                       'Petromyzonti'), 
                              endo = c('Palaeacanthocephala', 'Adenophorea', 'Chromadorea', 'Enoplea',
                                       'Secernentea', 'Cestoda', 'Trematoda', 'Myxozoa', 'Oligohymenophorea', 'Kinetoplastea', 
                                       'Microsporea', 'Conoidasida', 'Actinopterygii')
                              ))


write_csv(FinalSet, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkFull3.0.csv")

###How about some host variables
################################################################
library(rfishbase)
# Let's give this set some functional and other grouping variables ...
FinalSet <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkFull3.0.csv") %>%
  select (Host) %>%
  distinct()

#Lets see what pops out.
#FishBase works off a bunch of relational databases. 
#Therefore, you need to call the values you want from the database and bring it together

#fields<-list_fields(server = sealifebase)

#callor<-stocks("Callorhinchus capensis")

#Now that we have identified what we want... Lets run it.
shark<- validate_names(FinalSet$Host, server = 'fishbase')
sharksp<- species(shark, fields = c("Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", 
                                             "Length", "Weight", "Vulnerability"), server = 'fishbase')
sharkst<- stocks(shark, fields = c("Species", "IUCN_Code","IUCN_DateAssessed","Protected"), server = 'fishbase')
shark<- full_join(sharksp, sharkst, by = "Species")

nonshark<- validate_names(FinalSet$Host, server = 'sealifebase')
nonsharksp<- species(nonshark, fields = c("Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", 
                                              "Length", "Weight", "Vulnerability"), server = 'sealifebase')
nonsharkst<- stocks(nonshark, fields = c("Species", "IUCN_Code","IUCN_DateAssessed","Protected"), server = 'sealifebase')
nonshark<- full_join(nonsharksp, nonsharkst, by = "Species") %>%
  distinct()

#bring it back together
fullset <- bind_rows(shark, nonshark) %>%
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
write_csv(fullset, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkVars.csv")


play <- filter(fullset, is.na(Length))
