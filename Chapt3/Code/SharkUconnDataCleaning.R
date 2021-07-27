#What the fuck did I do here:
# I pulled in all of Planetary Biodiversity Inventory (2008-2017): Tapeworms from the vertebrate bowels of the earth off the website. http://lrpennerdb.uconn.edu/
# I also pulled all Shark References data
# I then resolved all their names with GNR resolve. 
# Finally, we remove duplicat host-parasite records between the two datasets and capeesh... we have the best list out there.
# but wait ... there's more. We now want to improve it by implementing the Worms list to see if there are any additional connections.

#Housekeeping
library(worrms)
library(taxize)
library(tidyverse)


#all data
UConn <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/tapewormspecies.csv") %>%
  select(c('Genus', 'Species', 'Taxonomic Status', 'Valid Name', 'Type Host(valid) Genus', 'Type Host(valid) Species', 'Additional Hosts')) %>%
  filter(!is.na(Species)) %>%
  #filter(is.na(Taxonomic Status)| Taxonomic Status) %>%
  unite(., 'Genus', 'Species', col = 'Parasite', sep = " ")%>%
  unite('Type Host(valid) Genus', 'Type Host(valid) Species', col = 'Host', sep = " ")

UConnhosts <- separate(UConn, 'Additional Hosts', into = c('Host1', 'Host2', 'Host3', 'Host4', 'Host5', 'Host6', 'Host7', 'Host8', 'Host9', 'Host10'), sep = '[[:punct:]]') %>%
  gather(., 'Host', 'Host1', 'Host2', 'Host3', 'Host4', 'Host5', 'Host6', 'Host7', 'Host8', 'Host9', 'Host10', key = 'Hosta', value = 'Host')%>%
  filter(Host != "NA NA") %>%
  mutate_at(vars(Host), funs(gsub("[[:punct:]]", '', Host)))

Valid<- filter(UConnhosts, is.na(`Taxonomic Status`)| `Taxonomic Status` == "Valid name") %>%
  select(c('Parasite', 'Host'))
Invalid<- filter(UConnhosts, `Taxonomic Status` != "Valid name") %>%
  select(c(Parasite = 'Valid Name', 'Host')) 

Valid<- dplyr::union(Valid, Invalid)

#gnr_datasources()
#Getting year of discovery and linking it to stuff. It also removes annoying genus only names

ValidGNRPsiteT <- gnr_resolve(Valid$Parasite, data_source_ids = c(1,9,11), resolve_once = TRUE, canonical = TRUE)
ValidGNRPsiteF <- gnr_resolve(Valid$Parasite, data_source_ids = c(1,9,11), resolve_once = TRUE, canonical = FALSE)
ValidGNRPsiteF <-mutate_at(ValidGNRPsiteF, vars(matched_name), funs(gsub("[[:punct:]]", '', matched_name))) %>%
  mutate(YearP = parse_number(matched_name))%>%
  select(.,c('submitted_name', 'YearP'))

ValidGNRHostT <- gnr_resolve(Valid$Host, data_source_ids = c(1,9,11), resolve_once = TRUE, canonical = TRUE)
ValidGNRHostF <- gnr_resolve(Valid$Host, data_source_ids = c(1,9,11), resolve_once = TRUE, canonical = FALSE)
ValidGNRHostF <- mutate_at(ValidGNRHostF, vars(matched_name), funs(gsub("[[:punct:]]", '', matched_name))) %>%
  mutate(YearH = parse_number(matched_name)) %>%
  select(.,c('submitted_name', 'YearH'))

ValidGNRHost <- separate(ValidGNRHostT, matched_name2, into = c('genus', 'species')) %>%
  filter(!is.na(species)) %>%
  unite(genus, species, col = "Host", sep = ' ') %>% 
  select(c('submitted_name', "Host")) %>%
  left_join(.,ValidGNRHostF, by = 'submitted_name') %>%
  distinct()

ValidGNRPsite <- separate(ValidGNRPsiteT, matched_name2, into = c('genus', 'species')) %>%
  filter(!is.na(species)) %>%
  unite(genus, species, col = "Parasite", sep = ' ') %>%
  select(c('submitted_name', 'Parasite')) %>%
  left_join(.,ValidGNRPsiteF, by = 'submitted_name') %>%
  distinct()

#Link it back to original Tapeworm database  
UConnhosts <- left_join(UConnhosts, ValidGNRHost, by = c('Host'= 'submitted_name'))
UConnhosts <- left_join(UConnhosts, ValidGNRPsite, by = c('Parasite'= 'submitted_name'))
colnames(UConnhosts)

UConnhosts <- select(UConnhosts, c(Host = 'Host.y', YearH, Parasite = 'Parasite.y', YearP))
UConnhosts <- distinct(UConnhosts)%>%
  filter(!is.na(YearH)&!is.na(YearP))
#write_csv(UConnhosts, "C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/TapewormClean.csv")

# Now we moving
#Read in the reference set to compare to UConn

UConn <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/TapewormClean.csv")

SharkRef <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/SharkHostParasitelist.csv")%>%
  select(c(Clade,Host,YearH,Parasite,YearP)) %>%
  mutate(diff = YearP - YearH)

sharksdat <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/NewData/Sharklist.csv") %>%
  filter(isMarine==1) %>%
  filter(status=='accepted') %>%
  select(c(valid_AphiaID,valid_name,valid_authority,status,phylum,class,order,family,genus)) %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(Year = `parse_number(valid_authority)`)

sharksweb <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/NewData/FullSharklist.csv") %>%
  filter(isMarine==1) %>%
  filter(status=='accepted') %>%
  select(c(valid_AphiaID,valid_name,valid_authority,status,phylum,class,order,family,genus)) %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(Year = `parse_number(valid_authority)`)

sharkp <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/NewData/SharkParasitelist.csv") %>%
  filter(isMarine==1) %>%
  filter(status=='accepted') %>%
  select(c(valid_AphiaID,valid_name,valid_authority,status,phylum,class,order,family,genus)) %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(Year = `parse_number(valid_authority)`)

## Now we draw out all shark species and call out the parasite links.
# first link the parasite lists

pa <- SharkRef %>%
  select(c(Host, YearH, Parasite, YearP)) %>%
  dplyr::union(., UConn) %>%
  arrange(Host)
#just remember, this is the entire TapeWorm list with the shark list. Includes terrestrial and marine species mixed with sharkRefs
#Now we fine tune it down to just sharks and rays and their parasites
sh <- sharksweb %>%
  select(c(Host = 'valid_name', YearH = 'Year')) %>%
  left_join(., pa, by = c('Host', 'YearH')) %>%
  filter(!is.na(Parasite)) %>%
  filter(!is.na(YearP)) %>%
  filter(YearP > 1000)
count<-count(sharksweb, Host)
#write_csv(sh, ("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/SharkHostParasitelistUConn.csv"))

SharkRef<-read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/SharkHostParasitelistUConn.csv") 
colnames(SharkRef)
### Pulling in full classification for SharkRefs + UConn

sharkp <- select(sharkp, c('valid_name', phylumP = 'phylum', classP = 'class', orderP = 'order', familyP = 'family', genusP = 'genus'))
sharksweb <- select(sharksweb, c('valid_name', phylumH = 'phylum', classH = 'class', orderH = 'order', familyH = 'family', genusH = 'genus'))

SharkRef<- SharkRef%>%
  select(., c('Host', 'YearH', 'Parasite', 'YearP'))%>%
  left_join(., sharkp, by = c('Parasite' = 'valid_name')) %>%
  left_join(., sharksweb, by = c('Host' = 'valid_name')) %>%
  select(.,c('Host', 'YearH', 'phylumH', 'classH', 'orderH', 'familyH', 'genusH', 'Parasite', 'YearP', 'phylumP', 'classP', 'orderP', 'familyP', 'genusP'))

write_csv(SharkRef, ("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/SharkHostParasitelistUConn.csv"))

NAS<- filter(SharkRef, is.na(genusP))
NAS<- classification(NAS$Parasite, db = 'gbif')
NAS<- do.call(rbind, NAS)
phylum<- filter(NAS, rank == 'phylum')
class<- filter(NAS, rank == 'class')
order<- filter(NAS, rank == 'order')
family<- filter(NAS, rank == 'family')
genus<- filter(NAS, rank == 'genus')
NASt<-cbind.data.frame(phylum$name, class$name, order$name, family$name, genus$name)
NASt<-colnames(c(phylum, class, order, family, genus))

sharkp<- dplyr::union(sharkp, NASt)

NASt<- left_join(SharkRef, NASt, by = c('Parasite' = 'valid_name'))

#

# Collection continued. Let's see what WoRMS holds...

SharkRef<-read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/SharkHostParasitelistUConn.csv") %>%
  select(-diff)
# Pull attributes off worms for parasites. See if we can increase Host/Parasite list.
sharkp <- dplyr::select(SharkRef, c('Parasite', Year = 'YearP'))%>% 
  distinct() %>%
  arrange(Parasite)

attributes <- wm_attr_data_(id = as.numeric(get_wormsid(sharkp$Parasite)))
att<-do.call(rbind, attributes$children)
atta<- filter(att, measurementTypeID %in% '58')
attb<-do.call(rbind, att$children)
attb<- filter(attb, measurementTypeID %in% '58')
attcomb<-rbind(atta,attb)
aphialist <- cbind.data.frame(Host = attcomb$measurementValue, Parasite = attcomb$AphiaID)

#housekeeping
rm(attributes, att, atta, attb, attcomb)

#I have a list of Host AphiaID's and Parasite AphiaID's, Now to populate the dataframe.
Host <- worms_records(ids = as.double(as.character(aphialist$Host))) %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearH = `parse_number(valid_authority)`) %>%
  select(c(Host = 'valid_name', status = 'status', 'YearH', phylumH = 'phylum', classH = 'class', orderH = 'order', familyH = 'family', genusH = 'genus'))

Parasite <- worms_records(ids = as.double(as.character(aphialist$Parasite))) %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(YearP = `parse_number(valid_authority)`) %>%
  select(c(Parasite = 'valid_name',status1 = 'status', 'YearP', phylumP = 'phylum', classP = 'class', orderP = 'order', familyP = 'family', genusP = 'genus'))

SharkRef2.0 <- bind_cols(Host, Parasite)%>%
  filter(status =='accepted') %>%
  filter(status1 =='accepted') %>%
  select(-c('status', 'status1'))

#There are issues, as there are some genus names in species name (Host) column & some repetition. 
#str <- str_detect(SharkRef2.0$Host, " ") #check if logical works
SharkRef2.0 <- filter(SharkRef2.0, str_detect(Host, " ") == TRUE) %>%
  distinct()  

#Now we need to join the the two datasets ... Uconn/sharkref's with worms.
anti <- anti_join(SharkRef2.0, SharkRef)
count(SharkRef2.0, phylumH, classH)

#There is an interesting thing that just happened
#These parasites don't just infest cartilagenous fish, but other animals too! This will have implications for specificity measures as differences
#go further than phylum. So Lets create two sets for different analyses. SharkRef (only cart), SharkReffull (everything).
#SharkReffull (everything)
SharkRef3.0 <- dplyr::union(SharkRef2.0, SharkRef) %>%
  filter(phylumH != 'Platyhelminthes')

see <- count(SharkRef3.0, phylumH)

#SharkRef (only cartilagenous fish)
SharkRef3.0 <- dplyr::union(SharkRef2.0, SharkRef) %>%
  dplyr::filter(classH %in% c('Elasmobranchii', 'Holocephali'))

#Bust first, replace all NA's in the year columns with actual years.
#Hosts
nana <- filter(SharkRef3.0, is.na(YearH)) %>%
  select(-YearH)
nanahost<- gnr_resolve(nana$Host, data_source_ids = 9, canonical = FALSE)%>%
  mutate_at(vars('matched_name'), funs(gsub("[[:punct:]]", '', matched_name))) %>%
  mutate(parse_number(matched_name)) %>%
  rename(YearH = `parse_number(matched_name)`)%>%
  select(c(submitted_name, YearH))
#nanahost[12,2] <- 1810
nana<- left_join(nana, nanahost, by = c('Host' = 'submitted_name'))
SharkRef3.0 <- filter(SharkRef3.0, !is.na(YearH))
SharkRef3.0 <- dplyr::union(SharkRef3.0, nana)

#Parasites
nana <- filter(SharkRef3.0, is.na(YearP)) %>%
  select(-YearP)
nanahost<- gnr_resolve(nana$Parasite, data_source_ids = 9, canonical = FALSE)%>%
  mutate_at(vars('matched_name'), funs(gsub("[[:punct:]]", '', matched_name))) %>%
  mutate(parse_number(matched_name)) %>%
  rename(YearP = `parse_number(matched_name)`)%>%
  select(c(submitted_name, YearP))
nana<- left_join(nana, nanahost, by = c('Parasite' = 'submitted_name'))
SharkRef3.0 <- filter(SharkRef3.0, !is.na(YearP))
SharkRef3.0 <- dplyr::union(SharkRef3.0, nana)

#write_csv(SharkRef3.0, ("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv"))
#write_csv(SharkRef3.0, ("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv"))


# finally ... How about throwing locality stuff onto this dataset.

sharksweb <- read_csv("C:/Users/tmor201/Google Drive/Publications/Mark idea's/Mark data/NewData/FullSharklist.csv") %>%
  filter(isMarine==1) %>%
  filter(status=='accepted') %>%
  mutate_at(vars('valid_authority'), funs(gsub("[[:punct:]]", '', valid_authority))) %>%
  mutate(parse_number(valid_authority)) %>%
  rename(yearH = `parse_number(valid_authority)`)

wm_distribution(id = sharksweb$valid_AphiaID)

########
#Pull full taxonomic classification for species known to infest cart species.

SharkRef <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv") %>%
  select('Host','YearH')
lost <- distinct(lost)
lost <- worms_records(scientific = SharkRef$Host)
lost <- filter(lost, status %in% 'accepted')%>%
  select(., c('valid_name', 'valid_AphiaID'))

class<- classification(lost$valid_AphiaID, db = 'worms')
play <- tibble(names = names(class), class) %>% 
  unnest() %>% 
  filter(rank %in% c("Phylum","Subphylum", "Superclass", "Class", "Infraclass", "Subclass", "Superorder", "Order","Family", "Subfamily","Genus")) %>% 
  filter(!(name %in% "Pisces"))%>%
  select(-id) %>% 
  spread(rank, name) %>% 
  select(valid_AphiaID = names, Phylum,Subphylum,Superclass,Class,Infraclass,Subclass,Superorder,Order,Family,Subfamily,Genus)

play$valid_AphiaID <- as.numeric(play$valid_AphiaID)

#class <- left_join(SharkRef, play, by = c('Host'='valid_AphiaID'))
sharkswebfull  <- left_join(sharksweb, play, by = 'valid_AphiaID')
SharkReffull <- left_join(SharkRef, play, by = 'valid_AphiaID') 

SharkRef <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv")
SharkReffull <- bind_cols(SharkReffull, select(SharkRef, 8:14))

#write_csv(SharkReffull, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv"))
#write_csv(sharkswebfull, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/FullSharklist.csv"))

# here are some species w/o full taxonomies.
NAS <- filter(Sharktax, is.na(Phylum)) #currently 12 (sharks) and 285 for SharkRef
NAS <- classification(NAS$Species, db = 'worms')
NAS <- rbind(NAS) %>%
  filter(rank %in% c("Phylum","Subphylum", "Superclass", "Class", "Infraclass", "Subclass", "Order",
                     "Family","Subfamily","Genus")) %>% 
  filter(!(name %in% c("Pisces","Tetrapoda")))%>%
  select(-id) %>% 
  spread(rank, name) %>% 
  select(Phylum,Subphylum,Superclass,Class,Infraclass,Subclass,Order,
         Family,Subfamily,Genus,Species = 'query')
Sharktax <- filter(Sharktax, !is.na(Phylum))
Sharktax <- bind_rows(Sharktax, NAS) %>%
  arrange(Species)
see <- select(Shark,Host,YearH,Parasite,YearP,phylumP, classP,orderP,familyP,genusP)
see <- left_join(see,Sharktax, by = c("Host" = "Species"))
see <- select(see,Host,YearH,Phylum,Subphylum,Superclass,Class,Infraclass,Subclass,Order,
              Family,Subfamily,Genus,Parasite,YearP,phylumP,classP,orderP,familyP,genusP)
#Sharktax <- column_to_rownames(Sharktax, var = 'Species') #Species to rowname

#write_csv(see, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv"))
#write_csv(see, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv"))

# there are double ups due to taxonomic issues
#Only cartilaginous fish ... and their parasites
Shark <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv")

#Parasites of cartilagenous fish and their hosts (incl. those that are not cartilageinous fish)
SharkRef <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv")

sharkp <- dplyr::select(SharkRef, c('Parasite', Year = 'YearP', Phylum = 'phylumP', Class = 'classP', Order = 'orderP', 
                                    Family = 'familyP', Genus = 'genusP'))%>%
  distinct()

NAS <- classification(sharkp$Parasite, db = 'worms')
NAStest <- rbind(NAS) %>%
  filter(rank %in% c("Phylum","Subphylum", "Superclass", "Class", "Infraclass", "Subclass", "Order",
                     "Family","Subfamily","Genus", "Species")) %>% 
  filter(!(name %in% c("Pisces")))%>%
  distinct() %>%
  select(-id) %>% 
  spread(rank, name) %>% 
  select(Phylum,Class,Order,
         Family,Genus,Species)
  
NAStest<-arrange(NAStest,Species)

### There are parasites with NA's across taxonomy and misspelled names.

SharkRef <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv")
  #SharkRef <- read_csv("C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv")
#colnames(SharkRef)
Shark <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv")
  #SharkRef <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv")
#as there are duplicates (I assume)
Shark <- select(Shark, 'Host') %>%
  left_join(., SharkRef, by = 'Host') %>%
  distinct()

NAS <- filter(SharkRef, is.na(PhylumP)) #filter out the issues
NAS <- classification(NAS$Parasite, db = 'gbif') #pull taxonomy from GBIF
NAStest <- rbind(NAS) %>%
  filter(rank %in% c('phylum','class','order','family','genus','species')) %>% 
  filter(!(name %in% c("Pisces")))%>%
  distinct() %>%
  select(-id) %>% 
  spread(rank, name) %>% 
  select (Phylum = 'phylum',Class = 'class',Order = 'order',Family = 'family',Genus = 'genus',Species = 'species', submitted_name = 'query') %>%
  mutate(matched_name2 = if_else(is.na(Species), submitted_name, Species))

pull<- Shark[c(1:18)] # only pull host info + parasite names and years

push<- SharkRef[c(17,19:29)] %>% #pull out only parasite info
  distinct()

minus <- filter(push, !Parasite %in% c(NAStest$submitted_name))%>% #remove the names that we weren't happy with (Fuck 'em) 
   mutate(submitted_name = Parasite)               #create a second name column called submitted_name

#The original set (pull), is still using the old (wrong names). so what we could do is try create two name columns 
#(the wrong ones and the right ones). Then we join to the wrong ones, and then delete the wrong column so the correct names remain.

cor <- select(NAStest, PhylumP = "Phylum", ClassP = "Class", OrderP = "Order", FamilyP = "Family",GenusP = "Genus", 
              Parasite = "matched_name2", submitted_name)
minustest <- full_join(cor, minus) #Lets go! # now we don't need those and can join our clean parasites with the rest of them
  
pulltest <- left_join(pull, minustest, by = c('Parasite' = 'submitted_name')) %>%
  select(!Parasite) %>%
  rename(Parasite = Parasite.y) %>%
  distinct() %>%
  select(Host,YearH,PhylumH,SubphylumH,SuperclassH,ClassH,Infraclass,SubclassH,SuperorderH,OrderH,
         SuborderH,Infraorder,SuperfamilyH,FamilyH,Subfamily,GenusH,Parasite,YearP,PhylumP,SubphylumP,
         SuperclassP,ClassP,SubclassP,SuperorderP,OrderP,SuborderP,SuperfamilyP,FamilyP,GenusP) 



#Clean out specific issues, particulalrly double ups:
see<- dplyr::count(pulltest,Parasite,Host) %>%
  arrange(desc(n))

final <- filter(pulltest, !(Parasite %in% "Nesippus orientalis" & YearP %in% '1865'))
final <- filter(final, !(Parasite %in% "Nesippus crypturus" & YearP %in% '1865'))
final <- filter(final, !(Parasite %in% "Tetrarhynchobothrium tenuicolle" & YearP %in% '1850'))
final <- filter(final, !(Parasite %in% "Tentacularia coryphaenae" & YearP %in% '1797'))
final <- filter(final, !(Parasite %in% "Orygmatobothrium musteli" & YearP %in% '1850'))
final <- filter(final, !(Parasite %in% "Orygmatobothrium musteli" & YearP %in% '1850'))
final <- filter(final, !(Parasite %in% "Pedibothrium lintoni" & YearP %in% '1986'& Host %in% "Ginglymostoma cirratum"))
final <- filter(final, !(Parasite %in% "Pedibothrium lintoni" & YearP %in% '1986'& Host %in% "Stegostoma fasciatum"))
final <- filter(final, !(Parasite %in% "Calliobothrium cisloi" & YearP %in% '2016'))		
final <- filter(final, !(Parasite %in% "Kroeyerina scottorum" & YearP %in% '1970'))
final <- filter(final, !(Parasite %in% "Prochristianella tumidula" & YearP %in% '1890'))	
final <- filter(final, !(Parasite %in% "Pterobothrium australiense" & YearP %in% '1997'))	
final <- filter(final, !(Parasite %in% "Trilocularia gracilis" & YearP %in% '1869'))		
final <- filter(final, !(Parasite %in% "Dinobothrium plicitum" & Host %in% "Sphyrna mokarran" & is.na(SubphylumP)))	
final <- filter(final, !(Parasite %in% "Acanthocheilus rotundatus" &	class %in% "Secernentea"))

see<- dplyr::count(final,Parasite,Host) %>%
  arrange(desc(n))

retest <- filter(final, is.na(PhylumP))

#Now there are a bunch of parasites with different years but the same name, these include:
tes<-filter(SharkRef,Parasite %in% 'Acanthobothrium filicolle')
tes<-filter(test,Parasite %in% 'Acanthobothrium filicolle')

test <- mutate(Shark,YearP = ifelse(Parasite %in%'Acanthobothrium filicolle', 1887, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium atahualpai',  1978, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium gracile',  1954, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium lintoni',  1968, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium marplatensis',  1998, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium mathiasi',  1959, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthobothrium rajaebatis',  1810, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Acanthocheilus rotundatus',  1819, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Alebion crassus',  1932, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Anthobothrium variabile',  1889, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Dasyrhynchus basipunctatus',  2014, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Dasyrhynchus giganteus',  1850, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Echeneibothrium dubium',  1858, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Echeneibothrium fallax',  1870, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Floriceps minacanthus',  1987, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Gymnorhynchus gigas',  1819, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Heterosphyriocephalus encarnae',  2017, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Kotorella pronosoma',  1901, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Kroyeria caseyi',  1986, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Lernaeopoda musteli',  1890, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Monorygma chlamydoselachi',  1898, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Nemesis lamna',  1826, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Nybelinia syngenes',  1928, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Orygmatobothrium musteli',  1849, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Paronatrema boholanum',  2010, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Pedibothrium lintoni',  1980, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Poecilancistrium caryophyllum',  1929, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Prochristianella minima',  1974, YearP))
test <- mutate(test,YearP = ifelse(Parasite %in%'Stoibocephalum arafurense',  2013, YearP))

test <- distinct(test)

#DOne! looks good!



#write_csv(SharkRef, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkReffull.csv"))
#write_csv(Shark, ("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/SharkRef.csv"))

#Finally ... Lets pull the full Elasmo and Holo dataset out
cart <- c('Elasmobranchii','Holocephali')
cart <- worms_records(cart, 'Class')
cart <- filter(cart, status %in% 'accepted') %>% filter(!is.na(valid_authority))
year <- mutate_at(cart, vars(valid_authority), funs(gsub("[[:punct:]]", '', valid_authority)))
year <- mutate(cart, Year = parse_number(valid_authority))%>%
  select(Species = 'valid_name',AphiaID = "valid_AphiaID",Year)

cartclass <- classification(year$AphiaID, db = 'worms')
play <- tibble(names = names(cartclass), cartclass) %>% 
  unnest(cols = c(cartclass)) %>% 
  filter(rank %in% c("Phylum","Subphylum", "Superclass", "Class", "Infraclass", "Subclass", "Superorder", "Order","Family", "Subfamily","Genus")) %>% 
  filter(!(name %in% "Pisces"))%>%
  select(-id) %>% 
  distinct() %>%
  spread(rank, name) %>% 
  select(AphiaID = names, Phylum,Subphylum,Superclass,Class,Infraclass,Subclass,Superorder,Order,Family,Subfamily,Genus)

play$AphiaID <- as.numeric(play$AphiaID)

playfull<- left_join(year, play, by = c("AphiaID") )

#class <- left_join(SharkRef, play, by = c('Host'='valid_AphiaID'))

#write_csv(playfull, "C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/FullSharklist.csv")
