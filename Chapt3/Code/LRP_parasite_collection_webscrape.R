#############################################################
##########Bring it all together with web scraping############
#############################################################
#1) Pull data from http://lrpennerdb.uconn.edu/index.php/parasites/specimen_results/1
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
##Remember to jump onto the website and see if anything has changed, particularly the number of pages
url <- 1:168 
url <- paste0("http://lrpennerdb.uconn.edu/index.php/parasites/specimen_results/",url,"") #creates a character string of all the website possibilities

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
codes<- mutate(UConnNew, Parasite = as.numeric(get_wormsid(UConnNew$Parasite, accepted = FALSE, ask = FALSE)))
codes<- mutate(codes, Host = as.numeric(get_wormsid(UConnNew$Host, accepted = FALSE, ask = FALSE)))


Psite <- select(codes, Parasite) %>% distinct() %>% filter(!is.na(Parasite))
Psitetest <- lapply(Psite$Parasite, function(i) {
  wm_record(id = i)
})

Psitetest <- do.call(rbind, Psitetest)

Host <- select(codes, Host) %>% distinct() %>% filter(!is.na(Host))
Hosttest <- lapply(Host$Host, function(i) {
  wm_record(id = i)
})

Hosttest <- do.call(rbind, Hosttest)

#now bring it back together
Psite<- select(Psitetest, AphiaIDp = 'valid_AphiaID', Parasite = 'valid_name', Authority = "valid_authority", 
               phylumP = "phylum", classP = "class", orderP = "order", familyP = "family", genusP = "genus") %>%
  mutate_at(vars('Authority'), funs(gsub("[[:punct:]]", '', Authority))) %>%
  mutate(YearP = parse_number(Authority)) %>%
  select(-Authority)

Host<- select(Hosttest, AphiaIDh = 'valid_AphiaID', Host = 'valid_name',Authority = "valid_authority", 
              phylumH = "phylum", classH = "class", orderH = "order", familyH = "family", genusH = "genus")%>%
  mutate_at(vars('Authority'), funs(gsub("[[:punct:]]", '', Authority))) %>%
  mutate(YearH = parse_number(Authority)) %>%
  select(-Authority)

##
UConnNew <- left_join(codes, Host, by = c("Host" = "AphiaIDh")) %>%
  select(- Host) %>%
  rename (Host = "Host.y")

UConnNew <- left_join(UConnNew, Psite, by = c("Parasite" = "AphiaIDp")) %>%
  select(- Parasite) %>%
  rename (Parasite = "Parasite.y")

#There are a few NA's ... lets clean these out
UConnNew <- filter(UConnNew, !is.na(Parasite))%>%
  filter(.,!is.na(Host)) 


write_csv(UConnNew, "C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/SharkLRP.csv")
######################################################


