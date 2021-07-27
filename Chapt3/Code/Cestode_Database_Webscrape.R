#############################################################
##########Bring it all together with web scraping############
#############################################################
#1) Pull data from http://www.tapewormdb.uconn.edu/index.php/parasites/species_results/1
#2) Pull all of Chondrichthys parasite attributes from WoRMS
#3) Pull all of them together and bring in a bunch of attributes

#Loading the rvest package
library('rvest')
library('tidyverse')
library('worrms')
library('taxize')

#############################################################
#1) UConn dataset
#Specifying the url for desired website to be scraped
##Remember to jump onto the website and see if anything has changed, particularly the number of pages
url <- 1:254
url <-paste0("http://www.tapewormdb.uconn.edu/index.php/parasites/species_results/",url,"")

#We have it working for one page, can we make it run for all of them...
table_data <- lapply(url, function(i) {
  webpage <- read_html(i)
  table_html <- html_nodes(webpage, 'table')
  table_data <- html_table(table_html, fill = TRUE)
  table_data[[1]]
})

table_data <- do.call(rbind, table_data)

#############################################################
#Now we have the dataset ... Lets clean it up. Just Host-Parasite list
colnames(table_data)
Uconnnew <- select(table_data, 'Genus', 'Species', 'Type Host Genus (Literal)', 'Type Host Species (Literal)', 
                   'Type Host Genus (Valid)', 'Type Host Species (Valid)') %>%
  unite(., 'Genus', 'Species', col = 'Parasite', sep = " ")%>%
  unite(.,'Type Host Genus (Literal)', 'Type Host Species (Literal)', col = 'Host_literal', sep = " ") %>%
  unite(.,'Type Host Genus (Valid)', 'Type Host Species (Valid)', col = 'Host_valid', sep = " ") %>%
  distinct()

#Run this at some point...
Uconncodes<- mutate(Uconnnew, Parasite = as.numeric(get_wormsid(Uconnnew$Parasite, accepted = FALSE, ask = FALSE)))
Uconncodes<- mutate(Uconncodes, Host_literal = as.numeric(get_wormsid(Uconncodes$Host_literal, accepted = FALSE, ask = FALSE)))
Uconncodes<- mutate(Uconncodes, Host_valid = as.numeric(get_wormsid(Uconncodes$Host_valid, accepted = FALSE, ask = FALSE)))

# Now let's clean that list
#Gather the two "host" rows as one, remove duplicates, and filter out the NA's
Uconncodestest <- gather(Uconncodes, key = 'set', value = 'Host', 'Host_valid', 'Host_literal') %>%
  distinct() %>%
  filter(!is.na(Parasite)) %>%
  filter(!is.na(Host)) %>%
  select(-set)

#Lets split parasites and hosts, run a name resolver and then bring back together.
Psite <- select(Uconncodestest, Parasite) %>% distinct()
Psitetest <- lapply(Psite$Parasite, function(i) {
  wm_record(id = i)
})

Psitetest <- do.call(rbind, Psitetest)

Host <- select(Uconncodestest, Host) %>% distinct()
Hosttest <- lapply(Host$Host, function(i) {
  wm_record(id = i)
})

Hosttest <- do.call(rbind, Hosttest)


#Now I have full WoRMS records for all species. Now to clean them up and bring them together meaningfully
#colnames(Psitetest)
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
Finalset <- left_join(Uconncodestest, Host, by = c("Host" = "AphiaIDh")) %>%
  select(- Host) %>%
  rename (Host = "Host.y")

Finalset <- left_join(Finalset, Psite, by = c("Parasite" = "AphiaIDp")) %>%
  select(- Parasite) %>%
  rename (Parasite = "Parasite.y")

#There are a few NA's ... lets clean these out
Finalset <- filter(Finalset, !is.na(Parasite))%>%
  filter(.,!is.na(Host))


write_csv(Finalset,"C:/Users/Mooseface/Google Drive/University/PhD NZ/Data_and_code/webscrapeCestodeDB.csv")
