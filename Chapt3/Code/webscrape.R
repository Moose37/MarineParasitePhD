###############################
## Webscraping SharkRefs.com ##
###############################
#See if we can update our sharkrefs count
#1- housekeeping (loading packages etc.)
#2- Establish list of websites I need to scrape from
#3- I have a "dont run" section below that runs it for an individual page. this is to identify the 
#   components of the website I'm interested in and pulling them off. once done I can create a function
#   to iteretively run through my website list above.
#4- We now have a few columns of strings that we need to bring together meaningfully. 
#   So there's a series of code to do that. This uses dplyr and stringr functions to manipulate data into 
#   the formats I need.
#5- Once this has been brought together, I then call into Worms through taxize to get species information.
#   

#Loading the rvest package
library('rvest')
library('tidyverse')

#Specifying the url for desired website to be scraped
# The SharkRef's webiste structures its species aphabetically. So We need to build a list of websites
# that run through each letter of the palphabet, scraping the information we need.
url <- (LETTERS) 
#creates a character string of all the website possibilities
url <- paste0("http://shark-references.com/species/host-parasites-list/",url,"") 

#We have it working for one page, can we make it run for 26...
#With the list, we can then use a function to iteratively run through the above list and pull specific information
species_data <- lapply(url, function(i) {
  webpage <- read_html(i)
  species_names_html <- html_nodes(webpage,'i')
  html_text(species_names_html)
})

species_data <- as_tibble(unlist(species_data))

psite_papers_data <- lapply(url, function(i) {
  webpage <- read_html(i)
  species_names_html <- html_nodes(webpage,'li')
  html_text(species_names_html)
})

psite_papers_data <- as_tibble(unlist(psite_papers_data))

host_papers_data <- lapply(url, function(i) {
  webpage <- read_html(i)
  species_names_html <- html_nodes(webpage,'div.list-text')
  html_text(species_names_html)
})

host_papers_data <- as_tibble(unlist(host_papers_data))


##########################################################################
################IF ABOVE WORKS DON'T RUN THIS!!!##########################
##########################################################################
#if running individually...
#url <- "http://shark-references.com/species/host-parasites-list/A"
#Reading the HTML code from the website
#webpage <- read_html(url)

#Using CSS selectors to scrape for information
#species_names_html <- html_nodes(webpage,'i')
#psite_papers_html <- html_nodes(webpage,'li')
#host_papers_html <- html_nodes(webpage,'div.list-text')

#Converting the ranking data to text and then into a dataframe
#species_data <- as_tibble(html_text(species_names_html))
#psite_papers_data <- as_tibble(html_text(psite_papers_html))
#host_papers_data <- as_tibble(html_text(host_papers_html))

###########################################################################
###########################################################################

############## Break the data up into it's various components.

#specifically, Host list, parasite list, then to link the two, and add number of papers columns per interaction
#Host list
host_list <- as_tibble(str_squish(host_papers_data$value)) %>% 
  ## Divide to get species and genus names
  separate(., value, into = c('genus', 'species', 'therest'), extra = "merge") %>% 
  ## bring together host names
  unite("genus", "species", col = 'value', sep = " ") %>% 
  select(-therest)

#parasite list
##pull all parasites out ...
parasite_list <- anti_join(species_data, host_list, by = "value")  
##and remove authority values
parasite_list <- separate(parasite_list, value, into = c('genus', 'species', 'authority'), extra = "merge") %>% 
  unite("genus", "species", col = 'parasite', sep = " ")

#bring them together meaningfully
## Create column of trues and falses matching host list
host_parasite_list <- mutate(species_data, test = ifelse(species_data$value %in% host_list$value, TRUE, FALSE)) 
## Pulls in matching hosts and converts falses to NA's
host_parasite_list <- mutate(species_data, host = ifelse(host_parasite_list$test == TRUE, species_data$value, NA)) 
##Fills below the host to populate parasites with their hosts
host_parasite_list <- fill(host_parasite_list,host, .direction = 'down') %>% 
  filter(!value == host) 

##Pull parasite species names out
host_parasite_list <- separate(host_parasite_list, 
                               value, into = c('genus', 'species', 'authority'), extra = "merge") %>%
  unite("genus", "species", col = 'parasite', sep = " ") %>% 
  ## ...and remove authority values
  select(-authority) 

#Use host_paper data to incorporate an index to the papers
host_papers_list <- as_tibble(str_squish(host_papers_data$value))

##pull out paper index attached to host name
host_papers_list <-  mutate(host_papers_list, index = str_match_all(host_papers_list$value, "(?<=\\[).+?(?=\\])"))
## pull in host names
host_papers_list <- bind_cols(host_list, host_papers_list) %>%
  setNames(c("host", "host2", "index")) %>%
  dplyr::select(-host2) %>%
  ##I need to extend this list to have an index associated with each host.
  unnest(cols = 'index')

#this is now ready ... Lets get the parasite papers into the same structure

##################### Break parasite papers into their various components
##remove all white space from rows
psite_papers_data <- str_squish(psite_papers_data$value)%>% 
  as_tibble() %>%
  #and keep all values with digits (removes website "titles")
  filter( str_detect(value, '[:digit:]')) 

##pulls names out to match Trues and falses
psite_papers_list <-  separate(psite_papers_data,value, into = c('genus', 'species', 'authority'), extra = "merge") %>% 
  unite("genus", "species", col = 'parasite', sep = " ")

test <- ifelse(psite_papers_list$parasite %in% host_parasite_list$parasite, TRUE, FALSE) %>%
  as_tibble()

psite_papers_list <- bind_cols(psite_papers_data, test = test) %>%
  setNames(c("value", "value1"))

psite_papers_list <- mutate(psite_papers_data, 
                            parasite = ifelse(psite_papers_list$value1 == TRUE, psite_papers_list$value, NA)) %>%
  ##Pull parasite species names out so we can match back to host list
  separate(., parasite, into = c('genus', 'species', 'authority'), extra = "merge") %>% 
  unite("genus", "species", col = 'parasite', sep = " ") %>% 
  select(-authority) 

##but now we have a bunch of 'NA NA' which we need to convert back to NA's
psite_papers_list <- na_if(psite_papers_list, "NA NA") 
##Fills below the parasite to populate parasite papers with their parasite species
psite_papers_list <- fill(psite_papers_list , parasite, .direction = 'down') %>%  
  bind_cols(., test = test) %>%
  setNames(c("value", "parasite", "value1")) %>%
  drop_na() %>%
  filter(., !value1 %in% TRUE)%>%
  filter(., !str_detect(value, 'nomen nudum'))%>%
  select(-value1)

#take out paper number between square brackets
psite_papers_list$index <- sub(".*\\[([^][]+)].*|.*", "\\1", psite_papers_list$value)

##Thats it! Now we bring all the various components together.
final_list <- bind_cols(psite_papers_list, host_papers_list) %>%
  setNames(c("value", "parasite", "psiteindex", "host", "hostindex"))%>%
  dplyr::select(papers = 'value', host, parasite, index = "psiteindex")

rm(test) 

#This is the final set for sampling effort analysis
write_csv(final_list, "C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/webscrapeSharkRef.csv")

rm(host_list, host_papers_data, host_papers_list, 
   parasite_list, psite_papers_data, psite_papers_list, species_data, url)

#########################################################################
##########This is to call species information off worms##################
#########################################################################
#Now ... Lets break this into the SharkRef's dataset with required stuff for full set.

library('taxize')
library('worrms')

SharkPapers <- read_csv("C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/webscrapeSharkRef.csv") %>%
  select(host, parasite) %>%
  distinct()
# Or if youve used ALL of the above just use "host_parasite_list"
SharkPapers <- distinct(host_parasite_list)

#Lets clean it up and go from there.
codes<- mutate(SharkPapers, Parasite = as.numeric(get_wormsid(SharkPapers$parasite, accepted = FALSE, ask = FALSE)))
codes<- mutate(codes, Host = as.numeric(get_wormsid(SharkPapers$host, accepted = FALSE, ask = FALSE)))

#I want to see all the NA's before I run this
NAS <- filter(codes, is.na(Parasite)) %>%
  select(parasite) %>%
  distinct() %>%
  filter(!str_detect(parasite, " sp")) %>%
  filter(!parasite %in% "nomen nudum")

#brings it down to 256 species that need cleaning.
psite<- as.numeric(get_wormsid(NAS$parasite)) #12 species.

#Now bring them back in.
NAS <- cbind(NAS, psite)
codestest <- left_join(codes, NAS, by = "parasite")
codes <- mutate(codestest, Parasite = if_else(!is.na(psite), psite, Parasite)) %>%
  select(-psite)
  

rm(NAS, psite)

#pull full WoRMS records

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
Finalset <- left_join(codes, Host, by = c("Host" = "AphiaIDh")) %>%
  select(- Host) %>%
  rename (Host = "Host.y")

Finalset <- left_join(Finalset, Psite, by = c("Parasite" = "AphiaIDp")) %>%
  select(- Parasite) %>%
  rename (Parasite = "Parasite.y")

#There are a few NA's ... lets clean these out
Finalset <- filter(Finalset, !is.na(Parasite))%>%
  filter(.,!is.na(Host)) %>%
  select(-host, -parasite)



write_csv(Finalset, "C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt3/Data/SharkRefscrape.csv")
####

