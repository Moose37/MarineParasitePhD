### Plot Discovery rates by year
#Article
setwd("C:/Users/mooseface/Google Drive/Publications/Mark idea's")
#Chapter 2
setwd("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code")

#source("DiscoveryRates.R")

#Need to pull all details together First

library(tidyverse)
library(ggpubr)
library(ggformula)
library(tidyquant)
library(Cairo)
library(mgcv)
library(taxize)
library(worrms)
library(robis)
library(rgbif)
library(gratia)
library(fitdistrplus)
library(vcd)
library(modelr)
library(forecast)
library(lemon)
library(nlme)
library(segmented)

###Housekeeping of datasets

#pull in dataframes (MarPara = Marine Parasites, marinedata = All Marine Species)

#decrease column number to relevant variables > select()
#remove punctuation                           > mutate_at(vars(authority), funs(gsub("[[:punct:]]", '', authority)))
#add year from authority                      > mutate(Year = parse_number(authority))


#file.edit("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/DiscoveryRates.R")
#MarPara <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/Worms_psite2.0.csv") %>%
MarPara <- read_csv("C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/FullParasite.csv.gz") %>%
  dplyr::select(.,c("valid_AphiaID","valid_name","valid_authority","status","phylum","class","order")) %>%
  mutate_at("valid_authority", ~ gsub("[[:punct:]]", '', valid_authority)) %>%
  mutate(Year = parse_number(valid_authority)) %>%
  dplyr::select(valid_AphiaID,valid_name,valid_authority,status,phylum,class,order,Year)

#file.edit("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Animaliadata.R")
#marinedata <- read_csv("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/WormsSpeciesList3.0.csv")%>%
marinedata <- read_csv("C:/Users/mooseface/Google Drive/University/PhD NZ/MarineParasitePhD/Chapt2/Data/FullAnimalia.csv.gz")%>%
  dplyr::select(.,c("valid_AphiaID","valid_name","valid_authority","status","phylum","class","order")) %>%
  mutate_at("valid_authority", ~ gsub("[[:punct:]]", '', valid_authority)) %>%
  mutate(Year = parse_number(valid_authority))

## How many marine parasites are in the marine species dataset? and can we remove them?
# 22 386 species similar.
marinedata <- dplyr::setdiff(marinedata, MarPara)
#27699 / 195985
#27699 / 171016
#199057-195985
#27699 / 199057
#27699 / 171016
#199057-27699
#phy<-dplyr::count(MarPara, phylum, class)
write.csv(phy,"C:/Users/mooseface/Google Drive//University/PhD NZ/Data_and_code/classes.csv")

### Now That we have years extracted. Lets pull this into a discovery rates set
DRMarPara <- MarPara
DRMarPara <- DRMarPara %>%
  group_by(Year) %>% #group by year
  summarise(n()) #Summarise (count) by group (year)  

#DRTerrPsite <- TerrPsite
#DRTerrPsite <- DRTerrPsite %>%
# group_by(Year) %>%
#  summarise(n()) %>%
#  filter(Year >= 1750) %>%
#  filter(Year <= 2005)

DRmarinedata <- marinedata
DRmarinedata <- DRmarinedata %>%
  group_by(Year) %>%
  summarise(n())

DR <-  full_join(DRmarinedata, DRMarPara, by='Year') %>%
  #full_join(.,DRTerrPsite, by = 'Year') %>% #join all three groups together
  rename(., MarineSpecies = `n().x`) %>%
  #rename(., TerrestrialParasites = `n()`) %>%
  rename(., MarineParasites = `n().y`) %>% #rename column values
  filter(Year >= 1750) %>%
  filter(Year <= 2018) %>% #subset to working time range
  arrange(Year)  

rm(DRMarPara, DRmarinedata)

#tidy data
#DRt<- gather(DR, 'MarineSpecies','TerrestrialParasites', 'MarineParasites', key = set, value = dr)
DRt<- pivot_longer(DR, c('MarineSpecies', 'MarineParasites'), names_to = "set", values_to = "dr")
DRt$set<- as.factor(DRt$set)

# Produces df. of count by year across all three datasets.

#Lets draw a simple scatter plot

#qplot(x = Year, y = dr, data=DRt, color = set, geom = "auto")

# Basics of GGplot:
#http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
#ggplot works with dataframes and not individual vectors.
#All the data needed to make the plot is typically contained within the dataframe supplied
#source("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/theme_new.R")
source("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/theme_new.R")
theme_set(theme_new()) #Set over all look of graph (theme_classic(), ggthemes - Package with additional ggplot2 themes)

###Example
#p<-ggplot(DRt,aes(x = Year, y = dr)) #Supply data frame and x/y. This only builds the blank plot
#p<-p+geom_point(aes(color = set))#Specifies how you want the data to be represented, Vaious "layers" can be added e.g. lm. 
# Set static color and size for points (col = , size = )
# By adding aes() change points based on another column in the source dataset
#p<-p+coord_cartesian(ylim=c(0,600),xlim=c(1750,2018)) #Zooms in to specific area without "deleting" points
#p<-p+xlim(1750,2015) + ylim(0,1000) #removes points outside of range
#p<-p+labs(title="Discovery Rates", subtitle="Marine Parasites", y="No. of descriptions", x="Year", caption="Caption")#labels
#p<-p+scale_y_continuous(expand = expand_scale(mult = c(0.01,0.01))) #Allow graph to sit flush with x axis
#p

#####################################################################################
#Overall datasets
##Plotted below

#No. of descriptions per year across all datasets. (Figure 1)
fig1 <- ggplot(DRt,aes(x = Year)) +
  geom_point(aes(y = (dr), shape = set), col = "grey50", size = rel(1)) +
  #geom_path(aes(linetype = set), col = "black", size = rel(0.5)) +
  geom_ma(aes(y = (dr), linetype = set), ma_fun = SMA, n = 5, col = "grey50", size = rel(0.5)) +
  #geom_smooth(aes(linetype = set), method = 'gam', formula = y ~ s(x), se = FALSE, colour = 'black', size = 0.7)+
  #geom_line(aes(y = (pred), linetype = set), data = DRt_gam) +
  scale_linetype_manual(values=c('dashed', 'dotted')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Number of species", x="Year", linetype = " ")

fig1

#Model of data

#see Chapt2_GAM1.R
#file.edit("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Chapt2_GAM1.R") 

# Theoretically, we have an issue with outliers; particularly the first point (linneus) 1758.
# Lets correct the data set and remove these
DRtcor <- mutate(DRt, dr = ifelse(Year == "1758", NA, dr))

##ARMA model(p,0,q)
#MA model with q = 5
gam <- gamm((dr) ~ set + s(Year, by = set), data = DRtcor)
gam_MA <- gamm((dr) ~ set + s(Year, by = set), data = DRtcor, correlation = corARMA(form = ~ Year|set, q = 1))

#Lets check the assumptions of the model
summary(gam_MA$gam)  #basic model assessment
gam.check(gam_MA$gam) #confirm convergence
anova.gam(gam_MA$gam)
appraise(gam_MA$gam) #confirm visual assumptions

# Lets plot the final model
dat <- DRtcor %>% 
  #data_grid(Year) %>%
  add_predictions(gam$gam, var = "gam_pred") %>%
  add_predictions(gam_MA$gam, var = "MA_pred")

#Plot
fig1<-ggplot(DRt,aes(x = Year)) +
  #geom_line(aes(y = (dr),linetype = set), col = "grey90", size = rel(0.5)) +
  #geom_ma(aes(y = (dr), linetype = set), ma_fun = SMA, n = 5, col = "grey30", size = rel(0.5)) +
  geom_point(aes(y = (dr), shape = set), col = "grey20", size = rel(1)) +
  #geom_line(aes(y = (gam_pred),linetype = set), data = dat, col = "black", size = rel(0.5)) +
  geom_line(aes(y = (MA_pred),linetype = set), data = dat, col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (AR_pred),linetype = set), data = dat, col = "blue", size = rel(0.5)) +
  #geom_line(aes(y = (ARMA_pred),linetype = set), data = dat, col = "green", size = rel(0.5)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Species descriptions", x="Year", linetype = " ")
fig1

###dual axes
dat2 <- dat %>% 
  #dplyr::select(Year, set, MA_pred) %>% 
  pivot_wider(names_from = set, values_from = c(MA_pred, dr, gam_pred))

summary(dat2)

fig1 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineSpecies)), shape = 2, col = "orangered", size = rel(1)) +
  geom_point(aes(y = (dr_MarineParasites)*5), shape = 1, col = "grey30", size = rel(1)) +
  geom_line(aes(y = (MA_pred_MarineSpecies)),linetype = "dashed", col = "orangered", size = rel(0.8)) +
  geom_line(aes(y = (MA_pred_MarineParasites)*5), col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineSpecies)),linetype = "dashed", col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineParasites)*5), col = "red", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01), sec.axis = sec_axis(trans=~./5, name="Marine Parasites")) +
  labs(subtitle = "(a)", y="Marine non-Parasites", x="Year", linetype = " ")
fig1

#### Cumulative plot with ARIMA forcast
datcum <- dplyr::select(dat2, Year, dr_MarineParasites) %>% 
  mutate(cum_MP = cumsum(ifelse(is.na(dr_MarineParasites), 0, dr_MarineParasites))+dr_MarineParasites*0)

gam_MA <- gamm((cum_MP) ~ s(Year, k = 50), data = datcum, correlation = corARMA(form = ~ Year, q = 1))

appraise(gam_MA$gam)
summary(gam_MA$gam)
gam.check(gam_MA$gam)

Year <- data.frame(Year=(2020:2100))

datcum <- datcum %>% 
  #data_grid(Year) %>%
  add_predictions(gam_MA$gam, var = "MA_pred")

se <- predict(gam_MA$gam, Year, type = "link", se.fit = TRUE)
se <- cbind(Year,fit = se$fit, se.fit = se$se.fit)

fig12 <- ggplot(datcum, aes(x = Year)) +
  geom_point(aes(y = (cum_MP)), shape = 1, col = "grey30", size = rel(1)) +
  #geom_line(aes(y = (cum_MP)), col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (MA_pred)), linetype = "dashed", col = "black", size = rel(0.8)) +
  geom_ribbon(aes(ymin = fit-se.fit, ymax = fit +se.fit), data = se, fill = "grey80") +
  geom_line(aes(x=Year, y = fit), data = se, col = "orangered") +
  
  
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2105), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(subtitle = "", y="Cumulative Descriptions", x="Year", linetype = " ")
fig12


###############################################################################
# Marine parasites as a function of marine species 
qplot(x= (MarineSpecies), data = DR, geom = 'histogram', binwidth = 50)
qplot(x= (MarineParasites), data = DR, geom = 'histogram', binwidth = 1)


fig10 <- ggplot(DR,aes(x = (MarineSpecies), y = (MarineParasites)))+
  geom_point(size = 1)+
  geom_smooth(method = 'poly', se = FALSE, colour = 'grey40', linetype = 'longdash', size = 0.5)+
  #geom_smooth(method = 'gam', formula = y ~ x, se = FALSE, colour = 'darkgrey', linetype = 'dashed', size = 0.5)+
  #geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = TRUE, colour = 'black', linetype = 'solid', size = 0.5)+
  coord_capped_cart(bottom = 'none', left = 'bottom') +
  #scale_x_continuous(limits = c(-20,2010), expand = c(0.0,0.0)) +
  #scale_y_continuous(limits = c(-20,600), expand = c(0.0,0.0)) +
  labs(x="Non-parasitic marine species", y="Marine parasites", linetype = " ")
fig10

#Model the distributions
lm <- gam((MarineParasites) ~ I(MarineSpecies^2), data = DR)
summary(lm)
anova(lm)
gam.check(lm)
appraise(lm)


# r2 = 0.637, p<0.001
gam <- gam(MarineParasites ~ s(MarineSpecies, bs = "cs", k = 150), data = DR, family=gaussian, method = "REML", gamma = 1.4)

anova(gam)
summary(gam) 
gam.check(gam)
appraise(gam)
#y ~ s(x, bs = "cs"), Ref.df = 9, r2 = 0.69.3, p<0.001

#Tests to see if the lm is statistically different than the GAM ... which it is!
anova.gam(lm, gam, test = "Chisq")

#predictions
dat <- DR %>% 
  #data_grid(Year) %>% 
  #add_predictions(lm, var = "lm_pred") %>%
  add_predictions(gam, var = "gam_pred")

se <- predict(gam, DR, type = "link", se.fit = TRUE)

dat <- cbind(dat, se.fit = se$se.fit)

fig10 <- ggplot(DR,aes(x = MarineSpecies, y = MarineParasites))+
  #geom_smooth(method = 'lm', se = FALSE, colour = 'grey40', linetype = 'longdash', size = 0.5)+
  #geom_smooth(method = 'gam', formula = y ~ I(x^2), se = TRUE, colour = 'grey20', linetype = 'dashed', size = 0.5)+
  #geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = TRUE, colour = 'black', linetype = 'solid', size = 0.5)+
  #geom_line(aes(y = (lm_pred)), data = dat, col = "grey20", size = rel(0.5)) +
  geom_ribbon(aes(ymin = gam_pred-se.fit, ymax = gam_pred +se.fit), data = dat, fill = "grey80") +
  geom_point(size = 1, shape = 1)+
  geom_line(aes(y = (gam_pred)), data = dat, col = "grey20", size = rel(0.5)) +
  coord_capped_cart(bottom = 'none', left = 'bottom') +
  scale_x_continuous(limits = c(-20,2025), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(-20,600), expand = c(0.0,0.0)) +
  labs(x="Non-parasitic Marine Species", y="Marine Parasites", linetype = " ")
fig10

#see if ranging across timeperiods might make it interesting. (Lattice?)
#first convert to time periods into data set
#, rep('2000-2017',18)
PeriodFifty<-c(rep('1757-1799', 43), rep('1800-1849',50), rep('1850-1899',50), rep('1900-1949',50), rep('1950-1999',50), rep('2000-2020',19))
PeriodCent<-c(rep('1757-1849', 93), rep('1850-1949',100), rep('1950-2020',69))
DRp <- cbind(PeriodFifty, PeriodCent, DR)
rm(PeriodFifty, PeriodCent)

# good to go, lets draw it out:

fig11 <- ggplot(DRp,aes(x = MarineSpecies, y = MarineParasites))+
  #geom_point(size = 0.5)+
  #facet_grid(.~ PeriodCent)+
  facet_grid(.~ PeriodFifty, labeller = "label_value")+
  #geom_smooth(method = "gam", formula = y ~ x, se = FALSE, colour = 'red', linetype = 'longdash', size = 0.5)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, colour = 'grey10', linetype = 'longdash', size = 0.5)+
  #geom_ribbon(aes(ymin = gam_pred-se.fit, ymax = gam_pred +se.fit), data = dat, fill = "grey80") +
  geom_point(size = 1, shape = 1)+
  #geom_line(aes(y = (gam_pred)), data = dat, col = "grey20", size = rel(0.5)) +
  labs(y = "MarineParasites", x = "MarineSpecies", fill = " ")+
  coord_capped_cart(bottom = 'none', left = 'bottom') +
  scale_y_continuous(limits = c(0,500), expand = expansion(mult = c(0.0,0.0)))+
  scale_x_continuous(limits = c(), expand = expansion(mult = c(0.0,0.0)))+
  labs(x="Non-parasitic Marine Species", y="Marine Parasites", linetype = " ")
fig11
#Test model family
descdist(as.numeric(DRp$MarineSpecies), discrete = TRUE) 
descdist(as.numeric(na.omit(DRp$MarineParasites)), discrete = TRUE) 
Ord_plot(DRp$MarineSpecies)

#Model the distributions

gam <- gam(MarineParasites ~ PeriodFifty + s(MarineSpecies, bs = "cs", by = PeriodFifty), data = DRp,
           family = gaussian, method = 'REML', gamma = 1.4 )
summary(gam)
gam.check(gam)
appraise(gam)
draw(gam)
#y ~ s(x, bs = "cs"), Ref.df = 9, r2 = 0.69.3, p<0.001

dat <- DRp %>% 
  #data_grid(Year) %>% 
  #add_predictions(lm, var = "lm_pred") %>%
  add_predictions(gam, var = "gam_pred")

se <- predict(gam, DRp, type = "link", se.fit = TRUE)
dat <- cbind(dat, se.fit = se$se.fit)

fig11 <- ggplot(DRp,aes(x = MarineSpecies, y = MarineParasites))+
  #geom_point(size = 0.5)+
  #facet_grid(.~ PeriodCent)+
  #facet_grid(.~ PeriodFifty)+
  #geom_smooth(method = "gam", formula = y ~ x, se = FALSE, colour = 'red', linetype = 'longdash', size = 0.5)+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, colour = 'grey10', linetype = 'longdash', size = 0.5)+
  #geom_ribbon(aes(ymin = gam_pred-se.fit, ymax = gam_pred +se.fit), data = dat, fill = "grey80") +
  geom_ribbon(aes(ymin = gam_pred-se.fit, ymax = gam_pred +se.fit), data = dat, fill = "grey80") +
  geom_line(aes(y = (gam_pred)), data = dat, col = "grey20", size = rel(0.5)) +
  geom_point(size = 1, shape = 1)+
  facet_grid(.~ PeriodFifty)+
  labs(y = "MarineParasites", x = "MarineSpecies", fill = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.25))+
  coord_capped_cart(bottom = 'none', left = 'bottom') +
  scale_y_continuous(limits = c(0,550), expand = expansion(mult = c(0.0,0.0)))+
  scale_x_continuous(limits = c(), expand = expansion(mult = c(0.0,0.0)))+
  labs(x="Non-parasitic Marine Species", y="Marine Parasites", linetype = " ")
fig11

#Tests to see if the lm is statistically different than the GAM ... which it is!
anova.gam(lm, gam, test = "Chisq")

#####################################################################################
###Author's per decade
#source("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/author.R")
source("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/author.R")
#file.edit("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/author.R")

#Author plot
#Aut <- dplyr::select(full_set, "Year", MarineSpecies = "AuM", MarineParasites = "AuP", TerrestrialParasites = "AuT")%>%
#  gather('MarineSpecies','TerrestrialParasites', 'MarineParasites', key = set, value = dr)
Aut <- dplyr::select(full_set, "Year", MarineSpecies = "AuM", MarineParasites = "AuP")%>%
  gather('MarineSpecies', 'MarineParasites', key = set, value = dr)
Aut$set<- as.factor(Aut$set)

#see Chapt2_GAM1.R
#file.edit("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/Chapt2_GAM1.R") 
##ARMA model(p,0,q)
#MA model with q = 1
gam_MA <- gamm((dr) ~ set + s(Year, by = set), data = Aut, correlation = corARMA(form = ~ Year|set, q = 1))

#Lets check the assumptions of the model
summary(gam_MA$gam)  #basic model assessment
gam.check(gam_MA$gam) #confirm convergence
anova.gam(gam_MA$gam)
appraise(gam_MA$gam) #confirm visual assumptions

# Lets plot the final model
dat <- Aut %>% 
  #data_grid(Year) %>%
  #add_predictions(gam$gam, var = "gam_pred") %>%
  add_predictions(gam_MA$gam, var = "MA_pred")

#Plot
fig2<-ggplot(Aut,aes(x = Year)) +
  #geom_line(aes(y = (dr),linetype = set), col = "grey90", size = rel(0.5)) +
  #geom_ma(aes(y = (dr), linetype = set), ma_fun = SMA, n = 5, col = "grey30", size = rel(0.5)) +
  geom_point(aes(y = (dr), shape = set, col = set), size = rel(1)) +
  #geom_line(aes(y = (gam_pred),linetype = set), data = dat, col = "black", size = rel(0.5)) +
  geom_line(aes(y = (MA_pred),linetype = set, col = set), data = dat, size = rel(0.8)) +
  #geom_line(aes(y = (AR_pred),linetype = set), data = dat, col = "blue", size = rel(0.5)) +
  #geom_line(aes(y = (ARMA_pred),linetype = set), data = dat, col = "green", size = rel(0.5)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  scale_color_manual(values=c('grey30', 'orangered')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Authors", x="Year", linetype = " ")
fig2

###dual axes
dat2 <- dat %>% 
  #dplyr::select(Year, set, MA_pred) %>% 
  pivot_wider(names_from = set, values_from = c(MA_pred, dr, gam_pred))

summary(dat2)

fig2 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineSpecies)), shape = 2, col = "orangered", size = rel(1)) +
  geom_point(aes(y = (dr_MarineParasites)*5), shape = 1, col = "grey30", size = rel(1)) +
  geom_line(aes(y = (MA_pred_MarineSpecies)),linetype = "dashed", col = "orangered", size = rel(0.8)) +
  geom_line(aes(y = (MA_pred_MarineParasites)*5), col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineSpecies)),linetype = "dashed", col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineParasites)*5), col = "red", size = rel(0.8)) +
  
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01), sec.axis = sec_axis(trans=~./5, name="Marine Parasites")) +
  labs(subtitle = "(b)", y="Marine non-Parasites", x="Year", linetype = " ")
fig2

###################################################################################
#Difference plot
#Difft <- dplyr::select(full_set, "Year", MarineSpecies = "DiffM", MarineParasites = "DiffP", TerrestrialParasites = "DiffT") %>%
#  gather('MarineSpecies','TerrestrialParasites', 'MarineParasites', key = set, value = dr)

Difft <- dplyr::select(full_set, "Year", MarineSpecies = "DiffM", MarineParasites = "DiffP") %>%
  gather('MarineSpecies', 'MarineParasites', key = set, value = dr)
Difft$set <- as.factor(Difft$set)

# Theoretically, we have an issue with outliers; particularly the first point (linneus) 1758.
# Lets correct the data set and remove these
Difft <- mutate(Difft, dr = ifelse(Year == "1758", NA, dr))


ggplot(Difft, aes(y = dr)) +
  geom_boxplot(aes(set)) +
  geom_point(aes(set))

ggplot(Difft, aes(x = log(dr))) +
  geom_histogram()

#Run ARMA

#Models of data
#see Chapt2_GAM1.R
#file.edit("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/Chapt2_GAM1.R") 
gam <- gamm((dr) ~ set + s(Year, by = set), data = Difft)
##ARMA model(p,0,q)
#MA model with p = 1
gam_AR <- gamm((dr) ~ set + s(Year, by = set), data = Difft, correlation = corARMA(form = ~ Year|set, p = 1))

#Lets check the assumptions of the model
summary(gam$gam)  #basic model assessment
gam.check(gam_AR$gam) #confirm convergence
anova.gam(gam_AR$gam)
appraise(gam_AR$gam) #confirm visual assumptions

# Lets plot the final model
dat <- Difft %>% 
  #data_grid(Year) %>%
  add_predictions(gam$gam, var = "gam_pred")%>%
  add_predictions(gam_AR$gam, var = "AR_pred")
  #add_predictions(lm, var = "lm_pred")

fig3 <- ggplot(Difft, aes(x = Year, y = dr)) +
  #geom_point(aes(shape = set), col = "darkgrey") +
  #geom_line(aes(linetype = set)) +
  geom_ma(aes(linetype = set), ma_fun = SMA, n = 5, col = "grey20", size = rel(0.5)) +
  #geom_line(aes(y = (gam_pred),linetype = set), data = dat, col = "red", size = rel(0.5)) +
  #geom_line(aes(y = (gam_pred),linetype = set), data = dat, col = "red", size = rel(0.5)) +
  geom_line(aes(y = (AR_pred),linetype = set), data = dat, col = "black", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.0,0.0)) +
  labs(y="Descriptions per author", x="Year", linetype = " ")
fig3

#try draw a difference from mean plot?
mean(full_set$DiffM) #6.310023
mean(full_set$DiffP, na.rm = TRUE) #3.119006

full_set <- full_set %>% mutate( MeanM = DiffM-6.310023) %>% mutate( MeanP = DiffP-3.119006)

Diffmean <- dplyr::select(full_set, "Year", MarineSpecies = "MeanM", MarineParasites = "MeanP") %>%
  gather('MarineSpecies', 'MarineParasites', key = set, value = dr)

# Theoretically, we have an issue with outliers; particularly the first point (linneus) 1758.
# Lets correct the data set and remove these
Diffmean <- mutate(Diffmean, dr = ifelse(Year == "1758", NA, dr))

fig3 <- ggplot(Diffmean, aes(x = Year, y = dr)) +
  #geom_point(aes(shape = set, col = set)) +
  geom_col(aes(col = set)) +
  geom_line(aes(y = (AR_pred_MarineSpecies)-6.310023),linetype = "dashed", col = "orangered", data = dat2, size = rel(0.8)) +
  geom_line(aes(y = (AR_pred_MarineParasites)-3.119006), data = dat2, col = "black", size = rel(0.8)) +
  #geom_line(aes(linetype = set)) +
  #geom_ma(aes(linetype = set, col = set), ma_fun = SMA, n = 5, size = rel(0.5)) +
  #geom_smooth(aes(linetype = set), method = 'gam', formula = y ~ s(x), se = TRUE, colour = 'black', size = 0.8)+
  scale_linetype_manual(values=c('solid', 'dashed')) +
  scale_color_manual(values=c('grey30', 'orangered')) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.position = 'none') +
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(-6,26), expand = c(0.0,0.0)) +
  labs(y="Difference from the mean descriptions per author", x="Year", linetype = " ")

fig3

###dual axes
dat2 <- dat %>% 
  #dplyr::select(Year, set, MA_pred) %>% 
  pivot_wider(names_from = set, values_from = c(AR_pred, dr))

summary(dat2)

fig3 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineSpecies)), shape = 2, col = "orangered", size = rel(1)) +
  geom_point(aes(y = (dr_MarineParasites)*2), shape = 1, col = "grey30", size = rel(1)) +
  geom_line(aes(y = (AR_pred_MarineSpecies)),linetype = "dashed", col = "orangered", size = rel(0.8)) +
  geom_line(aes(y = (AR_pred_MarineParasites)*2), col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineSpecies)),linetype = "dashed", col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineParasites)*2), col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (lm_pred_MarineParasites)*2), col = "blue", size = rel(0.8)) +
  #geom_line(aes(y = (seg_pred_MarineParasites)*2), col = "orange", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,35), expand = c(0.01,0.01), sec.axis = sec_axis(trans=~./2, name="Marine Parasites")) +
  labs(subtitle = "(c)", y="Marine non-Parasites", x="Year", linetype = " ")
fig3

###Break point analysis
#pull out marine parasites.
dat3 <- dplyr::select(dat2, Year, dr_MarineParasites, AR_pred_MarineParasites)
lm <- lm(dr_MarineParasites~Year, data = dat3)
seg <- segmented(lm, seg.Z = ~ Year, psi = NA)

dat <- Difft %>% 
  #data_grid(Year) %>%
  add_predictions(gam$gam, var = "gam_pred")%>%
  add_predictions(gam_AR$gam, var = "AR_pred") 
  #add_predictions(lm, var = "lm_pred") %>% 
  #add_predictions(seg, var = "seg_pred")

dat2 <- dat %>% 
  #dplyr::select(Year, set, MA_pred) %>% 
  pivot_wider(names_from = set, values_from = c(AR_pred, dr, gam_pred))

fig30 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineSpecies)), shape = 2, col = "orangered", size = rel(1)) +
  geom_line(aes(y = (AR_pred_MarineSpecies)),linetype = "dashed", col = "orangered", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineSpecies)),linetype = "dashed", col = "red", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,35), expand = c(0.01,0.01)) +
  labs(subtitle = "(c)", y="Marine non-Parasites", x="Year", linetype = " ")
fig30

fig31 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineParasites)*2), shape = 1, col = "grey30", size = rel(1)) +
  geom_line(aes(y = (AR_pred_MarineParasites)*2), col = "black", size = rel(0.8)) +
  #geom_line(aes(y = (gam_pred_MarineParasites)*2), col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (lm_pred_MarineParasites)*2), col = "blue", size = rel(0.8)) +
  #geom_line(aes(y = (seg_pred_MarineParasites)*2), col = "orange", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,35), expand = c(0.01,0.01)) +
  labs(subtitle = "(c)", y="Marine Parasites", x="Year", linetype = " ")
fig31

### 5 year and 10 year anomoly plot

Periodten<-c(rep('1750', 3),rep('1760', 10),rep('1770', 10),rep('1780', 10), rep('1790',10), rep('1800',10), 
             rep('1810',10),rep('1820',10),rep('1830',10),rep('1840',10),rep('1850',10),rep('1860',10),
             rep('1870',10),rep('1880',10),rep('1890',10),rep('1900',10),rep('1910',10),rep('1920',10),
             rep('1930',10),rep('1940',10),rep('1950',10),rep('1960',10),rep('1970',10),rep('1980',10),
             rep('1990',10),rep('2000',10),rep('2010',9))
Periodfive<-c(rep('1755',3),rep('1760',5),rep('1765',5),rep('1770',5),rep('1775',5),rep('1780',5),
              rep('1785',5),rep('1790',5),rep('1795',5),rep('1800',5),rep('1805',5), 
              rep('1810',5),rep('1815',5),rep('1820',5),rep('1825',5),rep('1830',5),rep('1835',5),
              rep('1840',5),rep('1845',5),rep('1850',5),rep('1855',5),rep('1860',5),rep('1865',5),
              rep('1870',5),rep('1875',5),rep('1880',5),rep('1885',5),rep('1890',5),rep('1895',5),
              rep('1900',5),rep('1905',5),rep('1910',5),rep('1915',5),rep('1920',5),rep('1925',5),
              rep('1930',5),rep('1935',5),rep('1940',5),rep('1945',5),rep('1950',5),rep('1955',5),
              rep('1960',5),rep('1965',5),rep('1970',5),rep('1975',5),rep('1980',5),rep('1985',5),
              rep('1990',5),rep('1995',5),rep('2000',5),rep('2005',5),rep('2010',5),rep('2015',4))

decdat <- dplyr::select(dat2, Year, dr_MarineParasites) %>%
  cbind(., Periodten, Periodfive)

ten <- decdat %>%
  group_by(Periodten) %>%
  summarize(meanTen = mean(dr_MarineParasites, na.rm = TRUE))

five <- decdat %>%
  group_by(Periodfive) %>%
  summarize(meanFive = mean(dr_MarineParasites, na.rm = TRUE))

ten$Periodten <- as.numeric(as.character(ten$Periodten))
five$Periodfive <- as.numeric(as.character(five$Periodfive))

fig32 <- ggplot(dat2, aes(x = Year)) +
  
  geom_point(aes(y = (dr_MarineParasites)*2), shape = 1, col = "grey30", size = rel(1)) +
  geom_line(aes(y = (AR_pred_MarineParasites)*2), col = "black", size = rel(0.8)) +
  geom_point(aes(x=Periodten, y = (meanTen)*2), data = ten, shape = 3, col = "darkblue", size = rel(3.0)) +
  geom_point(aes(x=Periodfive, y = (meanFive)*2), data = five, shape = 4, col = "blue", size = rel(2.0)) +
  #geom_line(aes(y = (gam_pred_MarineParasites)*2), col = "red", size = rel(0.8)) +
  #geom_line(aes(y = (lm_pred_MarineParasites)*2), col = "blue", size = rel(0.8)) +
  #geom_line(aes(y = (seg_pred_MarineParasites)*2), col = "orange", size = rel(0.8)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(0,35), expand = c(0.01,0.01)) +
  labs(subtitle = "(c)", y="Marine Parasites", x="Year", linetype = " ")
fig32

#anomoly plot

fig30 <- ggplot(dat2, aes(x = Year)) +
  
  geom_col(aes(x=Periodten, y=(meanTen)-3.119006), data = ten) +
  #geom_point(aes(x = Year, y= (dr_MarineParasites)-3.119006), shape = 1) +
  
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(subtitle = "(c)", y="Marine Parasites", x="Year", linetype = " ")
fig30

fig31 <- ggplot(dat2, aes(x = Year)) +
  
  geom_col(aes(x=Periodfive, y=(meanFive)-3.119006), data = five) +
  #geom_point(aes(x = Year, y= (dr_MarineParasites)-3.119006), shape = 1) +
  
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none', right = "none") +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.0,0.0)) +
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(subtitle = "(c)", y="Marine Parasites", x="Year", linetype = " ")
fig31


  
#####################################################################################
#Parasite discovery by taxon and functional groups dataset:

#first split them into groups
#MarPara <- read_csv("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/Worms_psite2.0.csv")%>%
#  dplyr::select(.,c("valid_AphiaID","valid_name","valid_authority","status","phylum","class","Year"))

#source("C:/Users/tmor201/Google Drive/University/PhD NZ/Data_and_code/author.R")
#source("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/author.R")
#file.edit("C:/Users/mooseface/Google Drive/University/PhD NZ/Data_and_code/author.R")

#see<- count(MarPara, phylum, class) 
#see$phylum
#see$class

#Taxon/functional (phylum) Group
funset <- MarPara %>%
  mutate(phylum = fct_collapse(phylum,
                               Helminths = c('Platyhelminthes', 'Acanthocephala', 'Annelida', 'Nematoda'),
                               Arthropoda = c('Arthropoda'),
                               Mollusca = c('Mollusca'),
                               MinorGroups = c('Chordata','Cnidaria','Dicyemida','Orthonectida','Tardigrada')
  ))
MarPara <- cbind(MarPara, group = funset$phylum)

#endo/ecto (class) Habitat 
funset <- MarPara %>%
  mutate(class = fct_collapse(class,
                               endo = c('Eoacanthocephala', 'Palaeacanthocephala', 'Cestoda', 'Chromadorea', 'Enoplea', 
                                        'Ichthyostraca', 'Trematoda', 'Petromyzonti', 'Actinopterygii', 
                                        'Rhombozoa', 'Myxozoa','Nematoda incertae sedis', 'Fecampiida', 'Orthonectida'),
                               ecto = c('Clitellata', 'Arachnida', 'Hexanauplia', 'Insecta',	'Malacostraca', 
                                         'Gastropoda', 'Monogenea', 'Heterotardigrada', 'Ostracoda',
                                        'Petromyzonti')
                               
  ))

MarPara <- cbind(MarPara, habitat = funset$class)
rm(funset)

#descriptions
funsetD <- dplyr::select(MarPara, valid_name, Year, group) %>% 
  distinct() %>% 
  count(Year, group, name = "Desc") %>%
  filter(Year>= 1758) %>%
  group_by(group) %>%
  mutate(csnd = cumsum(Desc)) # allow for cumulative graphic

funsetA <- dplyr::select(MarPara, firstAuthor, Year, group) %>% 
  distinct() %>% 
  count(Year, group, name = "Auth") %>%
  filter(Year>= 1758) %>%
  group_by(group) %>%
  mutate(csna = cumsum(Auth)) # allow for cumulative graphic

funsetg <- left_join(funsetD, funsetA, by = c("Year", "group"))
funsetg$group <- reorder(funsetg$group, c("Arthropods", "Helminths", "Molluscs", "MinorGroups"))


fig4 <- ggplot(funsetg,aes(x = Year, y = csnd)) +
  #geom_point(aes(shape = group), col = "grey70", size = rel(0.8)) +
  #geom_path(aes(linetype = group), col = "black", size = rel(0.5)) +
  geom_ma(aes(linetype = group), ma_fun = SMA, n = 5, col = "black", size = rel(0.5)) +
  #geom_spline(aes(linetype = group), all.knots = TRUE, col = "black", size = rel(0.5)) +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed', 'dotdash')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'bottom') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Cumulative descriptions", x="Year", linetype = " ")
fig4

fig5 <- ggplot(funsetg,aes(x = Year, y = Auth)) +
  #geom_point(aes(shape = group), col = "grey70", size = rel(0.8)) +
  #geom_path(aes(linetype = group), col = "black", size = rel(0.5)) +
  geom_ma(aes(linetype = group), ma_fun = SMA, n = 5, col = "black", size = rel(0.5)) +
  #geom_spline(aes(linetype = group), all.knots = TRUE, col = "black", size = rel(0.5)) +
  scale_linetype_manual(values=c('dotted', 'solid', 'dashed', 'dotdash')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Authors", x="Year", linetype = " ")
fig5


#Habitat
funsetD <- dplyr::select(MarPara, valid_name, Year, habitat) %>% 
  distinct() %>% 
  count(Year, habitat, name = "Desc") %>%
  filter(Year>= 1758) %>%
  group_by(habitat) %>%
  mutate(csnd = cumsum(Desc)) # allow for cumulative graphic

funsetA <- dplyr::select(MarPara, firstAuthor, Year, habitat) %>% 
  distinct() %>% 
  count(Year, habitat, name = "Auth") %>%
  filter(Year>= 1758) %>%
  group_by(habitat) %>%
  mutate(csna = cumsum(Auth)) # allow for cumulative graphic

funseth <- left_join(funsetD, funsetA, by = c("Year", "habitat"))
rm(funsetD, funsetA)

fig6 <- ggplot(funseth,aes(x = Year, y = csnd)) +
  #geom_point(aes(shape = habitat), col = "grey70", size = rel(0.8)) +
  #geom_path(aes(linetype = habitat), col = "black", size = rel(0.5)) +
  geom_ma(aes(linetype = habitat), ma_fun = SMA, n = 5, col = "black", size = rel(0.5)) +
  #geom_spline(aes(linetype = habitat), all.knots = TRUE, col = "black", size = rel(0.5)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Cumulative descriptions", x="Year", linetype = " ")
fig6

fig7 <- ggplot(funseth,aes(x = Year, y = Auth)) +
  #geom_point(aes(shape = habitat), col = "grey70", size = rel(0.8)) +
  #geom_path(aes(linetype = habitat), col = "black", size = rel(0.5)) +
  geom_ma(aes(linetype = habitat), ma_fun = SMA, n = 5, col = "black", size = rel(0.5)) +
  #geom_spline(aes(linetype = habitat), all.knots = TRUE, col = "black", size = rel(0.5)) +
  scale_linetype_manual(values=c('solid', 'dashed')) +
  theme(legend.position = 'bottom') +  
  coord_capped_cart(bottom = 'none', left = 'none') +
  scale_x_continuous(limits = c(1750,2020), expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(), expand = c(0.01,0.01)) +
  labs(y="Authors", x="Year", linetype = " ")
fig7



count(MarPara, habitat)

#Let see if there are statistical differences
lm <- gam(DR$MarineParasites ~ DR$MarineSpecies)
summary(lm)
#y=0.17x-10.551173, r2 = 0.625, p<0.001
gam <- gam(DR$MarineParasites ~ s(DR$MarineSpecies, bs = "cs"))
summary(gam)
#y ~ s(x, bs = "cs"), Ref.df = 9, r2 = 0.635, p<0.001

#Tests to see if the lm is statistically different than the GAM ... which it isnt.
anova.gam(lm, gam)



#ready to plot funset




#Sort out panels

ggarrange(fig1, fig2, fig3, ncol = 1, align = "v", heights = c(1,1,1), common.legend = FALSE)

fig8 <- ggarrange(fig4, fig5, ncol = 2, align = "h", 
          heights = c(1,1), common.legend = TRUE, legend = "bottom")

fig9 <- ggarrange(fig6, fig7, ncol = 2, align = "h", 
          heights = c(1,1,1,1), common.legend = TRUE, legend = "bottom")

ggarrange(fig8, fig9, nrow = 2)

ggarrange(fig30, fig31, ncol = 2, align = "h", heights = c(1,1), common.legend = FALSE)

#####Saving plot
ggsave('Chapt2_anomoly_wopoint.png', 
       plot = last_plot(), # last_plot() or give ggplot object name as in myPlot,
       width = 16, height = 12, 
       units = 'cm', # other options c("in", "cm", "mm"),
       type = 'cairo-png', #Specific to smoothing the graph
       dpi = 300)
