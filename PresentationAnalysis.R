########################################## STAT 4310 Final Project ##########################################

library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)

setwd("C:/Users/Katherine/Desktop/F17/STAT4310")

calls <- read_csv("911_Calls_for_Service.csv")
guns <- read_csv("Gun_Offenders.csv")

calls_final5<-calls %>% 
  mutate(datetime=mdy_hms(callDateTime, tz=Sys.timezone())) %>%
  filter(!is.na(callDateTime) & !is.na(location)) %>% #remove na's of datetime & location
  separate(location, c('lat', 'long'), sep=",") %>% #separating location
  mutate(lat = as.numeric(str_sub(lat,2,10)), long = as.numeric(str_sub(long,1,10))) %>% 
  mutate(lat = round(lat, digits=5), long = round(long, digits=5)) %>%
  select(datetime, priority, district, incidentLocation, long, lat, description) %>%
  filter(year(datetime)>=2015) #only 2015-2017 


guns_finals5<-guns %>% #CrimeDate & CrimeTime are characters
  mutate(datetime=mdy_hms(created_date, tz=Sys.timezone())) %>%
  filter(!is.na(datetime) & !is.na(`Location 1`), city=="Baltimore") %>% #remove na's of datetime & location
  mutate(lat = round(Latitude, digits=5), long = round(Longitude, digits=5)) %>%
  select(datetime, full_address, district, neighborhood, long, lat, Date_Of_Birth, sex, race) %>% #Inside/Outside not found
  filter(year(datetime)>=2015)

list <-unique(calls$description)


calls_arm <- calls_final5 %>% 
  filter (description == "ROBBERY ARMED" | description == "ARMED PERSON" |
            description == "ARMED PERSON/POS"| description == "POSS ARMED")

calls_guns <- calls_final5 %>% 
  filter (description == "DISCHRG FIREARM" |description == "RECOVERED GUN" | description == "BY SHOOTING AT" )

calls_knife <- calls_final5 %>%
  filter (description == "WITH KNIFE" | description == "KNIFE" |
             description == "W/KNIFE" | description == "POSS/KNIFE" | description == "KNIFE/POSS" | 
             description == "*KNIFE")


balt <- get_map("Baltimore", zoom =12, maptype = "roadmap")
ggmap(balt)+
  labs(title="Gun Offenders by Location and Race")+
  stat_density_2d(data=calls_guns, aes(x= long, y=lat, fill=..level.., alpha=250), geom = "polygon", bins=12, h=.01)+
  scale_alpha_continuous(guide = FALSE)+
  geom_point(data=guns_finals5, aes(x= long, y=lat), size=0.5)

ggmap(balt)+
  stat_density_2d(data=calls_guns, aes(x= long, y=lat, fill=..level.., alpha=..level..), geom = "polygon", bins=12, h=.01)+
  scale_alpha_continuous(guide = FALSE)+
  stat_density_2d(data=guns_finals5, aes(x= long, y=lat, colour= "Blue", alpha=200), bins=10, h=.01, size =.8)

unique(calls_final5$priority)

hml<-calls_final5 %>%
  filter(priority=="High" | priority=="Medium" | priority=="Low")
ggmap(balt) + 
  stat_density_2d(data=hml, aes(x= long, y=lat, colour = hml$priority, alpha=250), bins=10, h=.01)

ggmap(balt) + 
  stat_density_2d(data=high, aes(x= long, y=lat, colour = "Red", alpha=..level..), bins=12, h=.01)+
  stat_density_2d(data=med, aes(x= long, y=lat, colour = "Orange", alpha=..level..), bins=12, h=.01)

  
calls_final5 %>%
  mutate(description = toupper(description)) %>%
  filter (description == "HANDGUN VIOLATIO") %>%
  group_by(description) %>%
  summarize(count = n())

calls_weap <- calls_final5 %>%
  filter (description == "ROBBERY ARMED" | description == "ARMED PERSON" | description == "DISCHRG FIREARM" |
            description == "ARMED PERSON/POS"| description == "POSS ARMED"| description == "RECOVERED GUN" |
            description == "BY SHOOTING AT" | description == "SHOOTING" | description == "HANDGUN VIOLATIO" |
            description == "WITH KNIFE" | 
            description == "W/KNIFE" | description == "POSS/KNIFE" | description == "KNIFE/POSS" | 
            description == "*KNIFE") %>%
  mutate (description = ifelse(description == "ROBBERY ARMED" | description == "ARMED PERSON" | 
                                 description == "ARMED PERSON/POS"| description == "POSS ARMED","ARMED",
                               ifelse (description == "DISCHRG FIREARM" |description == "RECOVERED GUN" | 
                                         description == "BY SHOOTING AT" | description == "SHOOTING" | description == "HANDGUN VIOLATIO","GUN",
                                ifelse (description == "WITH KNIFE" | description == "KNIFE" |
                                          description == "W/KNIFE" | description == "POSS/KNIFE" | description == "KNIFE/POSS" | 
                                          description == "*KNIFE", "KNIFE", "OTHER")))) %>%
  arrange(desc(description))

ggmap(balt) + 
  stat_density_2d(data=calls_weap, aes(x= long, y=lat, colour = calls_weap$description, alpha=..level..), geom ="polygon", bins=15, h=.01, size=.8)

ggmap(balt) + 
  stat_density_2d(data=calls_weap, aes(x= long, y=lat, colour = description, alpha=250), bins=10, h=.01, size=.75)

ggmap(balt) + 
  stat_density_2d(data=calls_guns, aes(x= long, y=lat,colour="GUNS", alpha=250), bins=10, h=.01, size=.8)+
  stat_density_2d(data=calls_knife, aes(x= long, y=lat,colour="KNIFE", alpha=250), bins=10, h=.01, size=.8)+
  stat_density_2d(data=calls_arm, aes(x= long, y=lat,colour="ARMED", alpha=250), bins=10, h=.01, size=.8)


ggmap(balt) + 
  stat_density_2d(data=calls_guns, aes(x= long, y=lat, fill=..level..,alpha=..level..),  geom ="polygon", bins=12, h=.01)+
  scale_alpha_continuous(guide = FALSE)+
  labs (title = "Calls for Incidents Involving Guns")+
  xlab("Longitude") + ylab("Latitude")
  
ggmap(balt) +
  stat_density_2d(data=calls_knife, aes(x= long, y=lat, fill=..level..,alpha=..level..),  geom ="polygon", bins=12, h=.01)+
  scale_alpha_continuous(guide = FALSE)+
  labs (title = "Calls for Incidents Involving Knives")+
  xlab("Longitude") + ylab("Latitude")

ggmap(balt) +
  stat_density_2d(data=calls_arm, aes(x= long, y=lat,  fill=..level..,alpha=..level..),  geom ="polygon",bins=10, h=.01) +
  scale_alpha_continuous(guide = FALSE)+
  labs (title = "Calls for Armed Incidents")+
  xlab("Longitude") + ylab("Latitude")

  


 # takes to long/
# test <-calls_final5 %>%
#   na.omit()
# ggmap(balt)+
#   geom_point(data=test, aes(x= long, y=lat, colour=district), size=0.5)
# 
# ggmap(balt)+
#   geom_point(data=test, aes(x= long, y=lat, colour=priority), size=0.5)



