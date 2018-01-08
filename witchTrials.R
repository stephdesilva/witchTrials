# Every witchunter requires a toolkit
# Shadwell has http://www.bbc.co.uk/programmes/p02fxsvg/p02k41dx
# We don't have a pendulum of rigteousness. 
# We have the packages of visualisation and data discovery
install.packages("naniar")
install.packages("skimr")
install.packages("visdat")
#install.packages("tidyverse")

library(naniar)
library(skimr)
library(tidyverse)
library(visdat)

url <- "https://raw.githubusercontent.com/JakeRuss/witch-trials/master/data/trials.csv"

db <- read.csv(url)

db$decade <- as.numeric(db$decade)
db$country <- db$gadm.adm0
db <- db[,-9]

summary(db)
skim(db)
glimpse(db)


# note structure of database, numerics, factors, substantial NAs.

vis_dat(db, palette = "cb_safe")+
  theme_light()
vis_miss(db, cluster = TRUE, sort_miss = TRUE)+
  theme_light() +
  theme(legend.position="bottom")+
  theme(
    axis.text.x=element_text(angle=-45,hjust=1, size = 10))# takes awhile to process
gg_miss_var(db) + 
  theme_light()

# Let's see what the relationship between trials and deaths were, 
# allowing for the fact that there is missing data

ggplot(db, 
       aes(x = tried, y = deaths)) + 
      xlab("People tried")+
      ylab("Deaths")+
      geom_miss_point() + 
      theme_light()+
      theme(legend.position="bottom")+
      theme(
      axis.text.x=element_text(angle=45,hjust=1, size = 10))+
      ggtitle("Number of trials compared to number of deaths")

## Deaths over time

bydecade <- db[,c(2,4,5)] %>% 
      group_by(decade) %>% 
      summarise(deathsDecade = sum(deaths, na.rm = TRUE), 
                triedDecade = sum(tried, na.rm = TRUE)) 

ggplot(bydecade, 
       aes(x = decade, y = deathsDecade))+ 
  xlab("Decade")+
  ylab("Deaths")+
  geom_line() + 
  theme_light()+
  theme(legend.position="bottom")+
  theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 10))+
  ggtitle("Number of deaths over time")




# Is missing data a function of time?

ggplot(db, 
       aes(x = decade, y = deaths))+ 
    xlab("Decade")+
    ylab("Deaths")+
    geom_miss_point() + 
    theme_light()+
    theme(legend.position="bottom")+
    theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 10))+
    ggtitle("Number of deaths over time")


# Geography?

ggplot(db, 
       aes(x = country, y = deaths))+ 
  xlab("Country")+
  ylab("Deaths")+
  geom_miss_point() + 
  theme_light()+
  theme(legend.position="bottom")+
  theme(
  axis.text.x=element_text(angle=45,hjust=1, size = 10))+
  ggtitle("Number of deaths by country")

# Can we start to combine some things?

ggplot(db)+ 
  xlab("Decade")+
  ylab("Deaths")+
  geom_point(aes(x = decade, y = deaths, colour = country)) +
  theme_light()+
  theme(legend.position="bottom")+
  theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 10))+
  ggtitle("Number of deaths over time")

#How many people were acquitted?

db <- mutate(db, pcntDeaths = deaths/tried)

ggplot(db, aes(x = decade, y = pcntDeaths, colour = country))+ 
  xlab("Decade")+
  ylab("% Deaths of those tried")+
  #geom_point() +
  geom_jitter()+
  theme_light()+
  theme(legend.position="bottom")+
  theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 10))+
  ggtitle("% of deaths of those tried over time")


# Geographic missingness # yeah nah

ggplot(db, 
       aes(x = gadm.adm0, y = country))+ 
  xlab("Country")+
  ylab("Region")+
  geom_miss_point() + 
  theme_light()+
  theme(
    axis.text.x=element_text(angle=45,hjust=1, size = 10))+
  ggtitle("Country and region missingness")


