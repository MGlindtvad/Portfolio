library(ggplot2)
library(dplyr)
library(readr)
library(draw)

##Loading the data from Kaggel
dwguide <- read_csv("dwguide.csv")
allscript <- read_csv("all-scripts.csv")
allepisodes <- read_csv("all-detailsepisodes.csv")
imdb_details <- read_csv("imdb_details.csv")

##Understanding the data - see "DoctorWho_dataset_schema.pdf" for connections and possible keys to use for joins
str(dwguide, give.att = FALSE)
str(allscript, give.att = FALSE)
str(allepisodes, give.att = FALSE)
str(imdb_details, give.att = FALSE)

#left join dwguide with allepisodes
dw_episode <- dwguide %>% select(!(cast:summary)) %>%
          left_join(allepisodes, by = "title", suffix = c(".guide",".episode")) 
          
##Number of episode per doctor
dw_episode %>% group_by(doctorid) %>%
               count()
##There is 716 NA's which means we must find a way to determin the doctor of an episode in an other way.

##Which doctor has the maximum, minimum and mean of episodes

##To be continued

