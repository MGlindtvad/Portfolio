library(ggplot2)
library(dplyr)
library(readr)
library(draw)
library(rlang)
library(stringr)

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
  left_join(allepisodes, by = "title", suffix = c(".guide",".episode"))%>% 
  arrange(episodenbr)

##Number of episode per doctor
dw_episode %>% group_by(doctorid) %>%
  count()

##There is 716 NA's which means we must find a way to determin the doctor of an episode in an other way.

#Finding The doctors in dwguide
dwguide$cast

#The way to find the Doctors are in the variable cast and that variable is a long string containing 
Doctor <- data.frame(episodenbr = 1:nrow(dw_episode),DoctorText = rep("No Doctor", nrow(dw_episode)))

for(i in dw_episode$episodenbr){Doctor$DoctorText[i] <- unlist(strsplit(dwguide$cast[i], split = ","))[2]}

Doctor$DoctorText <- str_replace_all(str_replace_all(Doctor$DoctorText, "[^[:alnum:]]", ""), "name","")

##Checking to see that only the Doctors was 
unique(Doctor$DoctorText)

##One is just call Dr. Who and we want to know how many episodes has this as a cast
Doctor %>% filter(DoctorText == "DrWho") 

##Which doctor has the maximum, minimum and mean of episodes
##To be continued
  
  