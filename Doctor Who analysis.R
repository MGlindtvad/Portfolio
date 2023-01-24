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
dw_episode <- dwguide %>% select(!(crew:summary)) %>%
  left_join(allepisodes, by = "title", suffix = c(".guide",".episode"))%>% 
  arrange(episodenbr)

##Number of episode per doctor
dw_episode %>% group_by(doctorid) %>%
  count()

##There is 716 NA's which means we must find a way to determin the doctor of an episode in an other way.

#Finding The doctors in dwguide
dwguide$cast

#The way to find the Doctors are in the variable cast and that variable is a long string containing 
Doctor <- data.frame(episodenbr = 1:nrow(dw_episode),DoctorActor = rep("No Doctor", nrow(dw_episode)))

#Arranging dwguide, so we can use episodenbr in a for loop and letter join 
#together with the new Doctor table 
dwguide <- dwguide %>% 
  arrange(episodenbr)

#Staring with splitting the cast string by at "," and only keeping the 2 output 
#that contains the name of the actor who plays the doctor
for(i in dw_episode$episodenbr){Doctor$DoctorActor[i] <- unlist(strsplit(dwguide$cast[i], split = ","))[2]}

#Removing the part op the string that says "name" and all the unwanted characters 
Doctor$DoctorActor <- str_replace_all(str_replace_all(Doctor$DoctorActor, "[^[:alnum:]]", ""), "name","")

#Checking to see that only the Doctors was 
unique(Doctor$DoctorActor)

Doctor$DoctorActor <- str_replace_all(Doctor$DoctorActor, "creditonly", "")

#Adding space between first and last name of the actors
Doctor$DoctorActor <- gsub("([a-z])([A-Z])", "\\1 \\2", Doctor$DoctorActor)

#Saving the unique Actors name in DoctorActors_unique 
DoctorActors_unique <- unique(Doctor$DoctorActor)
DoctorActors_unique

#One is just call Dr. Who and we want to know how many episodes has this as a cast
outlier <- Doctor %>% filter(DoctorActor == "Dr Who")  

#Checking the title and the cast to see if the text was divide correct for the outlier
dwguide %>% filter(episodenbr == outlier$episodenbr)%>%
  select(title,cast) 

#Numbering the doctors by using the rownumber and giving the outlier the number 0
#We can use this method because of the arrange we did earlier that ordered the episodes 
Outlier_row <- which(DoctorActors_unique == "Dr Who")

DoctorActors_unique <- data.frame(Actor = DoctorActors_unique) %>% 
  mutate(nr = which(DoctorActors_unique == DoctorActors_unique))%>%  
  mutate(doctorNr = ifelse(nr == Outlier_row , 0,
                           ifelse(nr > Outlier_row ,nr-1,nr)))

#Join dw_episode with Doctor by episodenbr to acess the names of the doctor
doctor_episode <- dw_episode %>% left_join(Doctor, by = c("episodenbr" = "episodenbr")) %>%
  left_join(DoctorActors_unique, by = c("DoctorActor"="Actor"))

#checking the join
doctor_episode %>% select(doctorNr, DoctorActor, cast)

##Which doctor has the maximum, minimum and mean of episodes

##To be continued
  
  