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

#Removing space from McGann and McCoy
doctor_episode <- doctor_episode %>%
  mutate(DoctorActor = ifelse(DoctorActor == "Paul Mc Gann", "Paul McGann", 
                              ifelse(DoctorActor == "Sylvester Mc Coy","Sylvester McCoy",DoctorActor)))

#Plotting number of episodes each actor has be the main doctor in(Only one doctor pr episode)
Number_of_episode <- doctor_episode %>%
  mutate(Doctor_x = paste(doctorNr,". ", DoctorActor)) %>%
  filter(nr != Outlier_row) %>%
  group_by(Doctor_x) %>% 
  summarize(numberOfEpisodes = n()) %>%
  mutate(NumberDoctor = as.numeric(str_sub(Doctor_x,1,2))) %>%
  arrange(NumberDoctor)


#Changing the factor levels so as to order the x-axis 
#according to the order of the doctors
Number_of_episode$Doctor_x <- factor(Number_of_episode$Doctor_x, 
                                     levels = Number_of_episode$Doctor_x, 
                                     ordered =  TRUE)

#Plotting
EpisodePlot <- Number_of_episode%>%
  ggplot(aes(Doctor_x,numberOfEpisodes, group = 1))+
  geom_col(fill = "blue2", color = "black")+
  ggtitle("Number of episode for each doctor")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  ylab("Number of episodes")+
  xlab("Actor")

EpisodePlot

##Which doctor has the maximum, minimum and mean of episodes?
maximum <- Number_of_episode %>% filter(numberOfEpisodes == max(numberOfEpisodes))
minimum <- Number_of_episode %>% filter(numberOfEpisodes == min(numberOfEpisodes))
mean_sd <- Number_of_episode %>% summarize(mean = mean(numberOfEpisodes), 
                                           sd = sd(numberOfEpisodes))

unlist(maximum)
unlist(minimum)
unlist(mean_sd)

x_input_max <- Number_of_episode %>% 
  filter(NumberDoctor == maximum$NumberDoctor+2) %>%
  select(Doctor_x)

y_input_max <- maximum$numberOfEpisodes-15

x_input_min <- Number_of_episode %>% 
  filter(NumberDoctor == minimum$NumberDoctor) %>%
  select(Doctor_x)

y_input_min <- minimum$numberOfEpisodes+100

class(maximum$numberOfEpisodes)
EpisodePlot + annotate(geom = "curve",
                       x= as.numeric(x_input_max), 
                       y = y_input_max,
                       xend = as.character(maximum$Doctor_x), 
                       yend = (maximum$numberOfEpisodes+1),
                       curvature = .7,
                       arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x= as.numeric(x_input_max), 
           y = y_input_max-2,
           label = "doctor with most episodes",
           hjust = "left")+
  
  annotate(geom = "curve",
           x= as.numeric(x_input_min), 
           y = y_input_min,
           xend = as.character(minimum$Doctor_x), 
           yend = (minimum$numberOfEpisodes+1),
           curvature = .1,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x= as.numeric(x_input_min), 
           y = y_input_min+3,
           label = "doctor with least episodes",
           hjust = "left")

##Longest time on screen?
##Best reviews?
##To be continued
  
  