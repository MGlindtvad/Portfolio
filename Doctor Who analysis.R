library(ggplot2)
library(dplyr)
library(readr)
library(draw)

##Loading the data from Kaggel
##
dwguide <- read_csv("C:/Users/mvg92/OneDrive/Dokumenter/Portofolie/DoctorWhoDataSet/dwguide.csv")
allscript <- read_csv("C:/Users/mvg92/OneDrive/Dokumenter/Portofolie/DoctorWhoDataSet/all-scripts.csv")
allepisodes <- read_csv("C:/Users/mvg92/OneDrive/Dokumenter/Portofolie/DoctorWhoDataSet/all-detailsepisodes.csv")
imdb_details <- read_csv("C:/Users/mvg92/OneDrive/Dokumenter/Portofolie/DoctorWhoDataSet/imdb_details.csv")


str(dwguide, give.att = FALSE)
str(allscript, give.att = FALSE)
str(allepisodes, give.att = FALSE)
str(imdb_details, give.att = FALSE)

