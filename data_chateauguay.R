library(readxl)
library(mapview)
library(sf)
library(magrittr)
library(stringr)
library(dplyr)
library(janitor)
library(rgdal)
library(ade4)
library(vegan)

############
#eDNA data
###########

setwd('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/ADN/raw')

#open chatauguay data
chatauguay_data<-read_excel('Résultat_ADNe_Chatauguay_2021_.xlsx',sheet='présence_absence')
chatauguay_df<-as.data.frame(chatauguay_data) #make it into a data frame because it's a tibble 

#subset for only environmental data with coordinates
chatauguay_env<-chatauguay_df[c(3:60),c(1:9)]

#make it a spatial object
chatauguay_sf <- st_as_sf(x =chatauguay_env, 
                        coords = c("Longitude","Latitude"),
                        crs = "+proj=longlat +ellps=WGS84 +no_defs")

mapview(chatauguay_sf)

#build community matrix 
##make spatial object
chatau_comm_sf <- st_as_sf(x =chatauguay_df[c(3:60),c(2,3,10:50)], 
                          coords = c("Longitude","Latitude"),
                          crs = "+proj=longlat +ellps=WGS84 +no_defs")

#calculate Jaccards Index of similarity
##load function 
load('similarity_function.Rdata')

chatau_jacc_comm<-jaccard_similarity(chatau_comm_sf)
mapview(chatau_jacc_comm,zcol='col',layer.name='Jaccard Similarity Index')



