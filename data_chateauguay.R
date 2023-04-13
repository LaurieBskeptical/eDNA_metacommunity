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

similarity<-function(community_matrix){
  ##TRANSFORM COMMUNITY DATA
  comm<-st_drop_geometry(community_matrix) #drop sf from community matrix
  vec<-as.vector(comm) 
  mat<-do.call(cbind,vec) #bind all species columns together
  class(mat)<-'numeric'
  ##CALCULATE JACCARD SIMILARITY INDICE
  a <- mat %*% t(mat)
  b <- mat %*% (1 - t(mat))
  c <- (1 - mat) %*% t(mat)
  d <- ncol(mat) - a - b - c
  
  simi = a / (a+b+c)
  
  diag(simi) = 0
  col<-rowSums(simi)
  comm_sim<-cbind(community_matrix,col)
  
  return(comm_sim)
}

chatau_jacc_comm<-similarity(chatau_comm_sf)
mapview(chatau_jacc_comm,zcol='col',layer.name='Jaccard Similarity Index')



