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

#open saint_francois data
saint_francois_data<-read_excel('Résultat_ADNe_Saint-François_2021_.xlsx',sheet='présence_absence')
saint_francois_df<-as.data.frame(saint_francois_data)

#subset for only environmental data with coordinates
saint_francois_env<-saint_francois_df[c(3:146),c(1:9)]

#make it a spatial object
saint_francois_sf <- st_as_sf(x =saint_francois_env, 
                              coords = c("Longitude","Latitude"),
                              crs = "+proj=longlat +ellps=WGS84 +no_defs")

#mapview(saint_francois_sf)

#build community matrix 
##make spatial object
saint_comm_sf <- st_as_sf(x =saint_francois_df[c(3:146),c(2,3,10:50)], 
                           coords = c("Longitude","Latitude"),
                           crs = "+proj=longlat +ellps=WGS84 +no_defs")

#########
#Jaccard
#########
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

saint_jacc_comm<-similarity(saint_comm_sf)
mapview(saint_jacc_comm,zcol='col',layer.name='Jaccard Similarity Index')
