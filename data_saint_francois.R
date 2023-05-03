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

setwd('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/ADN/raw')
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
load('similarity_function.Rdata')


saint_jacc_comm<-jaccard_similarity(saint_comm_sf)
mapview(saint_jacc_comm,zcol='col',layer.name='Jaccard Similarity Index')
