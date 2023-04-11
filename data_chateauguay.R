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
chatau_comm<-st_drop_geometry(chatau_comm_sf)

chatau_vec<-as.vector(chatau_comm)

mat_chatau<-do.call(cbind,chatau_vec)

class(mat_chatau)<-'numeric'

chatau_jacc<-vegdist(mat_chatau,'jac',binary = TRUE)
jacc_mat<-as.matrix(chatau_jacc)
plot(rowSums(jacc_mat))

chatau_jacc

df = mat_chatau
a <- df %*% t(df)
b <- df %*% (1 - t(df))
c <- (1 - df) %*% t(df)
d <- ncol(df) - a - b - c

simi = a / (a+b+c)

diag(simi) = 0

plot(rowSums(simi))

col_chatau<-rowSums(simi)
chatau_sim<-cbind(chatauguay_sf,col_chatau)
mapview(chatau_sim,zcol='col_chatau')



##########
#ennironmental data
##########

#saint-francois
env_fr<-saint_francois_env[,-c(2,3)]






