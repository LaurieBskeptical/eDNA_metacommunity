library(readxl)
library(mapview)
library(sf)
library(sp)
library(magrittr)
library(stringr)
library(dplyr)
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

#open saint_francois data
saint_francois_data<-read_excel('Résultat_ADNe_Saint-François_2021_.xlsx',sheet='présence_absence')
saint_francois_df<-as.data.frame(saint_francois_data)

#subset for only environmental data with coordinates
saint_francois_env<-saint_francois_df[c(3:146),c(1:9)]

#make it a spatial object
saint_francois_sf <- st_as_sf(x =saint_francois_env, 
                          coords = c("Longitude","Latitude"),
                          crs = "+proj=longlat +ellps=WGS84 +no_defs")

mapview(saint_francois_sf)+chatauguay_sf

##########
#dam data
#########

dam_raw <-
  read_excel(
    'C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/repertoire_des_barrages.xls'
  )

#deal with multiple headers
no_name<-dam_raw%>%
  select(matches('...[1:44]'))
no_name_col<-colnames(no_name)

(head_1<-read_excel('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/repertoire_des_barrages.xls',col_names = TRUE)%>%
    names()%>%
    str_replace('...[1:44]',NA_character_))
no_name<-dam_raw%>%
   select(matches('...[1:44]'))

colnames(plok)
x<-colnames(plok)
