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
##first header
head_1<-dam_raw

names<-dam_raw%>% #select names that are in the header
  select(matches(c('IDENTIFICATION','LOCALISATION','HYDROGRAPHIE','CARACTÉRISTIQUES','PROPRIÉTAIRE / MANDATAIRE','ÉVALUATION DE LA SÉCURITÉ')))

name_col<-colnames(names) 

no_name<-select(dam_raw,-one_of(name_col))%>%colnames #select all the columns that have no name

##replace vector of no names by NAs
names(dam_raw)<-str_replace(names(dam_raw),paste(no_name,collapse = "|"),NA_character_)
names(dam_raw)  

##second header
(head_2<-read_excel('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/repertoire_des_barrages.xls',skip=1,col_names = TRUE)%>%
    names()) #let's forst check the names


  