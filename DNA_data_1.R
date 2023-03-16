library(readxl)
library(mapview)
library(sf)
library(magrittr)
library(stringr)
library(dplyr)
library(janitor)
library(rgdal)

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

setwd('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment')

dam_raw <-
  read_excel(
    'C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/repertoire_des_barrages.xls'
  )

#Set first row as column names
dam<-janitor::row_to_names(dam_raw, 1, remove_rows_above = FALSE) 
dam<-as.data.frame(dam) #turn into df

#remove NA from coords columns
dam_na<-dam[!is.na(dam$`Latitude (NAD 83)`),]
# sum(is.na(dam_na$`Latitude (NAD 83)`)) #it worked

#make it a spatial object
dam_sf<- st_as_sf(x =dam_na, 
                          coords = c("Longitude (NAD 83)","Latitude (NAD 83)"),
                          crs = "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83
+no_defs +towgs84=0,0,0
") 

mapview(dam_sf)


mapview(saint_francois_sf,col.regions='red')+dam_sf

#hydrography
##for saint-francois river
hydro_stf<-readOGR('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/hydro/GRHQ_03AF_GRP.gdb')

##for chateauguay river
hydro_chat<-readOGR('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/hydro/GRHQ_03AB.gdb')

mapview(list(hydro_stf,saint_francois_sf),col.regions=list('blue','red'))
   
#unites de decoupage
hydro_cut<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/decoupage')
hydro_03<-hydro_cut[hydro_cut$Bloc=='03',]

 mapview(hydro_03)
 