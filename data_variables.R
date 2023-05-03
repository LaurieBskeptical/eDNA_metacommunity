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

#mapview(dam_sf)


#mapview(saint_francois_sf,col.regions='red')+dam_sf


#unites de decoupage
hydro_cut<-st_read('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/decoupage')
hydro_03<-hydro_cut[hydro_cut$Bloc=='03',]

#mapview(hydro_03)

#cut dams out of the hydro zone
##check projection
st_crs(dam_sf) #not the same so transform to WGS84
st_crs(hydro_03)

dam_crs<-st_transform(dam_sf,4326)
hydro_03_crs<-st_transform(hydro_03,4326)

dam_zone<-st_intersection(dam_crs,hydro_03_crs)
#mapview(dam_zone) #it works


#############
#hydrography
#############

#hydrography
##for saint-francois river
hydro_stf<-readOGR('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/hydro/GRHQ_03AF_GRP.gdb')
hydro_stf<-st_as_sf(hydro_stf) #make it sf object

##for chateauguay river
hydro_chat<-readOGR('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/hydro/GRHQ_03AB.gdb')
hydro_chat<-st_as_sf(hydro_chat) #make it sf object

#extract st-francois of hydrography
hydro_stf_buffer<-st_buffer(hydro_stf,25) #create 25m buffer around hydrography to include points

##match geometry
stf_sf_crs<-st_transform(saint_francois_sf,4326)
hydrography_stf_crs<-st_transform(hydro_stf_buffer,4326)

hydro_in_stf<-st_intersection(hydrography_stf_crs,stf_sf_crs) #to get strahler order

#extract chateauguay of hydrography
hydro_chat_buffer<-st_buffer(hydro_chat,25) #create 25m buffer around hydrography to include points

chat_sf_crs<-st_transform(chatauguay_sf,4326)
hydrography_chat_crs<-st_transform(hydro_chat_buffer,4326)

hydro_in_chat<-st_intersection(hydrography_chat_crs,chat_sf_crs) #to get strahler order



#######################
#explanatory environmental variables
#######################

###############
#Saint-Francois
###############

