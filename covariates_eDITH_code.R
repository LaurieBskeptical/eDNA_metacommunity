#install.packages("Rcpp")
#install.packages("usethis")
library(devtools)
#install_github("lucarraro/eDITH")
library(eDITH)
library(sf)
library(mapview)
library(rgdal)
library(terra)


############
#TOPOGRAPHY
############

topo_raw<-read_sf('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/Topography/URL_feuillet_ProDer.shp')

#load hydrography to check which cell to choose
##saint-francois
hydro_stf<-readOGR('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/hydro/GRHQ_03AF_GRP.gdb')
hydro_stf<-st_as_sf(hydro_stf) #make it sf object

mapview(topo_raw,color='red')+
  mapview(hydro_stf,color='blue')

#####
#check if importing tif files work
#essai<-rast('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/Topography/TIF/MNT_31I02SO.tif')
#plot(essai) #GOOD!
####



