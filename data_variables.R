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
library(igraph)
library(adespatial)
library(spdep)


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

hydro_in_stf<-st_intersection(hydrography_stf_crs,stf_sf_crs) #to get strahler order ##save it to RDS file

#saveRDS(hydro_in_stf,file='saint_francois_strahler.RDS')

#extract chateauguay of hydrography
hydro_chat_buffer<-st_buffer(hydro_chat,25) #create 25m buffer around hydrography to include points

chat_sf_crs<-st_transform(chatauguay_sf,4326)
hydrography_chat_crs<-st_transform(hydro_chat_buffer,4326)

hydro_in_chat<-st_intersection(hydrography_chat_crs,chat_sf_crs) #to get strahler order #save in RDS file

#saveRDS(hydro_in_chat,file='chateauguay_strahler.RDS')

#######################
#explanatory environmental variables
#######################

###############
#Saint-Francois
###############


stf_env<-hydro_in_stf[,c(10,40:42)]
##rename columns
stf_env<-stf_env%>%rename( "Plantes"="Présence.de...plantes.",
                           "Habitat"="Type...d.habitat",
                           "O_Strahler"="O_STRAHLER")

stf_env$O_Strahler<-as.factor(stf_env$O_Strahler)
stf_env$Plantes<-as.factor(stf_env$Plantes)
stf_env$Habitat<-as.factor(stf_env$Habitat)

#saveRDS(stf_env,file='saint_francois_environmental_variables_final.RDS')

###############
#Chateauguay
###############

chat_env<-hydro_in_chat[,c(10,40:42)]
#rename columns
chat_env<-chat_env%>%rename( "Plantes"="Présence.de...plantes.",
                           "Habitat"="Type...d.habitat",
                           "O_Strahler"="O_STRAHLER")

chat_env$O_Strahler<-as.factor(chat_env$O_Strahler)
chat_env$Plantes<-as.factor(chat_env$Plantes)
chat_env$Habitat<-as.factor(chat_env$Habitat)

#saveRDS(chat_env,file='chateauguay_environmental_variables_final.RDS')


###################
#spatial variables
##################

load("C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/code/tolerane_nb.Rdata")

#################
##Saint-Francois
#################


############
#Chatauguay
###########
chatauguay_df<-readRDS('C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/ADN/chatau_df.RDS')
chatauguay_points<-chatauguay_df[c(3:60),c(1:3)]
row.names(chatauguay_points)<-1:58
chatauguay_points_geom <- st_as_sf(x =chatauguay_points, 
                                   coords = c("Longitude","Latitude"),
                                   crs = "+proj=longlat +ellps=WGS84 +no_defs")

chatau_clust<-hclust(dist(chatauguay_points[,2:3]),method = 'single')
chatau_clust_gr<-cutree(chatau_clust,k=10)
plot(chatauguay_points[,2:3],col=chatau_clust_gr)
text(chatauguay_points$Latitude,chatauguay_points$Longitude,labels = 1:58,col=chatau_clust_gr)

#create tolerance matrix
#tolerance_matrix <- function(whole,grp) {
  link_mat <-
    matrix(0,
           ncol = length(grp) + 1,
           nrow = length(grp) + 1)
  for (i in 1:length(grp)) {
    if (i + 1 > i) {
      link_mat[i, i + 1] <- 1
    } else {
      link_mat[i, i + 1] <- 0
    }
  }
  return(link_mat)
}

#tolerance_matrix_blob<- function(val,grp) {
  link_mat <-
    matrix(0,
           ncol = length(val),
           nrow = length(val) )
  for (i in seq_along(val)) {
    if (val[i+1] > val[i]) {
      link_mat[val[i+1], val[i]] <- 1
    } else {
      link_mat[val[i+1], val[i]] <- 0
    }
  }
  return(link_mat)
  }


#blobbi<-tolerance_matrix_blob(val=bloob)

#mat_gr1<-tolerance_matrix(whole = chatau_clust_gr,grp=chatau_clust_gr[which(chatau_clust_gr == 1)])
#mat_1<-mat_gr1[-nrow(mat_gr1), -1]
#colnames(mat_1)<-names(chatau_clust_gr[which(chatau_clust_gr == 1)])

#adj_mat<-link_mat[-nrow(link_mat), -1]

chatauguay_mat <- read_csv("C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/chatauguay_mat.csv")
chatau_mat<-chatauguay_mat[,-1]
chatau_mat[is.na(chatau_mat)]<-0
chatau_tol<-as.matrix(chatau_mat)

#chatau_clust_gr[which(chatau_clust_gr == 1)]

#link_mat_chatau_gr1[-nrow(link_mat_chatau_gr1),-1]

#tol = tolerance.nb(st_coordinates(chatauguay_points_geom),
                  # max.dist = 0.05,
                  # tolerance = 90, rot.angle = 180,plot.sites = TRUE)

plot(tol, st_coordinates(chatauguay_sf$geometry))

tol_list <- mat2listw(chatau_tol)
tol_neighbours<-tol_list$neighbours

##Moran's I positive and significant
# Building AEMs
Wdist <- 1/as.matrix(dist(chatauguay_points[,2:3]))
AEM_Matrix <- aem.build.binary(nb.object=tol_neighbours, coords=cbind(1:nrow(chatauguay_points_geom),st_coordinates(chatauguay_points_geom[,2])),plot.connexions = TRUE)
linkBase <- AEM_Matrix[[2]] #edge
link <- linkBase[-which(linkBase[,1] == 0),]
weight <- numeric()

for(i in 1:nrow(link)){
  weight[i] <- Wdist[link[i,1],link[i,2]]
}

AEM <- aem(AEM_Matrix, weight = weight, rm.link0 = TRUE)

# Constructing asymmetric matrix
matasym <- matrix(0,ncol=58, nrow=58)

for(i in 1:nrow(link)){
  matasym[link[i,1],link[i,2]]<- weight[i]
}

# Build a listw object from the asymmetric matrix
listwAsym <- mat2listw(matasym)

# Calculate Moran's I for AEM
MoranIAEM <- moran.randtest(AEM$vectors, listwAsym, nrepet = 9999)

#Positive Moran's I associated vectors
chatauguay_spatial_vectors <- AEM$vectors[,which(MoranIAEM$obs>0 & MoranIAEM$adj.pvalue <= 0.05)]
saveRDS(chatauguay_spatial_vectors, file="chatauguay_spatial_vectors.RDS")
