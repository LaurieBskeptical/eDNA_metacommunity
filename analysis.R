### Install devtools (if you do not have them already)
#install.packages('devtools')

### Install depend packages (if you do not have them already)
#install.packages('Rcpp')
#install.packages('RcppArmadillo')
#install.packages('coda')

### Install suggested packages (if you do not have them already)
#install.packages('beanplot')
#install.packages('circlize')
#install.packages('corrplot')
# install.packages('coda')

# load the package
library(devtools)

# install HMSC from github
#install_github('guiblanchet/HMSC')

# and load it
library(HMSC)
library(fastDummies)
library(ggtern)

###########
#Chatauguay
###########

#load data
chatau_comm_sf<-readRDS("C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/ADN/chatau_comm_sf.RDS")
chatau_comm<-st_drop_geometry(chatau_comm_sf)
chatau_comm_num<-as.data.frame(lapply(chatau_comm, as.numeric))

env_chat_sf<-readRDS("C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/environment/chateauguay_environmental_variables_final.RDS")
env_chat<-st_drop_geometry(env_chat_sf)


space_chat<-readRDS("C:/Users/greco/OneDrive - USherbrooke/Maitrise/Projet de maitrise/data/Spatial/chatauguay_spatial_vectors.RDS")

#format data
mat_chatau<-data.frame(env_chat,space_chat)
mat_chatau$Habitat[9]<-'Eau calme' #########!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!####### 

##make dummy variables of categorical data
mat_chatau_env <- dummy_cols(mat_chatau, select_columns = c('Habitat', 'Plantes'),
                      remove_selected_columns = TRUE,remove_first_dummy = TRUE)
mat_chatau_env<-mat_chatau_env[,-1]
mat_chatau_env<-mat_chatau_env[,-12]

chatau_env_hmsc<-mat_chatau_env[,c(10:14)]
chatau_space_hmsc<-mat_chatau_env[,1:9]

chatau_random<-as.factor(1:nrow(chatau_comm_num))

formdata_chatau <- as.HMSCdata(Y = as.matrix(chatau_comm_num),
                            X = mat_chatau_env,
                            Random = chatau_random,
                            interceptX = FALSE,
                            scaleX = TRUE)

# Estimate model parameters
model_chatau <- hmsc(formdata_chatau, family = "probit", niter = 100000, nburn = 30000)
R2chatau<-Rsquared(model_chatau,type='nakagawa') 

# Check convergence
mcmcParamX_chatau <- as.mcmc(model_chatau, parameters = "paramX")
plot(mcmcParamX_chatau)

effetSize_chatau <- effectiveSize(mcmcParamX_chatau)

#======================
#Variation partitioning 
#======================

#---------------
##For chatauguay
#---------------
#Partition

vari_chatau<-variPart(hmsc=model_chatau,groupX=c(rep('chatau_env_hmsc',5),rep('chatau_space_hmsc',9)),indSite=TRUE,R2adjust=TRUE,type='III')
#saveRDS(vari_chatau,file="variation_part_chatauguay.RDS")

##Organise data
variFinal_chatau <- vari_chatau$overlap1
df_variFinal_chatau<-as.data.frame(variFinal_chatau)
# Environment
variFinal_chatau[,,"chatau_env_hmsc"] <- vari_chatau$overlap1[,,"chatau_env_hmsc"] +
  vari_chatau$overlap2[,,"chatau_env_hmsc-random"] +
  vari_chatau$overlap2[,,"chatau_env_hmsc-chatau_space_hmsc"]/2 +
  vari_chatau$overlap3/2

# Space
variFinal_chatau[,,"chatau_space_hmsc"] <- vari_chatau$overlap1[,,"chatau_space_hmsc"] +
  vari_chatau$overlap2[,,"chatau_space_hmsc-random"] +
  vari_chatau$overlap2[,,"chatau_env_hmsc-chatau_space_hmsc"]/2 +
  vari_chatau$overlap3/2

# Summary for plotting
vp_chatau_Site <- apply(variFinal_chatau, c(1,3), mean)
vp_chatau_Site<-ifelse(vp_chatau_Site<0,0,vp_chatau_Site)

vp_chatau_Sp <- apply(variFinal_chatau, c(2,3), sum)
vp_chatau_Sp<-ifelse(vp_chatau_Sp<0,0,vp_chatau_Sp)


##Plot results

###For sites
par(mfrow = c(1, 2))
sites_chatau_plot<-ggtern(data=as.data.frame(vp_chatau_Site),
       aes(random, chatau_space_hmsc, chatau_env_hmsc)) +
  ggtitle("Sites rivière Châtauguay") +
  geom_point(colour = rgb(0, 0, 0, 0.5),
             aes(size = rowSums(vp_chatau_Site)*400)) +
  xlab("Co-Distribution") + ylab("Spatial") + zlab("Environnement")+
  theme_rgbw()


sites_chatau_plot+ scale_size(name   = "R2",
                         breaks = fivenum(rowSums(vp_chatau_Site)* 400),
                         labels = fivenum(rowSums(vp_chatau_Site))) 
#for species
par(mfrow = c(2, 2))
sp_chatau_plot<-ggtern(data=as.data.frame(vp_chatau_Sp),
       aes(random, chatau_space_hmsc, chatau_env_hmsc)) +
  ggtitle("Espèces rivière Châtauguay") +
  geom_point(colour = rgb(0, 0, 0, 0.5),
             aes(size = rowSums(vp_chatau_Sp)* 10)) +
  xlab("Co-Distribution") + ylab("Spatial") + zlab("Environnement")+
  theme_rgbw()

sp_chatau_plot+ scale_size(name   = "R2",
                                        breaks = fivenum(rowSums(vp_chatau_Sp)* 10),
                                        labels = fivenum(rowSums(vp_chatau_Sp))) 
