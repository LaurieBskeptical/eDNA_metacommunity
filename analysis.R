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
##make dummy variables of categorical data
mat_chatau_env <- dummy_cols(mat_chatau, select_columns = c('Habitat', 'Plantes'),
                      remove_selected_columns = TRUE)
mat_chatau_env$O_Strahler<-as.numeric(as.character(mat_chatau_env$O_Strahler))

mat_chatau_env<-mat_chatau_env[,-8] #remove NA

chatau_random<-as.factor(1:nrow(chatau_comm))

formdata_chatau <- as.HMSCdata(Y = as.matrix(chatau_comm_num),
                            X = mat_chatau_env,
                            Random = chatau_random,
                            interceptX = TRUE,
                            scaleX = TRUE)

model_chatau <- hmsc(formdata_chatau, family = "probit", niter = 99999, nburn = 30000)
R2chatau<-Rsquared(model_chatau) ##0,709 #good

mcmcParamX <- as.mcmc(model_chatau, parameters = "paramX")
plot(mcmcParamX)


