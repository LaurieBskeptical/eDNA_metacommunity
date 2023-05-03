
#Write functions 

##jaccard index function 
jaccard_similarity<-function(community_matrix){
  ##TRANSFORM COMMUNITY DATA
  comm<-st_drop_geometry(community_matrix) #drop sf from community matrix
  vec<-as.vector(comm) 
  mat<-do.call(cbind,vec) #bind all species columns together
  class(mat)<-'numeric'
  ##CALCULATE JACCARD SIMILARITY INDICE
  a <- mat %*% t(mat)
  b <- mat %*% (1 - t(mat))
  c <- (1 - mat) %*% t(mat)
  d <- ncol(mat) - a - b - c
  
  simi = a / (a+b+c)
  
  diag(simi) = 0
  col<-(rowSums(simi))/nrow(mat) #divide by number of sites
  comm_sim<-cbind(community_matrix,col)
  
  return(comm_sim)
}

save('jaccard_similarity',file='similarity_function.Rdata')

