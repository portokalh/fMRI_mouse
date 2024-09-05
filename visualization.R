library(dplyr)
library(brainconn2)
require(magrittr)
require(plyr)
require(tidyverse)
library(mediation)
library(ggplot2)
library(ggpubr)

load("distance_Elastic_net.rda")


load("connectivity.rda")





not_in_Atlas = c(23, 86, 139, 150, 189, 274, 296, 316)
present_rois = setdiff(1:332, not_in_Atlas)


connectivity_new = array(0,c(332 , 332, dim(connectivity)[3]))

for (i in 1:dim(connectivity)[3]) {
  connectivity_new[present_rois,present_rois,i] = connectivity[,,i]
}
connectivity = connectivity_new

not_in_Atlas_symm = -1
for (i in not_in_Atlas) {
  if ( i + 166 <=332) {if (!(i+166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i+166)}  } 
  if ( i - 166 >=0) {if (!(i-166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i-166) }  } 
}
not_in_Atlas_symm = not_in_Atlas_symm[-1]

for (i in 1:dim(connectivity)[3]) {
  connectivity[not_in_Atlas_symm,,i] = 0
  connectivity[,not_in_Atlas_symm,i] = 0
}





u = as.matrix(edge_vector)



sum(u==0)
#len=length(u)
sum(u!=0)
u[u!=0]

uout=matrix(NA, 332*331/2,1 )
#put those zeros back
# uout[indd]=0
# # uout=u
uout=u


temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  

connectivityexample=connectivity[,,1]

# connectivityexample[indexlowertrue[indd]] ##yes the're them
# connectivityexample[indexlowertrue[indd]]="zeros" # lest make them known for a special word
indexofzeros=which(connectivityexample=="zeros", arr.ind=TRUE)

#results of connectivities that matter:
nonzeroindex=which(uout!=0)

connectivityexample=connectivity[,,1]
connectivityexample[]=0
connectivitvals=connectivityexample
nonzerouout=uout[uout!=0]
for (i in 1:length(nonzeroindex)) {
  connectivityexample[indexlowertrue[nonzeroindex[i]]]=c("nonzero") # lest make them known for a special word
  connectivitvals[indexlowertrue[nonzeroindex[i]]]=nonzerouout[i] #store their coefitient values
}


library('igraph');
connectivitvalsones=connectivitvals
#####
diag(connectivitvals)=0







#####
t=which(connectivitvalsones!=0, arr.ind=TRUE)
t <- cbind(t, connectivitvals[which(connectivitvals!=0,arr.ind=TRUE)]) 
t.graph=graph.data.frame(t,directed=F)
E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red') 
#t.names <- colnames(cor.matrix)[as.numeric(V(t.graph)$name)]
minC <- rep(-Inf, vcount(t.graph))
maxC <- rep(Inf, vcount(t.graph))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
                    miny=minC, maxy=maxC)      

pathnames='mouse_anatomy.csv'
datanmes=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
datanmes$ROI

# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

#datanmes=datanmes[-noreadcsf]

# datanmess=datanmes$ROI[-noreadcsf] # remove csf
datanmess=datanmes$ROI





connectivitvals=connectivitvals+t(connectivitvals) #symetric


plot = brainconn(atlas ="CHASS", conmat=connectivitvals, 
                 view="top", node.size =4, 
                 node.color = "black", 
                 edge.width = 8, edge.color="red", 
                 edge.alpha = 0.65,
                 edge.color.weighted = T,
                 scale.edge.width=T,
                 labels = T,
                 all.nodes =F, 
                 show.legend = T, 
                 label.size=14, background.alpha=1, 
                 label.edge.weight=F,background = "Chass")  

library(ggplot2)
ggsave(paste0("glasstop", sum(uout!=0), ".png"), plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)






subnets=groups(components(t.graph))
subnetsresults=vector(mode = "list", length = length(subnets))
colsumabs=colSums(abs(connectivitvals))
colsum=colSums(connectivitvals)

leftright=datanmes$Hemisphere




####################3
for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[1,]=as.numeric(temp)
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  #indofleftright=tt>=164
  #net[5,][indofleftright]="Right"
  #net[5,][!indofleftright]="Left"
  
  
  net[2,]=datanmess[temp]
  net[5,]=leftright[temp]
  net[1,]=paste(net[1,],net[5,])
  net[3,]= as.numeric( colsum[temp]   )
  net[4,]= as.numeric( colsumabs[temp]   )
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net 
}

##############new net

net_new=matrix(NA, length(subnetsresults),4)


for (j in 1:dim(net_new)[1]) {
  temps=subnetsresults[[j]]
  net_new[j,1]=j
  net_new[j,2]= paste(temps[8,], collapse = ", ")
  net_new[j,3] = paste(paste(temps[5,],temps[2,]), collapse = ", ")
  net_new[j,4] = paste(temps[7,1])
  
}
colnames(net_new)=c("Sub-Network", "Region Number", "Region Name", "Sub-Network Weight")


#install.packages("xlsx")
library(xlsx)


for (i in 1:length(subnetsresults)){
  net=t(subnetsresults[[i]])
  # write.xlsx2(net, "nets.xlsx", sheetName =  paste0(i), append=TRUE )
}

write.xlsx2(net_new, paste0(sum(uout!=0),"_net.xlsx") )




#########################



uout
top_indices <- order(abs(uout), decreasing = TRUE)[1:50]
top_elements <- rep(0, length (uout))
top_elements[top_indices] <- uout[top_indices]
uout = top_elements
sum(uout!=0)

temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  

connectivityexample=connectivity[,,1]

# connectivityexample[indexlowertrue[indd]] ##yes the're them
# connectivityexample[indexlowertrue[indd]]="zeros" # lest make them known for a special word
indexofzeros=which(connectivityexample=="zeros", arr.ind=TRUE)

#results of connectivities that matter:
nonzeroindex=which(uout!=0)

connectivityexample=connectivity[,,1]
connectivityexample[]=0
connectivitvals=connectivityexample
nonzerouout=uout[uout!=0]
for (i in 1:length(nonzeroindex)) {
  connectivityexample[indexlowertrue[nonzeroindex[i]]]=c("nonzero") # lest make them known for a special word
  connectivitvals[indexlowertrue[nonzeroindex[i]]]=nonzerouout[i] #store their coefitient values
}


library('igraph');
connectivitvalsones=connectivitvals
#####
diag(connectivitvals)=0







#####
t=which(connectivitvalsones!=0, arr.ind=TRUE)
t <- cbind(t, connectivitvals[which(connectivitvals!=0,arr.ind=TRUE)]) 
t.graph=graph.data.frame(t,directed=F)
E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red') 
#t.names <- colnames(cor.matrix)[as.numeric(V(t.graph)$name)]
minC <- rep(-Inf, vcount(t.graph))
maxC <- rep(Inf, vcount(t.graph))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
                    miny=minC, maxy=maxC)      

pathnames='mouse_anatomy.csv'
datanmes=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
datanmes$ROI

# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

#datanmes=datanmes[-noreadcsf]

# datanmess=datanmes$ROI[-noreadcsf] # remove csf
datanmess=datanmes$ROI





connectivitvals=connectivitvals+t(connectivitvals) #symetric


plot = brainconn(atlas ="CHASS", conmat=connectivitvals, 
                 view="top", node.size =4, 
                 node.color = "black", 
                 edge.width = 8, edge.color="red", 
                 edge.alpha = 0.65,
                 edge.color.weighted = T,
                 scale.edge.width=T,
                 labels = T,
                 all.nodes =F, 
                 show.legend = T, 
                 label.size=14, background.alpha=1, 
                 label.edge.weight=F,background = "Chass")  

library(ggplot2)
ggsave(paste0("glasstop", sum(uout!=0), ".png"), plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)






subnets=groups(components(t.graph))
subnetsresults=vector(mode = "list", length = length(subnets))
colsumabs=colSums(abs(connectivitvals))
colsum=colSums(connectivitvals)

leftright=datanmes$Hemisphere




####################3
for (i in 1:length(subnets)) {
  temp=subnets[[i]]
  temp=as.numeric(temp)
  net=matrix(NA,8,length(temp) )
  net[1,]=as.numeric(temp)
  tt=as.numeric(net[1,])
  #tt=c(1,200)
  #indofleftright=tt>=164
  #net[5,][indofleftright]="Right"
  #net[5,][!indofleftright]="Left"
  
  
  net[2,]=datanmess[temp]
  net[5,]=leftright[temp]
  net[1,]=paste(net[1,],net[5,])
  net[3,]= as.numeric( colsum[temp]   )
  net[4,]= as.numeric( colsumabs[temp]   )
  net[6,]=sum(as.numeric(net[4,]))
  net[7,]=sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex=which(datanmes$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]]=net 
}

##############new net

net_new=matrix(NA, length(subnetsresults),4)


for (j in 1:dim(net_new)[1]) {
  temps=subnetsresults[[j]]
  net_new[j,1]=j
  net_new[j,2]= paste(temps[8,], collapse = ", ")
  net_new[j,3] = paste(paste(temps[5,],temps[2,]), collapse = ", ")
  net_new[j,4] = paste(temps[7,1])
  
}
colnames(net_new)=c("Sub-Network", "Region Number", "Region Name", "Sub-Network Weight")


#install.packages("xlsx")
library(xlsx)


for (i in 1:length(subnetsresults)){
  net=t(subnetsresults[[i]])
  # write.xlsx2(net, "nets.xlsx", sheetName =  paste0(i), append=TRUE )
}

write.xlsx2(net_new, paste0("",sum(uout!=0),"_net.xlsx") )


