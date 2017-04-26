# Open Output Stats File
setwd("")
x <- load("ft_AchnatherumHymenoides.Rdata")
stats.table <- cbind

# remove past sessions
rm(list=ls())

#Set local working directory


#load serial model output
attach(stats.table)

#Check that data have imported correctly
head(stats.table, n=10)

#eliminate outliers or non-mainland NZ species


#Create new variables associated with niche distributions
#Max Temperature
#Q or Solar Radiation
#W or Water Availability Influence on Photosynthesis
#Ns or Shoot Nitrogen
#Tmean or temperature dependence on N uptake
#WN or Water availability influence on N Uptake
#Nsoil or topsoil N concentration
#Tmin or minimum temperature dependence on growth
#TmeanR or temperature dependence for respiration
tmax1<-as.numeric(par1)/2
tmax2<-as.numeric(par2)/2+tmax1
tmax3<-as.numeric(par3)/2+tmax2
tmax4<-as.numeric(par4)/2+tmax3
Q1<-as.numeric(par25)/2
Q2<-as.numeric(par26)/2+Q1
W1<-as.numeric(par5)/2
W2<-as.numeric(par6)/2+W1
N1<-as.numeric(par7)/2
N2<-as.numeric(par8)/2+N1
tmeanN.1<-as.numeric(par9)/2
tmeanN.2<-as.numeric(par10)/2+tmeanN.1
WN.1<-as.numeric(par13)/2
WN.2<-as.numeric(par14)/2+WN.1
WN.3<-as.numeric(par15)/2+WN.2
WN.4<-as.numeric(par16)/2+WN.3
Nsoil1<-as.numeric(par11)/2
Nsoil2<-as.numeric(par12)/2+Nsoil1
Tmin1<-as.numeric(par17)/2
Tmin2<-as.numeric(par18)/2+Tmin1
Tmin3<-as.numeric(par19)/2+Tmin2
Tmin4<-as.numeric(par20)/2+Tmin3
TmeanR1<-as.numeric(par27)/2
TmeanR2<-as.numeric(par28)/2+TmeanR1

#Return 1 for values exceeding 1
tmax1n<-ifelse (tmax1>1,1,tmax1)
tmax2n<-ifelse (tmax2>1,1,tmax2)
tmax3n<-ifelse (tmax3>1,1,tmax3)
tmax4n<-ifelse (tmax4>1,1,tmax4) 
Q1n<-ifelse(Q1>1,1,Q1)
Q2n<-ifelse(Q2>1,1,Q2)
W1n<-ifelse(W1>1,1,W1)
W2n<-ifelse(W2>1,1,W2)
N1n<-ifelse(N1>1,1,N1)
N2n<-ifelse(N2>1,1,N2)
tmeanN.1n<-ifelse(tmeanN.1>1,1,tmeanN.1)
tmeanN.2n<-ifelse(tmeanN.2>1,1,tmeanN.2)
WN.1n<-ifelse(WN.1>1,1,WN.1)
WN.2n<-ifelse(WN.2>1,1,WN.2)
WN.3n<-ifelse(WN.3>1,1,WN.3)
WN.4n<-ifelse(WN.4>1,1,WN.4)
Nsoil1n<-ifelse(Nsoil1>1,1,Nsoil1)
Nsoil2n<-ifelse(Nsoil2>1,1,Nsoil2)
Tmin1n<-ifelse(Tmin1>1,1,Tmin1)
Tmin2n<-ifelse(Tmin2>1,1,Tmin2)
Tmin3n<-ifelse(Tmin3>1,1,Tmin3)
Tmin4n<-ifelse(Tmin4>1,1,Tmin4)
TmeanR1n<-ifelse(TmeanR1>1,1,TmeanR1)
TmeanR2n<-ifelse(TmeanR2>1,1,TmeanR2)

#Create clean dataframe from added and truncated data
clean.par<-cbind(N1n,N2n,Nsoil1n,Nsoil2n,Q1n,Q2n,tmax1n,tmax2n,tmax3n,tmax4n,tmeanN.1n,tmeanN.2n,TmeanR1n,TmeanR2n,Tmin1n,Tmin2n,Tmin3n,Tmin4n,W1n,W2n,WN.1n,WN.2n,WN.3n,WN.4n)

#Create output file with clean parameters
pars<-cbind(N1n,N2n,Nsoil1n,Nsoil2n,Q1n,Q2n,tmax1n,tmax2n,tmax3n,tmax4n,tmeanN.1n,tmeanN.2n,TmeanR1n,TmeanR2n,Tmin1n,Tmin2n,Tmin3n,Tmin4n,W1n,W2n,WN.1n,WN.2n,WN.3n,WN.4n)
pars <- cbind(stats.table[,1], pars)
write.csv(pars,"C:/Paramters_output/paramaters.csv")
#Install Library

library("labdsv")

#priciple components analysis
pca.1<-pca(clean.par,cor=TRUE,dim=10)

summary(pca.1,dim=10)
varplot.pca(pca.1)

#Discriminant Analysis of Principal Components using adegenet
#Load Library

library("adegenet")

#create groups, keep as many PCs as possible, identify number of groups
grp<-find.clusters(clean.par,max.n.clust=12)

dapc1<-dapc(clean.par,grp$grp)
# choose number of princial componenets to retain--low is better (8)
# choose number of discriminant functions to retain--all is best (3)

#create scatterplot
jpeg("C:/Paramters_output/PCA.jpeg", width = 4, height = 4, units = 'in', res = 300)

myCol <- c("darkblue","purple","green","orange","red","blue")
scatter(dapc1, posi.da="bottomright", bg="white",
        pch=17:22, cstar=0, col=myCol, scree.pca=TRUE,
        posi.pca="bottomleft")

dev.off()


#create single discriminant function plot
scatter(dapc1,1,1, col=myCol, bg="white",
        scree.da=FALSE, legend=TRUE, solid=.4)

#examine loading variables
jpeg("C:/Paramters_output/Axis2.jpeg", width = 4, height = 4, units = 'in', res = 300)
set.seed(4)
contrib<-loadingplot(dapc1$var.contr,axis=2,thres=0.1,lab.jitter=1)
dev.off()
#create table
sp <- stats.table[,1]
table(sp,grp$grp)

#output as csv file
x<-data.frame(sp,grp$grp,N1n,N2n,Nsoil1n,Nsoil2n,Q1n,Q2n,tmax1n,tmax2n,tmax3n,tmax4n,tmeanN.1n,tmeanN.2n,TmeanR1n,TmeanR2n,Tmin1n,Tmin2n,Tmin3n,Tmin4n,W1n,W2n,WN.1n,WN.2n,WN.3n,WN.4n)
write.csv(x, file = "C:/Paramters_output/Groups_pars.csv")
y<-data.frame(sp,grp$grp)
write.csv(y,file="C:/Paramters_output/grouping.csv")




