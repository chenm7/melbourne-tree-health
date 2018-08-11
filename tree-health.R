setwd("C:/Users/aanpa/Dropbox/School and work/UC Davis/2017-1 Fall/STA 206/Project/Project")
k<-read.csv("Trees__with_species_and_dimensions__Urban_Forest_.csv", header=TRUE ,na.strings = c("", " "))   #Read Data


###### Missing Values
dim(k)#Start with 69275 obs
s=k[complete.cases(k),] #Remove missing values 
dim(s) #After removing missing,

#Seems to be some Unknowns in Family, need to drop these
toBeRemoved<-which(s$Family=="UNKNOWN")
s=s[-toBeRemoved,] #Dropping UNKNOWNs
dim(s)[1] 

#Redundant Information
s=subset(s,select=-c(CoordinateLocation, Easting, Northing, UploadDate)) 

###Drop unused levels in Family
length(levels(s$Family))
s$Family = droplevels(s$Family) #Dropping unused Factors
length(levels(s$Family)) 

##Dropping unused levels in Genus
s$Genus=factor(s$Genus)
length(levels(s$Genus))
s$Genus=droplevels(s$Genus)
length(levels(s$Genus))

##Dropping unused levels in Scientific Name
length(levels(s$Scientific.Name))
s$Scientific.Name = droplevels(s$Scientific.Name) #Dropping unused Factors
length(levels(s$Scientific.Name)) 

###### Latitude and Longitude

####CODE FOR Latitude Longitude Location Plot
locatedindicator=ifelse(s$Located.in=="Street",1,0)
palette(c("red", "blue"))
png('/Users/douglasturner/Documents/STA206/Project/LatLongLoc.png')
plot(s$Latitude,s$Longitude, pch=20, col=locatedindicator+1, xlab="Latitude", ylab="Longitude", main="Tree Location") # Plot of Trees
legend(x="bottomleft",pch=15, c("Park", "Street"), col=c("red","blue"))
dev.off()
####

####CODE FOR PLOT Latitude Longitude Precinct Plot

palette(rainbow(12))
png('/Users/douglasturner/Documents/STA206/Project/LatLongPre.png')
plot(s$Latitude,s$Longitude, pch=20, col=s$Precinct, xlab="Latitude", ylab="Longitude", main="Tree Location by Precinct")
dev.off()

legend(x="topright",pch=20, levels(factor(s$Precinct)), col=rainbow(12))

####Legend Needs to be seperate for this since its so large
####


####CODE FOR PLOT Latitude Longitude Cluster Plot

clustersize=6 #Set cluster size
labels=kmeans(data.matrix(data.frame(s$Latitude,s$Longitude)),clustersize)$cluster  #Generate clusters of size Cluster Size
palette(c("red", "black", "purple", "yellow", "blue", "green"))
png('/Users/douglasturner/Documents/STA206/Project/LatLongCluster.png')
plot(s$Latitude,s$Longitude,col=labels, pch=20,xlab="Latitude", ylab="Longitude", main="K-means clustering of Tree Location") #Plot clusters with colors
dev.off()
####








###### Family

###CODE FOR Family Pie Chart
Familyind=s$Family
Familyind=as.character(Familyind)
Familyind[!(s$Family=="Myrtaceae" | s$Family=="Platanaceae" | s$Family=="Ulmaceae")]= "Other"
Familyind=factor(Familyind)
png('/Users/douglasturner/Documents/STA206/Project/Family.png')
pie(table(Familyind), main="Family")
dev.off()
####










###### Genus





### CODE FOR Genus Pie Chart
Genusind=s$Genus
Genusind=as.character(Genusind)
Genusind[!(s$Genus=="Eucalyptus" | s$Genus=="Corymbia" | s$Genus=="Platanus"| s$Genus=="Ulmus" )]= "Other"
Genusind=factor(Genusind)
png('/Users/douglasturner/Documents/STA206/Project/Genus.png')
pie(table(Genusind), main="Genus")
dev.off()






##### Scientific Name
#pie(table(s$Scientific.Name))




##### Diameter.Breast.Height

###CODE FOR PLOT Diamter Breast Height Histogram
png('/Users/douglasturner/Documents/STA206/Project/DBHhist.png')
hist(s$Diameter.Breast.Height,xlab="Diameter Breast Height", main="Diameter Breast Height")
dev.off()
###







##### Located.In
#pie(table(s$Located.in))
#pretty simple







##### Useful Life Expectancy value
#hist((s$Useful.Life.Expectency.Value), bins=60)
#boxplot(s$Useful.Life.Expectency.Value )
#levels(factor(s$Useful.Life.Expectency.Value))






#### CODE FOR PLOT Standardized Useful Life Expectancy Histogram
me=ave(s$Useful.Life.Expectency.Value, s$Scientific.Name)
dmule=s$Useful.Life.Expectency.Value-me
div=ave(s$Useful.Life.Expectency.Value, s$Scientific.Name, FUN=var)
stanule=dmule/sqrt(div)
png('/Users/douglasturner/Documents/STA206/Project/StanULEV.png')
hist(stanule, xlab= "", main="Standardized Useful Life Expectancy Value")
dev.off()

######################################################################


#############################################################################################
####Training and Testing Data
library(MASS)
set.seed(123)
smp_size <- floor(0.75 * dim(s)[1]) #75 percent for training data, 25 percent for testing
train_ind <- sample(seq_len(nrow(s)), size = smp_size)
train <- s[train_ind, ]
test <- s[-train_ind, ]


###Standardize for training datset
#DBH
me=ave(train$Diameter.Breast.Height, train$Scientific.Name)
dmule=train$Diameter.Breast.Height-me
div=ave(train$Diameter.Breast.Height, train$Scientific.Name, FUN=var)
standbh=dmule/sqrt(div)
train=data.frame(train,standbh)
#ULE
me=ave(train$Useful.Life.Expectency.Value, train$Scientific.Name)
dmule=train$Useful.Life.Expectency.Value-me
div=ave(train$Useful.Life.Expectency.Value, train$Scientific.Name, FUN=var)
stanule=dmule/sqrt(div)
train=data.frame(train,stanule)






#Some with zero variance need to be dropped
train=train[!(is.na(train$stanule)),]
train=train[!(is.na(train$standbh)),]

##Model 2.1
#No transformation Fit

fit=lm((stanule-min(stanule)+.01)~Age.Description+ standbh +Located.in+Precinct, data=train)

####Code for QQ plot of untransformed 
png('/Users/douglasturner/Documents/STA206/Project/Model21notransQQ.png')
plot(fit, which=2, sub.caption= "", main="Standardized ULE Q-Q plot with No Transformation")
dev.off()





####Code for Box-Cox Plot of Model 2.1
png('/Users/douglasturner/Documents/STA206/Project/StanULEVboxcox.png')
boxcox(fit, plotit = TRUE)
dev.off()
##########

##########
###Cube Root transformation
stanule34=sign(train$stanule)*abs(train$stanule)^(3/4)
train=data.frame(train,stanule34)
fit=lm(stanule34~Age.Description+ standbh +Located.in+Precinct, data=train)
#plot(fit,which=2)



#PLOT 
png('/Users/douglasturner/Documents/STA206/Project/Model21FvR.png')
plot(fit, which=1, sub.caption="")
dev.off()

#PLOT
png('/Users/douglasturner/Documents/STA206/Project/Model21QQ.png')
plot(fit, which=2, sub.caption="", main="Standardized ULE Q-Q Plot with Transformation")
dev.off()

#PLOT
png('/Users/douglasturner/Documents/STA206/Project/Model21Cooks.png')
plot(fit, which=4, sub.caption="")
dev.off()





#############Model selection for Model 2


minclust=3
maxclust=15
labels=matrix(0,length(train$Latitude),maxclust-minclust+1)


for (i in 1:maxclust-minclust+1) {
labels[,i]=kmeans(data.matrix(data.frame(train$Latitude,train$Longitude)),i+(minclust-1))$cluster
}


train=data.frame(train,labels)


####Train a model on each clustersize
fits=list()
for (i in c(1:(maxclust-minclust+1))) {
  fits[[i]]=lm(stanule34~Age.Description+ standbh +Located.in+factor(train[,paste("X",i, sep="")]), data=train)
}


n=length(train$Genus)

#number of coefficients in each model: p
ssto = sum((train$stanule34-mean(train$stanule34))^2)

SSE=c()
Rsq=c()
R2a=c()
p=c()
clustersize=c()
for (i in c(1:(maxclust-minclust+1))) {
  clustersize=c(clustersize,i+(minclust-1))
  SSE=c(SSE, (1-summary(fits[[i]])$r.squared)*ssto)
  Rsq=c(Rsq, summary(fits[[i]])$r.squared)
  R2a=c(R2a,summary(fits[[i]])$adj.r.squared)
  p=c(p,dim(summary(fits[[i]])$coefficients)[1])
}
aic = n*log(SSE/n)+2*p
bic = n*log(SSE/n)+log(n)*p
selection=matrix()
selection=cbind(clustersize,p,SSE,Rsq,R2a,aic,bic)

###Train a model on precinct instead

fitpre=lm(stanule34~Age.Description+ standbh +Located.in+Precinct, data=train)

SSE=c()
Rsq=c()
R2a=c()
p=c()
aic=c()
bic=c()
SSE=(1-summary(fitpre)$r.squared)*ssto
Rsq=summary(fitpre)$r.squared
R2a=summary(fitpre)$adj.r.squared
p=dim(summary(fitpre)$coefficients)[1]
aic = n*log(SSE/n)+2*p
bic = n*log(SSE/n)+log(n)*p
selection=rbind(selection, c(NA, p,SSE,Rsq,R2a,aic,bic))
selection

#Adding stars
for (i in c("aic","bic","SSE")) {
  y=which.min(selection[,i])
  selection[y,i]=paste(selection[y,i], "*", sep="")
}
for (i in c("Rsq","R2a")) {
  y=which.max(selection[,i])
  selection[y,i]=paste(selection[y,i], "*", sep="")
}
selection
#Cluster size of 12 seems to be optimal

capture.output(selection, file = "/Users/douglasturner/Documents/STA206/Project/clustercomparison.txt")
######Cluster model

cluster=factor(train[,"X10"])
train=data.frame(train,cluster)
clustmodel=lm(stanule34~Age.Description+ standbh +Located.in+cluster, data=train)
summary(clustmodel)
plot(clustmodel, which=2)











##################################### Validation Data #############

##Standardizing

#DBH
me=ave(test$Diameter.Breast.Height, test$Scientific.Name)
dmule=test$Diameter.Breast.Height-me
div=ave(test$Diameter.Breast.Height, test$Scientific.Name, FUN=var)
standbh=dmule/sqrt(div)

#ULE Standardized
me=ave(test$Useful.Life.Expectency.Value, test$Scientific.Name)
dmule=test$Useful.Life.Expectency.Value-me
div=ave(test$Useful.Life.Expectency.Value, test$Scientific.Name, FUN=var)
stanule=dmule/sqrt(div)
test=data.frame(test, standbh, stanule)


#Drop variance zero cases
stanule=stanule[complete.cases(test)]
test=test[complete.cases(test),]




#Recreate Cluster and predict
labels=kmeans(data.matrix(data.frame(test$Latitude, test$Longitude)),12)$cluster
cluster=factor(labels)
test=data.frame(test, cluster)
pretest=predict(clustmodel, test) #gives us predictions of transformed standardized values

#undoing transformation
pretest=sign(pretest)*abs(pretest)^(4/3)

MSPE=sum((stanule-pretest)^2)/length(pretest)
MSE=sum((clustmodel$residuals)^2)/(length(train$Genus)-dim(summary(clustmodel)$coefficients)[1])







####################################
#Full Dataset, Final Model
cluster=kmeans(data.matrix(data.frame(s$Latitude, s$Longitude)),12)$cluster
s=data.frame(s, factor(cluster))


#Standardizing

#DBH
me=ave(s$Diameter.Breast.Height, s$Scientific.Name)
dmule=s$Diameter.Breast.Height-me
div=ave(s$Diameter.Breast.Height, s$Scientific.Name, FUN=var)
standbh=dmule/sqrt(div)

#ULE Standardized
me=ave(s$Useful.Life.Expectency.Value, s$Scientific.Name)
dmule=s$Useful.Life.Expectency.Value-me
div=ave(s$Useful.Life.Expectency.Value, s$Scientific.Name, FUN=var)
stanule=dmule/sqrt(div)
s=data.frame(s, standbh, stanule)

###transform stanule
stanule34=sign(s$stanule)*abs(s$stanule)^(3/4)
s=data.frame(s,stanule34)

###Drop variance 0 species
s=s[complete.cases(s),]



clustmodel=lm(stanule34~Age.Description+ standbh +Located.in+ factor.cluster., data=s)


library(sjPlot)
###Output regression tables
sjt.lm(clustmodel, file='/Users/douglasturner/Documents/STA206/Project/Modelfinalfit.html')
###


###Plots for full Model
#PLOT 
png('/Users/douglasturner/Documents/STA206/Project/Model22FvR.png')
plot(clustmodel, which=1, sub.caption="")
dev.off()

#PLOT
png('/Users/douglasturner/Documents/STA206/Project/Model22QQ.png')
plot(clustmodel, which=2, sub.caption="", main="Standardized ULE Q-Q Plot with Transformation")
dev.off()

#PLOT
png('/Users/douglasturner/Documents/STA206/Project/Model22Cooks.png')
plot(clustmodel, which=4, sub.caption="")
dev.off()






