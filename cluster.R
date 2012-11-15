## Obtaining the data and observing it
allCallsNAN=read.table("combine.csv", header=TRUE,sep=';',colClasses="character")
allCallsNAN$My_hashNum <- as.numeric(allCallsNAN$My_hashNum)
allCallsNAN$Event <- as.numeric(allCallsNAN$Event)
allCallsNAN$Contact <- as.numeric(allCallsNAN$Contact)
allCallsNAN$hashNum <- as.numeric(allCallsNAN$hashNum)
allCallsNAN$Duration <- as.numeric(allCallsNAN$Duration)
allCallsNAN$Description <- as.matrix(allCallsNAN$Description)
allCallsNAN$Direction <- as.matrix(allCallsNAN$Direction)
allCallsNAN$Date <- as.Date(allCallsNAN$Date)
#Havent yet converted Date field. currently it is a character.


#allCallsNAN=read.table("combine.csv", header=TRUE,sep=';')
summary(allCallsNAN)

##The data has been processed by the following assumption:
# allCalls is the matrix of meaningful calls. If there is a NaN in the hashNum field, that number is excluded from this matrix
# packetCalls is the matrix that has the calls which have their hashNum field as NaNs.
# DirectionDuration is the matrix that has as its $Direction == 'Duration'

allCalls=allCallsNAN[allCallsNAN$Description!='Packet Data',]
packetCalls=allCallsNAN[allCallsNAN$Description=='Packet Data',]

#Self calls were supposed to capture those cases where a multiple of phone numbers called the same number. However,
#this implementation is incorrect because it just captures the cases where the sender and receiver have the same number.
#We need more of a system which checks for the date+time of calls, sender's and receiver's phone number. 
selfCalls <- allCalls[allCalls$My_hashNum==allCalls$hashNum,]
selfPackets <- packetCalls[packetCalls$My_hashNum!=packetCalls$hashNum,]

#Seperating out categorical data:
#1. Weekend/Weekday(1/0)
#2. Daytime/Nighttime(1/0)
#3. Direction of phonecall: outgoing/incoming(includes missedcalls)(1/0)
#4. Type of call: SMS/voice(1/0) There is a third category called the "Data call" type. This has been kept as it is. 
#5. Missed call(1/0)

#Data manipulation for Description. 
Description = subset(x=allCalls,select=Description)
Description[Description=="Short message"]=1
Description[Description=="Voice call"]=0


#Data manipulation for Direction of phone call
Direction = subset(x=allCalls, select=Direction)
Direction[Direction=="Outgoing"]=1
Direction[Direction=="Incoming"]=0
Direction[Direction=="Missed"]=0

#Data manipulation for Missed calls
Missed = subset(x=allCalls, select=Direction)
Missed[Missed=="Missed"]=1
Missed[Missed!=1]=0
names(Missed)="Missed"


newCalls=cbind(allCalls, Description, Direction, Missed)


#Irrelevant
typeNew = type
typeNew <- gsub("Short message", "1",fixed=TRUE, typeNew)
typeNew[typeNew=="Short message"]=1
typeNew[is.na(typeNew)] <- 1
typeNew <- replace(typeNew, typeNew == "Short message",1)
levels(typeNew) <- gsub("Short message","1", levels(typeNew))
allCalls$Description <- replace(allCalls$Description, allCalls$Description=="Short message", 1)
#allCalls <- cbind(allCalls, allCalls$Description=="Short message")
iris$Species <- replace(iris$Species, iris$Species == "setosa","NewName")




allCallsInter <- allCalls[allCalls$hashNum < 110]
singleUser <- allCalls[allCalls$My_hashNum==4,]

calls94 <- allCallsNAN[allCallsNAN$My_hashNum==94,]
calls102 <- allCallsNAN[allCallsNAN$My_hashNum==102,]

library(cluster)
require(graphics)                   
library(clusterSim)                   


## k means clustering begins here. 
dbIndex=array(0,dim=c(1,25))                   
withinss=array(0,dim=c(1,25))                  
for(k in 2:25){
  kcenters=k
  km = kmeans(subset(allCalls,select=c(2,5,6,7,8)),kcenters,100)          
  curr_dbIndex=index.DB(allCalls[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  withinss[1,k]=km$tot.withinss
  
}

# We can judge the clustering quality through the below graphs:
plot(t(dbIndex))
plot(t(withinss))                   


# Finding optimum k 
for(k in 2:25){
  if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
    idealk=k
    break
  }
}

# Re doing k means clustering using the optimum k 
km = kmeans(subset(allCalls,select=c(2,3,4,5,6,7)),idealk,500)          
curr_dbIndex=index.DB(allCalls[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)

clusplot(allCalls[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   

# Finding the Euclidean distance from the center of the cluster to each of the data point
library(fields)
diss=array(0,dim=c(dim(norm2Final3)[1],1))
cluCenters=km$centers
clusterData=km$cluster
for(i in 1:dim(norm2Final3)[1]){
  diss[i,1]=rdist(t(norm2Final3[i,2:7]),t(cluCenters[clusterData[i],]))
}

## Creating new data in which we dont include the outliers that are causing the clustering to be unclear
newNormData = array(0,dim=c(dim(norm2Final3)[1],dim(norm2Final3)[2]+1))
newNormData[,1:7] = norm2Final3
newNormData[,8]=diss
newNormData2 = newNormData[diss<quantile(diss)[4],]


dbIndex=array(0,dim=c(1,25))                   
withinss=array(0,dim=c(1,25))                  
for(k in 2:25){
  kcenters=k
  km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),kcenters,100)          
  curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  withinss[1,k]=km$tot.withinss
  
}                   
plot(t(dbIndex))
plot(t(withinss))                   


for(k in 2:25){
  if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
    idealk=k
    break
  }
}

km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),idealk,500)          
curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)

clusplot(newNormData2[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   

library(fields)
diss=array(0,dim=c(dim(newNormData2)[1],1))
cluCenters=km$centers
clusterData=km$cluster
for(i in 1:dim(newNormData2)[1]){
  diss[i,1]=rdist(t(newNormData2[i,2:7]),t(cluCenters[clusterData[i],]))
}

## Repeating the process once more where we remove outliers in the clusters. 
newNormData3 = array(0,dim=c(dim(newNormData2)[1],dim(newNormData)[2]))
newNormData3[,1:7] = newNormData2[,1:7]
newNormData3[,8]=diss
newNormData4 = newNormData3[diss<0.9*max(diss),]


dbIndex=array(0,dim=c(1,25))                   
withinss=array(0,dim=c(1,25))                  
for(k in 2:25){
  kcenters=k
  km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),kcenters,100)          
  curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  withinss[1,k]=km$tot.withinss
  
}                   
plot(t(dbIndex))
plot(t(withinss))                   


for(k in 2:25){
  if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
    idealk=k
    break
  }
}

km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),idealk,500)          
curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)

clusplot(newNormData4[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   


## Repeating the above process after excluding products:


dbIndex=array(0,dim=c(1,25))                   
withinss=array(0,dim=c(1,25))                  
for(k in 2:25){## Obtaining the data and observing it
  
  library("outliers")
  Final=read.table("Final.csv", header=TRUE,sep=',')
  summary(Final)
  plot(Final[,2])
  plot(Final[,3])
  plot(Final[,4])
  plot(Final[,5])
  plot(Final[,6])
  plot(Final[,7])
  plot(Final[,2]*Final[,3]*Final[,4]*Final[,5]*Final[,6]*Final[,7])
  
  #chisq.out.test(Final[,3],variance=var(Final[,3]),opposite=FALSE)
  #dixon.test(Final[,2],type=0,opposite=FALSE,two.sided=TRUE)
  #grubbs.test(Final[,2],type=20,opposite=FALSE,two.sided=FALSE)
  
  
  ## First set of outliers excluded:
  Final2<-Final[Final$highVisit<30,]
  plot(Final2[,2])
  plot(Final2[,3])
  plot(Final2[,4])
  plot(Final2[,5])
  plot(Final2[,6])
  plot(Final2[,7])
  
  
  ## Second set of outliers excluded:
  plot(Final2[,2]*Final2[,3]*Final2[,4]*Final2[,5]*Final2[,6]*Final2[,7])
  Final3<-Final2[(Final2[,2]*Final2[,3]*Final2[,4]*Final2[,5]*Final2[,6]*Final2[,7])<0.5e+11,]
  plot(Final3[,2])
  plot(Final3[,3])
  plot(Final3[,4])
  plot(Final3[,5])
  plot(Final3[,6])
  plot(Final3[,7])
  
  
  ## Normalization of data:
  #  Removing the contribution of Average quantity
  norm2Final3=array(0,dim=dim(Final3))
  norm2Final3[,1]=Final3[,1]                   
  for(i in 2:3)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,2]+Final3[,3])/2)
  }             
  for(i in 4:5)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,4]+Final3[,5])/2)
  }             
  for(i in 6:7)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,6]+Final3[,7])/2)
  }             
  
  
  ## Dividing by the maximum of the corresponding column and multipying by 100.
  for(i in 2:7){
    norm2Final3[,i]=100*norm2Final3[,i]/max(norm2Final3[,(i+((i+1)%%2)*1)])
  }
  
  summary(norm2Final3)
  
  ## Required libraries for the next part
  
  library(cluster)
  require(graphics)                   
  library(clusterSim)                   
  
  
  ## k means clustering begins here. 
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(norm2Final3,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(norm2Final3[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }
  
  # We can judge the clustering quality through the below graphs:
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  # Finding optimum k 
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  # Re doing k means clustering using the optimum k 
  km = kmeans(subset(norm2Final3,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(norm2Final3[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(norm2Final3[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  
  # Finding the Euclidean distance from the center of the cluster to each of the data point
  library(fields)
  diss=array(0,dim=c(dim(norm2Final3)[1],1))
  cluCenters=km$centers
  clusterData=km$cluster
  for(i in 1:dim(norm2Final3)[1]){
    diss[i,1]=rdist(t(norm2Final3[i,2:7]),t(cluCenters[clusterData[i],]))
  }
  
  ## Creating new data in which we dont include the outliers that are causing the clustering to be unclear
  newNormData = array(0,dim=c(dim(norm2Final3)[1],dim(norm2Final3)[2]+1))
  newNormData[,1:7] = norm2Final3
  newNormData[,8]=diss
  newNormData2 = newNormData[diss<quantile(diss)[4],]
  
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }                   
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(newNormData2[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  
  library(fields)
  diss=array(0,dim=c(dim(newNormData2)[1],1))
  cluCenters=km$centers
  clusterData=km$cluster
  for(i in 1:dim(newNormData2)[1]){
    diss[i,1]=rdist(t(newNormData2[i,2:7]),t(cluCenters[clusterData[i],]))
  }
  
  ## Repeating the process once more where we remove outliers in the clusters. 
  newNormData3 = array(0,dim=c(dim(newNormData2)[1],dim(newNormData)[2]))
  newNormData3[,1:7] = newNormData2[,1:7]
  newNormData3[,8]=diss
  newNormData4 = newNormData3[diss<0.9*max(diss),]
  
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }                   
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(newNormData4[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  ## Obtaining the data and observing it
  
  library("outliers")
  Final=read.table("Final.csv", header=TRUE,sep=',')
  summary(Final)
  plot(Final[,2])
  plot(Final[,3])
  plot(Final[,4])
  plot(Final[,5])
  plot(Final[,6])
  plot(Final[,7])
  plot(Final[,2]*Final[,3]*Final[,4]*Final[,5]*Final[,6]*Final[,7])
  
  #chisq.out.test(Final[,3],variance=var(Final[,3]),opposite=FALSE)
  #dixon.test(Final[,2],type=0,opposite=FALSE,two.sided=TRUE)
  #grubbs.test(Final[,2],type=20,opposite=FALSE,two.sided=FALSE)
  
  
  ## First set of outliers excluded:
  Final2<-Final[Final$highVisit<30,]
  plot(Final2[,2])
  plot(Final2[,3])
  plot(Final2[,4])
  plot(Final2[,5])
  plot(Final2[,6])
  plot(Final2[,7])
  
  
  ## Second set of outliers excluded:
  plot(Final2[,2]*Final2[,3]*Final2[,4]*Final2[,5]*Final2[,6]*Final2[,7])
  Final3<-Final2[(Final2[,2]*Final2[,3]*Final2[,4]*Final2[,5]*Final2[,6]*Final2[,7])<0.5e+11,]
  plot(Final3[,2])
  plot(Final3[,3])
  plot(Final3[,4])
  plot(Final3[,5])
  plot(Final3[,6])
  plot(Final3[,7])
  
  
  ## Normalization of data:
  #  Removing the contribution of Average quantity
  norm2Final3=array(0,dim=dim(Final3))
  norm2Final3[,1]=Final3[,1]                   
  for(i in 2:3)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,2]+Final3[,3])/2)
  }             
  for(i in 4:5)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,4]+Final3[,5])/2)
  }             
  for(i in 6:7)      {
    norm2Final3[,i]=Final3[,i]-((Final3[,6]+Final3[,7])/2)
  }             
  
  
  ## Dividing by the maximum of the corresponding column and multipying by 100.
  for(i in 2:7){
    norm2Final3[,i]=100*norm2Final3[,i]/max(norm2Final3[,(i+((i+1)%%2)*1)])
  }
  
  summary(norm2Final3)
  
  ## Required libraries for the next part
  
  library(cluster)
  require(graphics)                   
  library(clusterSim)                   
  
  
  ## k means clustering begins here. 
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(norm2Final3,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(norm2Final3[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }
  
  # We can judge the clustering quality through the below graphs:
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  # Finding optimum k 
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  # Re doing k means clustering using the optimum k 
  km = kmeans(subset(norm2Final3,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(norm2Final3[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(norm2Final3[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  
  # Finding the Euclidean distance from the center of the cluster to each of the data point
  library(fields)
  diss=array(0,dim=c(dim(norm2Final3)[1],1))
  cluCenters=km$centers
  clusterData=km$cluster
  for(i in 1:dim(norm2Final3)[1]){
    diss[i,1]=rdist(t(norm2Final3[i,2:7]),t(cluCenters[clusterData[i],]))
  }
  
  ## Creating new data in which we dont include the outliers that are causing the clustering to be unclear
  newNormData = array(0,dim=c(dim(norm2Final3)[1],dim(norm2Final3)[2]+1))
  newNormData[,1:7] = norm2Final3
  newNormData[,8]=diss
  newNormData2 = newNormData[diss<quantile(diss)[4],]
  
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }                   
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  km = kmeans(subset(newNormData2,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(newNormData2[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(newNormData2[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  
  library(fields)
  diss=array(0,dim=c(dim(newNormData2)[1],1))
  cluCenters=km$centers
  clusterData=km$cluster
  for(i in 1:dim(newNormData2)[1]){
    diss[i,1]=rdist(t(newNormData2[i,2:7]),t(cluCenters[clusterData[i],]))
  }
  
  ## Repeating the process once more where we remove outliers in the clusters. 
  newNormData3 = array(0,dim=c(dim(newNormData2)[1],dim(newNormData)[2]))
  newNormData3[,1:7] = newNormData2[,1:7]
  newNormData3[,8]=diss
  newNormData4 = newNormData3[diss<0.9*max(diss),]
  
  
  dbIndex=array(0,dim=c(1,25))                   
  withinss=array(0,dim=c(1,25))                  
  for(k in 2:25){
    kcenters=k
    km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),kcenters,100)          
    curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
    dbIndex[1,k]=curr_dbIndex$DB
    withinss[1,k]=km$tot.withinss
    
  }                   
  plot(t(dbIndex))
  plot(t(withinss))                   
  
  
  for(k in 2:25){
    if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
      idealk=k
      break
    }
  }
  
  km = kmeans(subset(newNormData4,select=c(2,3,4,5,6,7)),idealk,500)          
  curr_dbIndex=index.DB(newNormData4[,2:7],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  clusplot(newNormData4[,2:7],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=TRUE,labels=4,lines=0)                   
  
  kcenters=k
  km = kmeans(subset(norm2Final3,select=c(2,3,4,5)),kcenters,100)          
  curr_dbIndex=index.DB(norm2Final3[,2:5],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  withinss[1,k]=km$tot.withinss
  
}                   
plot(t(dbIndex))
plot(t(withinss))                   


for(k in 2:25){
  if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
    idealk=k
    break
  }
}

km = kmeans(subset(norm2Final3,select=c(2,3,4,5)),idealk,500)          
curr_dbIndex=index.DB(norm2Final3[,2:5],km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)

clusplot(norm2Final3[,2:5],clus=km$cluster,color=TRUE,plotchar=FALSE,shade=TRUE,labels=0,lines=0)                   


library(fpc)
plotcluster(norm2Final3[,2:5],km$cluster)
