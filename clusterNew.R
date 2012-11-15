## Obtaining the data and observing it
allCallsNAN=read.table("clusterCallsNew.csv", header=TRUE,sep=';')

#seperating packet calls and other calls
#The MMS calls are only 25 in number and they can be kept seperate at the level of the sql dataset itself
MMSCalls=allCallsNAN[allCallsNAN$MMS1==1,]
nonMMSCalls=allCallsNAN[allCallsNAN$MMS1==0,]

beforenormallCalls=nonMMSCalls[nonMMSCalls$description!='Packet Data',]
allCalls=nonMMSCalls[nonMMSCalls$description!='Packet Data',]
packetCalls=nonMMSCalls[nonMMSCalls$description=='Packet Data',]
#normalizing duration of calls
allCalls$duration=allCalls$duration/mean(allCalls$duration)


library(cluster)
require(graphics)                   
library(clusterSim)                   


## k means clustering begins here. 
maxCluster=100
dbIndex=array(0,dim=c(1,maxCluster))                   
totwithinss=array(0,dim=c(1,maxCluster))
betweenss=array(0,dim=c(1,maxCluster))
toCluster <- subset(allCalls,select=c(8,10,11,12,13,15,17))
for(k in 2:maxCluster){
  km = kmeans(toCluster,k,1000)          
  curr_dbIndex=index.DB(toCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  totwithinss[1,k]=km$tot.withinss
  betweenss[1,k]=km$betweenss
}

# We can judge the clustering quality through the below graphs:
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))


#upon observing the graph of totwithinss vs number of clusters, we see that the totwithinss increases sharply below 9 and flattens out after approximately 14. So, the range of the ideal k lies between 7 and 15. We perform the clustering for each of these numbers to determine which k can give the best demarcation.

clusters=array(8,9,10,11,12,13,14,15)
for(cluster in 8:15){
  km = kmeans(toCluster,cluster,1000)          
  curr_dbIndex=index.DB(toCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)

  str=sprintf("kmCenterSize%d.csv",cluster)
  write.table(cbind(km$centers,km$size),str)
  str=sprintf("kmWithinssBetweenssDBIndex%d.csv",cluster)
  write.table(cbind(km$tot.withinss,km$betweenss,curr_dbIndex$DB),str)
}

#optimum k = 13
km = kmeans(toCluster,13,1000)  
clusplot(toCluster,clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)

str=sprintf("kmCenterSize%d.csv",cluster)
write.table(cbind(km$centers,km$size),str)
cluster11=as.matrix(km$cluster[km$cluster==11])
write.table(cluster11,"cluster11.csv")
clusterTable=read.table("cluster11.csv", header=TRUE, sep=' ')

#cluster number 
min=1000
for(i in 1:dim(clusterTable[1])){
  compare=allCalls[allCalls$rowNum==clusterTable[i,1],]$duration
  if(min>compare)
    min=compare
}

#On running this code, we found min to be 14.59931 which corresponds to 1512s in the original raw data.
#A new table was created with one more column added to the existing columns. The new column called longDuration1 
#is 1 for calls which have duration > 1512, otherwise 0
highlowCalls=read.table("highlowclusterCalls.csv",header=TRUE, sep=';')

#The above table, just like the allCallsNAN table contains the packetcalls, MMS calls. We need to seperate these calls
highlowMMSCalls=highlowCalls[highlowCalls$MMS1==1,]
highlownonMMSCalls=highlowCalls[highlowCalls$MMS1==0,]


highlowallCalls=highlownonMMSCalls[highlownonMMSCalls$description!='Packet Data',]
highlowpacketCalls=highlownonMMSCalls[highlownonMMSCalls$description=='Packet Data',]

#prefix bn=before normalization
bnhighCalls=highlowallCalls[highlowallCalls$longDuration1==1,]
bnlowCalls=highlowallCalls[highlowallCalls$longDuration==0,]
hCalls=bnhighCalls
lCalls=bnlowCalls


#normalizing duration of calls
hCalls$duration=hCalls$duration/mean(hCalls$duration)
lCalls$duration=lCalls$duration/mean(lCalls$duration)
#allCalls$duration=allCalls$duration/mean(allCalls$duration)


#######We first perfom the clustering on the hCalls. 
## k means clustering begins here. 
maxCluster=100
dbIndex=array(0,dim=c(1,maxCluster))                   
totwithinss=array(0,dim=c(1,maxCluster))
betweenss=array(0,dim=c(1,maxCluster))
htoCluster <- subset(hCalls,select=c(8,10,11,12,13,15,17))
for(k in 2:maxCluster){
  km = kmeans(htoCluster,k,1000)          
  curr_dbIndex=index.DB(htoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  totwithinss[1,k]=km$tot.withinss
  betweenss[1,k]=km$betweenss
}

# We can judge the clustering quality through the below graphs:
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))


#upon observing the graph of totwithinss vs number of clusters, we see that the totwithinss increases sharply below 9 and flattens out after approximately 14. So, the range of the ideal k lies between 7 and 15. We perform the clustering for each of these numbers to determine which k can give the best demarcation.
dbIndex=array(0,dim=c(1,15))                   
totwithinss=array(0,dim=c(1,15))
betweenss=array(0,dim=c(1,15))
for(cluster in 4:15){
  km = kmeans(htoCluster,cluster,1000)          
  curr_dbIndex=index.DB(htoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  
  dbIndex[1,cluster]=curr_dbIndex$DB
  totwithinss[1,cluster]=km$tot.withinss
  betweenss[1,cluster]=km$betweenss
  
  str=sprintf("hkmCenterSize%d.csv",cluster)
  write.table(cbind(cluster, km$centers,km$size),str)
  str=sprintf("hkmWithinssBetweenssDBIndex%d.csv",cluster)
  write.table(cbind(cluster, km$tot.withinss,km$betweenss,curr_dbIndex$DB),str)
}
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))

#optimum k = 8

km = kmeans(htoCluster,8,1000)          
clusplot(htoCluster[1:4],clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)





#######################################################################################################
#######clustering on the lCalls. 
## k means clustering begins here. 
maxCluster=100
dbIndex=array(0,dim=c(1,maxCluster))                   
totwithinss=array(0,dim=c(1,maxCluster))
betweenss=array(0,dim=c(1,maxCluster))
ltoCluster <- subset(lCalls,select=c(8,10,11,12,13,15,17))
for(k in 2:maxCluster){
  km = kmeans(ltoCluster,k,1000)          
  curr_dbIndex=index.DB(ltoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  totwithinss[1,k]=km$tot.withinss
  betweenss[1,k]=km$betweenss
}

# We can judge the clustering quality through the below graphs:
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))


#upon observing the graph of totwithinss vs number of clusters, we see that the totwithinss increases sharply below 9 and flattens out after approximately 14. So, the range of the ideal k lies between 7 and 15. We perform the clustering for each of these numbers to determine which k can give the best demarcation.
dbIndex=array(0,dim=c(1,23))                   
totwithinss=array(0,dim=c(1,23))
betweenss=array(0,dim=c(1,23))
for(cluster in 8:23){
  km = kmeans(ltoCluster,cluster,1000)          
  curr_dbIndex=index.DB(ltoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
#  str=sprintf("lkmClusters%d.csv",cluster)
#  write.table(km$cluster, str)

  dbIndex[1,cluster]=curr_dbIndex$DB
  totwithinss[1,cluster]=km$tot.withinss
  betweenss[1,cluster]=km$betweenss
  
  str=sprintf("lkmCenterSize%d.csv",cluster)
  write.table(cbind(cluster, km$centers,km$size),str)
  str=sprintf("lkmWithinssBetweenssDBIndex%d.csv",cluster)
  write.table(cbind(cluster, km$tot.withinss,km$betweenss,curr_dbIndex$DB),str)
}
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))

#optimum k = 10
km = kmeans(ltoCluster,10,1000)          
clusplot(ltoCluster,clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)


#######################################################################################################

#Performing the analysis for numbers
#Please note that these number correspond to non-MMS calls and non-packet data. In order to incorporate all the calls
#the data set imported has to be different

#MySQL query to modify:
#
#CREATE TABLE numbers AS SELECT myHashNum, COUNT(*) AS 'numCalls',AVG( duration ) AS  'avgduration', 
#AVG( weekend1weekday0 ) AS  'avgweekend1weekday0', AVG( day1night0 ) AS  'avgday1night0', 
#AVG( out1in0 ) AS  'avgout1in0', AVG( missed1 ) AS  'avgmissed1', AVG( SMS1 ) AS  'avgSMS1', 
#AVG( VoiceCall1 ) AS  'avgVoiceCall1', AVG( longDuration1 ) AS  'avglongDuration1'
#FROM  `clusterCallsNew` 
#WHERE packet1 =0
#AND MMS1 =0
#GROUP BY myHashNum
#


#prefix bn=before normalization
bnnumbers=read.table("numbers.csv",header=TRUE, sep=';')


#normalizing duration of calls
numbers=bnnumbers
numbers$numCalls=bnnumbers$numCalls/mean(bnnumbers$numCalls)
numbers$avgduration=bnnumbers$avgduration/mean(bnnumbers$avgduration)


#Clustering with several cluster centers to determine optimum k
maxCluster=20
dbIndex=array(0,dim=c(1,maxCluster))                   
totwithinss=array(0,dim=c(1,maxCluster))
betweenss=array(0,dim=c(1,maxCluster))
numtoCluster <- subset(numbers,select=c(2,3,4,5,6,7,8,9,10))
for(k in 2:maxCluster){
  km = kmeans(numtoCluster,k,1000)          
  curr_dbIndex=index.DB(numtoCluster,km$cluster,d=NULL,centrotypes="centroids",p=2,q=2)
  dbIndex[1,k]=curr_dbIndex$DB
  totwithinss[1,k]=km$tot.withinss
  betweenss[1,k]=km$betweenss
  
  str=sprintf("numkmCenterSize%d.csv",k)
  write.table(cbind(k,km$centers,km$size),str)
  str=sprintf("numkmWithinssBetweenssDBIndex%d.csv",k)
  write.table(cbind(k,km$tot.withinss,km$betweenss,curr_dbIndex$DB),str)
  
}

# We can judge the clustering quality through the below graphs:
plot(t(dbIndex))
plot(t(totwithinss))  
plot(t(betweenss))

#optimum clusters = 4
km = kmeans(numtoCluster,4,1000)

clusplot(numtoCluster,clus=km$cluster)

clusplot(numtoCluster,clus=km$cluster,color=TRUE,plotchar=TRUE,shade=FALSE,labels=4,lines=0)



#######################################################################################################
# Finding optimum k 
for(k in 2:maxCluster){
  if(dbIndex[1,k-1]>dbIndex[1,k]&&dbIndex[1,k+1]>dbIndex[1,k]){
    idealk=k
    break
  }
}

# Re doing k means clustering using the optimum k 
km = kmeans(toCluster,2,1000)          
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
