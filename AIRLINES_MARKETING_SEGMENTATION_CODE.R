#######----- K-MEANS CLUSTERING - CASE STUDY 1------------------#######

##PROBLEM STATEMENT:
#Q1. Which TWO variables have (on average) the smallest values and largest values? 

#Q2. In this problem, we will normalize our data before we run the clustering algorithms. 
#In the normalized data, which variable has the largest maximum and smallest minimum value? 
#  (Hint: Use the preProcess and predict function from CARET package to normalize the data). 

#Q3. Hierarchical clustering: Compute the distances between data points (using euclidean 
#distance) and then run the Hierarchical clustering algorithm 
#(using method="ward.D") on the normalized data.

rm(list = ls())

list.of.packages <- c("datasets", "ggplot2","cluster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
install.packages("cluster", dependencies = TRUE)
library(datasets)
library(ggplot2)
library(cluster)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"C:/Users/sai ram/Desktop/CHAVI_IVY/MODULE-4_R/CASE STUDIES/CLUSTERING/CASE STUDY1/02DATA"

setwd(Path)
getwd()

cdata<-read.csv("AirlinesCluster.csv",header = TRUE)
cdata_copy = cdata#To create a backup of original data


#--------------------------------------Exploring the dataset-----------------------------------#
#Understand the data type and summary of each coloumn
str(cdata)  # all are integers
summary(cdata)  

#Checking missing values
as.data.frame(colSums(is.na(cdata)))  ## NO MISSING VALUES 

#QUESTION 1: 
m1<- mean(cdata$Balance) #highest mean
m2<- mean(cdata$QualMiles)
m3<- mean(cdata$BonusMiles)
m4<- mean(cdata$BonusTrans) # lowest mean
m5<- mean(cdata$FlightMiles)
m6<- mean(cdata$DaysSinceEnroll)

nam<- c("Balance","QualMiles", "BonusMiles", "BonusTrans","FlightMiles","DaysSinceEnroll")
mean_data<- as.data.frame(rbind(m1,m2,m3,m4,m5,m6),nam)
mean_data
min(mean_data) 
max(mean_data)

###--- Answer 1: a) The variable which has highest value(on average)=> Balance
###----          b) The variable which has lowest vales(on avergae)=> BonusTrans

##--QUESTION 2:
#Normalizing the Data for clustering 
library(caret)
preproc<-preProcess(cdata)
airlinesNorm<-predict(preproc,cdata)
summary(airlinesNorm)

print(min(airlinesNorm))  ## -1.993361
print(max(airlinesNorm))  ## 21.68029

## QUESTION 3: Hierarchical clustering:

#Hiearchical Clustering
distan<-dist(airlinesNorm, method = "euclidean")
ClusterAirline<-hclust(distan, method = "ward.D")
plot(ClusterAirline)

## QUESTION 4: 
#Assigning 5 points to the clusters
AirlineCluster<-cutree(ClusterAirline, k = 5)
table(AirlineCluster)

## number of data points in cluster 1= 776

## QUESTION 5:
#Computing the average values of the cluster groups
MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)
  print(z)
}

#1) for 'balance' variable:
Bal_mean<-MeanComp(cdata$Balance, AirlineCluster, mean)

#2) for 'DaysSinceEnroll' variable:
Bal_DaysSinceEnroll<-MeanComp(cdata$DaysSinceEnroll, AirlineCluster, mean)

#3) for 'QualMiles' variable:
Bal_QualMiles<-MeanComp(cdata$QualMiles, AirlineCluster, mean)

#4) for 'BonusMiles' variable:
Bal_BonusMiles<-MeanComp(cdata$BonusMiles, AirlineCluster, mean)

#5) for 'BonusTrans' variable:
Bal_BonusTrans<-MeanComp(cdata$BonusTrans, AirlineCluster, mean)

#6) for 'FlightMiles' variable:
Bal_FLighMiles<-MeanComp(cdata$FlightMiles, AirlineCluster, mean)

#7) for 'FlightTrans' variable:
Bal_FlightTrans<-MeanComp(cdata$FlightTrans, AirlineCluster, mean)

#Appending the Clusters Assignment
Airlines_H<-data.frame(cdata,AirlineCluster)
#write.csv(Airlines_H,"Airlines_Hierarchical.csv", row.names = FALSE)

## QUESTION 6:
#k-Means Clustersing
set.seed(88)
k<-5
AirlineCluster_K<-kmeans(airlinesNorm, centers = k,iter.max = 1000)
table(AirlineCluster_K$cluster)
AirlineCluster_K$centers

#Finding out the Mean Values of the Variables in the Clusters
Bal_mean_k<-aggregate(cdata, by=list(cluster=AirlineCluster_K$cluster), mean)
Bal_mean<-MeanComp(cdata$Balance, AirlineCluster, mean)


#Appending the Clusters Assignment
airlines_new_k <- data.frame(cdata, AirlineCluster_K$cluster)
#write.csv(airlines_new_k,"Airlines_k-Means.csv", row.names = FALSE)







