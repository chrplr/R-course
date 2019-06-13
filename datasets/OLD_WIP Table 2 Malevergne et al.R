# This file for replicating the table 2 of Malevergne et al. (2005)
library(Hmisc)

rm(list = ls())

setwd("~/Académique/Recherche/Stable law estimators for relative tail risk estimation/Data")
data<-read.csv("3markets.csv", header = T)

# calculate the logreturn
allmarkets<-as.matrix(data[-1])
dlallmarkets<-diff(log(allmarkets),lag = 1)

# split the dataframe between positive and negative return
dlallmarketspos<-dlallmarkets
dlallmarketspos[which(dlallmarketspos<0)]=NA
colnames(dlallmarketspos)<-paste(colnames(dlallmarketspos), "pos", sep="_")
dlallmarketsneg<-dlallmarkets
dlallmarketsneg[which(dlallmarketsneg>0)]=NA
colnames(dlallmarketsneg)<-paste(colnames(dlallmarketsneg), "neg", sep="_")
# take the absolute value of negative returns
dlallmarketsneg<--dlallmarketsneg 
datasplit<-as.data.frame(cbind(dlallmarketsneg, dlallmarketspos))

# sort the dataframe by column name
datasplit<-datasplit[,order(colnames(datasplit))]

# Set the percentile levels
percentile<-c(0, 0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.925, 0.95, 0.96, 0.97, 0.98, 0.99)
npercentile<-NROW(percentile)

# calculate the thresholds at various quantiles
result.table2.threshold_u<-apply(datasplit,2,function(x) 
  quantile(x, probs = percentile, na.rm = TRUE, names = F, type = 2))

# we get an error message if some quantiles are equal due to too small sample or patchy series
# we identify and remove the columns with duplicated quantiles
# test if any duplicated quantile by column
test<-apply(result.table2.threshold_u,2,function(x) duplicated(x))
# select the column names that have duplicates
drops<-names(which(colSums(test=="TRUE")>0))
# delete thes columns from the database
var.out.bool <- !names(datasplit) %in% drops
datasplit<-datasplit[,var.out.bool]
result.table2.threshold_u<-result.table2.threshold_u[,var.out.bool]

result.table2.quartile<-apply(datasplit, 2, function(x) 
  cut2(x, cuts=quantile(x, probs = percentile, na.rm = TRUE, names = F, type = 2),
                                oneval = FALSE))
result.table2.quartile<-apply(result.table2.quartile, 2, function(x) subset(x, !duplicated(x) & !is.na(x)))
result.table2.quartile<-apply(result.table2.quartile,2, sort)

# Calculate the size of each subsample corresponding to each quantile interval
# myFuninter function counts the number of elements corresponding to each quantile interval
# The command outer passes myFun to each element (r,c) of the matrix  result.table2.threshold_u
# Since the command outer only works on vectorized function, we must first vectorize myFun
# See Joran's answer at 
# https://stackoverflow.com/questions/7395397/how-to-apply-function-over-each-matrix-elements-indices
myFuninter<-function(r,c){sum(datasplit[,c]>=result.table2.threshold_u[r,c] & 
                                datasplit[,c]<result.table2.threshold_u[r+1,c], na.rm=T)}
myVecFuninter<-Vectorize(myFuninter,vectorize.args = c('r','c'))
result.table2.sizeinter <- outer(1:(npercentile-1),1:ncol(datasplit),myVecFuninter)

# Calculate the size of each subsample beyond each quantile
# myFunbeyond function counts the number of elements beyond each quantile
# outer(X, Y, FUN) aplly the function FUN to each couple of X and Y
myFunbeyond<-function(r,c){sum(datasplit[,c]>=result.table2.threshold_u[r,c], na.rm=T)}
myVecFunbeyond<-Vectorize(myFunbeyond,vectorize.args = c('r','c'))
result.table2.sizebeyond <- outer(1:npercentile,1:ncol(datasplit),myVecFunbeyond)

colnames(result.table2.sizeinter) <- colnames(result.table2.threshold_u)
colnames(result.table2.sizebeyond) <- colnames(result.table2.threshold_u)
rownames(result.table2.threshold_u)<-paste("Q", as.character(percentile), sep ="")
rownames(result.table2.sizeinter)<-paste("Q", as.character(c( "0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7",
                                                              "0.7-0.8", "0.8-0.9", "0.9-0.925", "0.925-0.95", "0.95-0.96", "0.96-0.97",
                                                              "0.97-0.98", "0.98-0.99")), sep ="")
rownames(result.table2.sizebeyond)<-paste("Q", as.character(percentile), sep ="")
rownames(result.table2.quartile)<-paste("Q", as.character(percentile), sep ="")

