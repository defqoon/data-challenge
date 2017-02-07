## clean the workspace
rm(list=ls())

## Code
setwd("C:/Users/Thomas/Dropbox/Dropbox/CodingProjects/aerialintel-challenge/codes")
datadir = "C:/Users/Thomas/Dropbox/Dropbox/CodingProjects/aerialintel-challenge/data/"
figdir = "C:/Users/Thomas/Dropbox/Dropbox/CodingProjects/aerialintel-challenge/figs/"

################################### data pre-processing ######################################### 
# load the data
df2013 = read.csv(paste(datadir,"wheat-2013-supervised.csv",sep=""),header = TRUE)
df2014 = read.csv(paste(datadir,"wheat-2014-supervised.csv",sep=""),header = TRUE)


StatesList = unique(df2013[2])

# clean the data
toDrop = c("CountyName","State","Date")
df2013 = df2013[ , !(names(df2013) %in% toDrop)]
df2014 = df2014[ , !(names(df2014) %in% toDrop)]

# data cleaning
# look for missing data - using summary
missing2013 = df2013[!complete.cases(df2013),]
missing2014 = df2014[!complete.cases(df2014),]

# get ride of these lines 
df2013 = na.omit(df2013)
df2014 = na.omit(df2014)


################################### exploratory data analysis ################################### 
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

# look at some basics statistics
summary(df2013)
summary(df2014)

# one column is empty - drop it
toDrop = c("precipTypeIsOther")
df2013 = df2013[ , !(names(df2013) %in% toDrop)]
df2014 = df2014[ , !(names(df2014) %in% toDrop)]

### data visualization - plot densities ###

colNames = names(df2013[3:22])
plot_list = list()
j = 1
for (i in colNames)
{
  varName = names(df2013[i])
  p = ggplot() + geom_density(data = df2013,aes_string(x=i),fill = "red",alpha=.3) +  geom_density(data = df2014,aes_string(x=i),fill="blue",alpha=.3)
  plot_list[[j]] = p
  j = j + 1
}

pdf(paste(figdir,"distribution.pdf",sep = ""))
for (i in 1:length(plot_list)) 
{
  print(plot_list[[i]])
}
dev.off()


# potential collinearity
library(corrplot)
c2013 <- cor(df2013)
c2014 <- cor(df2014)

plot_list = list()
pdf(paste(figdir,"corrplots.pdf",sep = ""),width = 7,height = 7,paper = "a4r")
plot_list[[1]] = corrplot(c2013,method="circle",type = "upper")
plot_list[[2]] = corrplot(c2014,method="circle",type = "upper")
print(plot_list[[1]])
print(plot_list[[2]])
dev.off()






################################### first model - multiple linear regression ################################### 
# train and validate the model on year i - test it on year j
# need for regularization!!
# k fold cross validation



library(MASS)
library(ISLR)
library(caret)
library(glmnet)

lmlasso <- function(data)
{
  nfolds <- 10
  foldSize = round(nrow(data)/nfolds-1)
  index = seq(1,nrow(data))
  index = sample(index)
  #foldsIndex = split(index, ceiling(seq_along(index)/foldSize))
  
  mse = matrix(0,1,10)
  bestlmat = matrix(0,1,10)
  
  x = model.matrix(Yield~.,data)[,-1]
  y = data$Yield
  
  
  for (i in 1:nfolds)
  {
    # select the fold for testing
    tmpTest = index[(foldSize*(i-1)+1):(foldSize*i)]
    grid=10^seq(10,-2,length=100)
    lasso.model=glmnet(x[-tmpTest,],y[-tmpTest],alpha=1,lambda=grid)
    
    
    # cross validation on each k-1 fold to determine the best lambda
    cv.out = cv.glmnet(x[-tmpTest,],y[-tmpTest],alpha=1)
    bestl = cv.out$lambda.min
    
    if (i == 1){
      coefMat = coef(cv.out)
    }else{
      tempCoef = coef(cv.out)
      coefMat = cbind(coefMat,tempCoef)
    }
    # prediction on the vaidation fold
    lasso.pred = predict(lasso.model,s = bestl,newx = x[tmpTest,])
    
    bestlmat[i] = bestl
    mse[i] = mean((lasso.pred-y[tmpTest])^2)
  }
  # model is stable?
  hist(mse,breaks = length(mse),main = "Mean square error")
  
  output <- list("mse" = mse,"bestl" = bestlmat, "model" = lasso.model,"coef" = coefMat)
  return(output)
}

################################### Second model - PCR ################################### 
library(pls)


pcrpred <- function(data)
{
  nfolds <- 10
  foldSize = round(nrow(data)/nfolds-1)
  index = seq(1,nrow(data))
  index = sample(index)
  #foldsIndex = split(index, ceiling(seq_along(index)/foldSize))
  x = model.matrix(Yield~.,data)[,-1]
  y = data$Yield
  
  mse = matrix(0,1,10)
  comp = matrix(0,1,10)
  
  for (i in 1:nfolds)
  {
    # select the fold for testing
    tmpTest = index[(foldSize*(i-1)+1):(foldSize*i)]
    grid=10^seq(10,-2,length=100)
    
    pcr.model = pcr(Yield~.,data = data[-tmpTest,],scale = TRUE,validation = "CV")
    # 
    # if (i == 1){
    #   coefMat = coef(cv.out)
    # }else{
    #   tempCoef = coef(cv.out)
    #   coefMat = cbind(coefMat,tempCoef)
    # }
    # prediction on the vaidation fold
    ncomp = which(cumsum(pcr.model$Xvar/pcr.model$Xtotvar*100) > 90)[1]
    comp[i] = ncomp
    pcr.pred = predict(pcr.model,x[tmpTest,],M = ncomp)
    
    
    mse[i] = mean((pcr.pred-y[tmpTest])^2)
  }
  # model is stable?
  hist(mse,breaks = length(mse),main = "Mean square error")
  
  output <- list("mse" = mse, "comp" = comp, "model" = pcr.model)
  return(output)
}

################################### third model - best subset selection ################################### 
# train and validate the model on year i - test it on year j
# need for regularization!!
# k fold cross validation



library(MASS)
library(ISLR)
library(caret)
library(glmnet)
library(leaps)

lmsubset <- function(data)
{
  nfolds <- 10
  foldSize = round(nrow(data)/nfolds-1)
  index = seq(1,nrow(data))
  index = sample(index)
  #foldsIndex = split(index, ceiling(seq_along(index)/foldSize))
  
  mse = matrix(0,1,10)
  bestlmat = matrix(0,1,10)
  
  x = model.matrix(Yield~.,data)[,-1]
  y = data$Yield
  
  
  for (i in 1:nfolds)
  {
    # select the fold for testing
    tmpTest = index[(foldSize*(i-1)+1):(foldSize*i)]
    regfit.full=regsubsets (Yield~.,data = data[-tmpTest,],nvmax=22)
    
    if (i == 1){
      coefMat = coef(cv.out)
    }else{
      tempCoef = coef(cv.out)
      coefMat = cbind(coefMat,tempCoef)
    }
    # prediction on the vaidation fold
    lmsubset.pred = predict(regfit.full,newx = x[tmpTest,])
    
    mse[i] = mean((lmsubset.pred-y[tmpTest])^2)
  }
  # model is stable?
  hist(mse,breaks = length(mse),main = "Mean square error")
  
  output <- list("mse" = mse, "model" = lasso.model,"coef" = coefMat)
  return(output)
}


################################### outliers ################################### 
outliers2013 <- boxplot.stats(df2013$Yield)$out
ind <- which(df2013$Yield %in% outliers2013)
#df2013 = df2013[-ind,]

outliers2014 <- boxplot.stats(df2014$Yield)$out
ind <- which(df2014$Yield %in% outliers2014)
df2014 = df2014[-ind,]


################################### predictions ################################### 

# 2013 to forecast 2014
pcr2013 <- pcrpred(df2013)
lasso2013 <- lmlasso(df2013)

# calculate the CV estimate
CVlasso2013 = mean(lasso2013$mse)
CVpcr2013 = mean(pcr2013$mse)

# lasso is doing better than PCR  
# use the entire data to build the model
x2013 = model.matrix(Yield~.,df2013)[,-1]
y2013 = df2013$Yield
x2014 = model.matrix(Yield~.,df2014)[,-1]
y2014 = df2014$Yield

# train the model in 2013
grid=10^seq(10,-2,length=100)
lasso.model2013=glmnet(x2013,y2013,alpha=1,lambda=grid)
cv.out2013 = cv.glmnet(x2013,y2013,alpha=1)
bestl = cv.out2013$lambda.min

# prediction on the test data - 2014
lasso.pred2014 = predict(lasso.model2013,s = bestl,newx = x2014)
msepred1 = mean((lasso.pred2014-y2014)^2)

##########################
# 2014 to "forecast" 2013
pcr2014 <- pcrpred(df2014)
lasso2014 <- lmlasso(df2014)
CVlasso2014 = mean(lasso2014$mse)
CVpcr2014 = mean(pcr2014$mse)

# lasso is doing better than PCR  
# train the model in 2013
grid=10^seq(10,-2,length=100)
lasso.model2014=glmnet(x2014,y2014,alpha=1,lambda=grid)
cv.out2014 = cv.glmnet(x2014,y2014,alpha=1)
bestl = cv.out2014$lambda.min

# prediction on the test data - 2013
lasso.pred2013 = predict(lasso.model2014,s = bestl,newx = x2013)
msepred2 = mean((lasso.pred2013-y2013)^2)

################################### visualization ################################### 

png(paste(figdir,"prediction2013.png",sep=""))
qqplot(lasso.pred2013,y2013,xlab = "Forecast",ylab = "True", main = "Predicting 2013 using 2014",xlim = c(0,70),ylim = c(0,70)) 
abline(0,1)
dev.off()
png(paste(figdir,"prediction2014-3.png",sep=""))
qqplot(lasso.pred2014,y2014,xlab = "Forecast",ylab = "True", main = "Predicting 2014 using 2013",xlim = c(0,3),ylim = c(0,3)) 
abline(0,1)
dev.off()





