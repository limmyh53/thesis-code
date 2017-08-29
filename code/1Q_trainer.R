###########################################################
# This script trains 1Q-ahead RR, SVR, KNN and RF models  #
# Preliminaries:                                          #
source("code/prem_v1.R") # Import data                    #
# Import libraries                                        #
library(MASS)                                             #
library(e1071)                                            #
library(caret)                                            #
library(randomForest)                                     #
# Import functions                                        #
source("code/rr.R")   # RR function                       #
source("code/svr.R")  # SVR function                      #
source("code/knn.R")  # KNN function                      #
source("code/rf.R")   # RF function                       #
###########################################################

### Warmup period
warm.end <- c(1965,4)

### Ridge regression
set.seed(123)
# 1q
rr.1q.4lag <- RidgeReg(warm.end=warm.end, lags = 4, lambdas = 2^seq.int(-5, 5, 0.5), forecast.ahead = 1, 
                       val.size = 4)
rr.1q.3lag <- RidgeReg(warm.end=warm.end, lags = 3, lambdas = 2^seq.int(-5, 5, 0.5), forecast.ahead = 1, 
                       val.size = 4)
rr.1q.2lag <- RidgeReg(warm.end=warm.end, lags = 2, lambdas = 2^seq.int(-5, 5, 0.5), forecast.ahead = 1, 
                       val.size = 4)
rr.1q.1lag <- RidgeReg(warm.end=warm.end, lags = 1, lambdas = 2^seq.int(-5, 5, 0.5), forecast.ahead = 1, 
                       val.size = 4)

### SVR: Linear
set.seed(123)
# 1q
svr.linear.1q.4lag <- SVR(warm.end=warm.end, lags = 4, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-7, 1, 0.5), kernel="linear")
svr.linear.1q.3lag <- SVR(warm.end=warm.end, lags = 3, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-7, 1, 0.5), kernel="linear")
svr.linear.1q.2lag <- SVR(warm.end=warm.end, lags = 2, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-7, 1, 0.5), kernel="linear")
svr.linear.1q.1lag <- SVR(warm.end=warm.end, lags = 1, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-7, 1, 0.5), kernel="linear")

### SVR: Radial
set.seed(123)
# 1q
svr.radial.1q.4lag <- SVR(warm.end=warm.end, lags = 4, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-3, 3, 0.5), gamma=2^seq.int(-5,1,0.5),
                          kernel="radial")
svr.radial.1q.3lag <- SVR(warm.end=warm.end, lags = 3, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-3, 3, 0.5), gamma=2^seq.int(-5,1,0.5),
                          kernel="radial")
svr.radial.1q.2lag <- SVR(warm.end=warm.end, lags = 2, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-3, 3, 0.5), gamma=2^seq.int(-5,1,0.5),
                          kernel="radial")
svr.radial.1q.1lag <- SVR(warm.end=warm.end, lags = 1, forecast.ahead = 1, val.size = 4,
                          cost = 2^seq.int(-3, 3, 0.5), gamma=2^seq.int(-5,1,0.5),
                          kernel="radial")

### KNN
set.seed(123)
# 1q ahead
knn.1q.4lag <- KNNReg(warm.end=warm.end, lags = 4, forecast.ahead = 1, val.size = 4,
                      neighbours = 1:50)
knn.1q.3lag <- KNNReg(warm.end=warm.end, lags = 3, forecast.ahead = 1, val.size = 4,
                      neighbours = 1:50)
knn.1q.2lag <- KNNReg(warm.end=warm.end, lags = 2, forecast.ahead = 1, val.size = 4,
                      neighbours = 1:50)
knn.1q.1lag <- KNNReg(warm.end=warm.end, lags = 1, forecast.ahead = 1, val.size = 4, 
                      neighbours = 1:50)


### RF
set.seed(123)
# 1q
rf.1q.4lag <- RF(warm.end=warm.end, lags = 4, forecast.ahead = 1, val.size = 4, 
                 mtry=c(1:(3*4)), nodesize=c(1,3,5,8), ntree=1000)
rf.1q.3lag <- RF(warm.end=warm.end, lags = 3, forecast.ahead = 1, val.size = 4,
                 mtry=c(1:(3*3)), nodesize=c(1,3,5,8), ntree=1000)
rf.1q.2lag <- RF(warm.end=warm.end, lags = 2, forecast.ahead = 1, val.size = 4,
                 mtry=c(1:(3*2)), nodesize=c(1,3,5,8), ntree=1000)
rf.1q.1lag <- RF(warm.end=warm.end, lags = 1, forecast.ahead = 1, val.size = 4,
                 mtry=c(1:(3*1)), nodesize=c(1,3,5,8), ntree=1000)
