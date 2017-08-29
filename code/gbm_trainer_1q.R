###########################################################
# This script trains 1Q-ahead GBM models                  #
# Preliminaries:                                          #
source("code/prem_v1.R") # Import data                    #
# Import library                                          #
library(xgboost)                                          #
# Import functions                                        #
source("code/gbm_v4.R")  # Import GBM function            #
###########################################################

# Reproducibilty
set.seed(123)

# 1q
gbm.1q.4lag <- gbm(warm.end=c(1965,4), lags = 4, forecast.ahead = 1, val.size = 4,
                   max_depth=c(3, 5, 7, 9), 
                   min_child_weight=c(0.75, 1, 1.5, 2),
                   gamma=c(0.1, 0.3, 0.5, 0.8),
                   eta=c(0.25, 0.5, 0.75, 0.85))
xgb.save(gbm.1q.4lag$gbm.fixed, "models/gbm/gbm.1q.4lag.fixed")
xgb.save(gbm.1q.4lag$gbm.update, "models/gbm/gbm.1q.4lag.update")

gbm.1q.3lag <- gbm(warm.end=c(1965,4), lags = 3, forecast.ahead = 1, val.size = 4,
                   max_depth=c(3, 5, 7, 9), 
                   min_child_weight=c(0.75, 1, 1.5, 2),
                   gamma=c(0.1, 0.3, 0.5, 0.8),
                   eta=c(0.25, 0.5, 0.75, 0.85))
xgb.save(gbm.1q.3lag$gbm.fixed, "models/gbm/gbm.1q.3lag.fixed")
xgb.save(gbm.1q.3lag$gbm.update, "models/gbm/gbm.1q.3lag.update")

gbm.1q.2lag <- gbm(warm.end=c(1965,4), lags = 2, forecast.ahead = 1, val.size = 4,
                   max_depth=c(3, 5, 7, 9), 
                   min_child_weight=c(0.75, 1, 1.5, 2),
                   gamma=c(0.1, 0.3, 0.5, 0.8),
                   eta=c(0.25, 0.5, 0.75, 0.85))
xgb.save(gbm.1q.2lag$gbm.fixed, "models/gbm/gbm.1q.2lag.fixed")
xgb.save(gbm.1q.2lag$gbm.update, "models/gbm/gbm.1q.2lag.update")

gbm.1q.1lag <- gbm(warm.end=c(1965,4), lags = 1, forecast.ahead = 1, val.size = 4,
                   max_depth=c(3, 5, 7, 9), 
                   min_child_weight=c(0.75, 1, 1.5, 2),
                   gamma=c(0.1, 0.3, 0.5, 0.8),
                   eta=c(0.25, 0.5, 0.75, 0.85))
xgb.save(gbm.1q.1lag$gbm.fixed, "models/gbm/gbm.1q.1lag.fixed")
xgb.save(gbm.1q.1lag$gbm.update, "models/gbm/gbm.1q.1lag.update")
