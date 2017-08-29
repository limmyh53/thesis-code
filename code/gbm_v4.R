#################################################################################################
# GBM regression                                                                                #
# Preliminaries --  load 'xgboost' library                                                      #
#                   source 'Models/importer/prem_v1.R'                                          #
#                                                                                               #
# Arguemnts --      warm.end: vector specifying end of warm up period in training set           # 
#                             this also specifies the rolling window length                     #
#                   val.size: size of each validation set (default=4)                           #
#                       lags: lag order of inputs                                               #
#             forecast.ahead: forecast horizon (support up to 4Q-ahead)                         #
#                  max_depth: maximum depth of a tree                                           #
#           min_weight_child: minimum sum of weights of all observations required in a child    #
#                      gamma: specifies the minimum loss reduction required to make a split     #
#                        eta: learning rate                                                     #
#                update.rate: this determines therate at which model is retrained (default=4)   #                                        
#################################################################################################


gbm <- function(warm.end, 
                val.size=4, 
                lags, 
                forecast.ahead, 
                max_depth, 
                min_child_weight, 
                gamma,
                eta=0.1,
                update.rate=4) {
  
  # Use factors to create formula dynamically
  inf.factors <- character(lags)
  unemp.factors <- character(lags)
  oil.factors <- character(lags)
  if (forecast.ahead==4) {
    for (l in 1:lags) {
      inf.temp <- paste("inflation.lag",l+3,sep="")
      unemp.temp <- paste("unemp.lag",l+3,sep="")
      oil.temp <- paste("oil.lag",l+3,sep="")
      inf.factors[l] <- inf.temp
      unemp.factors[l] <- unemp.temp
      oil.factors[l] <- oil.temp
    }
  } else if (forecast.ahead==1) {
    for (l in 1:lags) {
      inf.temp <- paste("inflation.lag",l,sep="")
      unemp.temp <- paste("unemp.lag",l,sep="")
      oil.temp <- paste("oil.lag",l,sep="")
      inf.factors[l] <- inf.temp
      unemp.factors[l] <- unemp.temp
      oil.factors[l] <- oil.temp
    }
  }
  factors <- rbind(inf.factors, unemp.factors, oil.factors)
  
  # Warm up period: up to 1965Q4
  N.warm <- length(window(df.train$inflation.target, start=start(df.train$inflation.target), end=warm.end))
  warmIndex <- 1:N.warm
  # Split folds so each validation set is one year long (this simulates how testing is done)
  k <- nrow(df.train[-warmIndex,])/val.size
  folds <- cut(seq(1, length(df.train$inflation.target[-warmIndex])), breaks=k, labels=FALSE)
  
  # Check folds are in yearly chunks (4Qs)
  # for (i in 1:max(folds)) {
  #   print(sum(folds==i))
  # }
  
  Yhat.valid <- numeric(nrow(df.train)) # store validation forecasts
  Y.valid <- df.train$inflation.target  # real values
  
  # Vector to store validation error
  rmse.val <- numeric(k)
  # Create 4D array to store mean validation score: gamma is first dim 
  #                                                 min_child_weight is second dim
  #                                                 max_depth is third dim
  #                                                 eta is fourth dim
  rmse.mean <- c(numeric(length(max_depth) * length(min_child_weight) * length(gamma) * length(eta)))
  rmse.mean <- array(rmse.mean, dim = c(length(max_depth), length(min_child_weight), length(gamma), length(eta)))
  
  # Keep track of number of iterations during validation stage
  sum.iters <- 0
  
  # create progress bar
  #pb <- txtProgressBar(min=1, max=length(rmse.mean), style=3)
  # Store validation rmse for each combination of hyperparameters
  cat("---- Begin", k, "fold walk-forawrd cross validation", "\n")
  for (s in 1:length(eta)) {
    for (r in 1:length(max_depth)) {
      for (q in 1:length(min_child_weight)) {
        for (p in 1:length(gamma)) {
          cat("---- Fitting: eta =", eta[s], 
              "| max_depth =", max_depth[r], 
              "| min_child_weight =", min_child_weight[q], 
              "| gamma =", gamma[p], "\n")
          for (i in 1:k) {
            # Segment data by using which() function
            validIndex <- which(folds==i, arr.ind=TRUE) + max(warmIndex)
            validData <- data.matrix(df.train[validIndex,])
            trainIndex <- (val.size*(i-1) + 1):(validIndex[1]-1)
            trainData <- data.matrix(df.train[trainIndex,])
            
            dvalidation <- xgb.DMatrix(data=validData[, paste(factors)], label=validData[,1])
            dtrain <- xgb.DMatrix(data=trainData[, paste(factors)], label=trainData[,1])
            
            # Train
            model <- xgb.train(data = dtrain,
                               nrounds = 100,
                               watchlist = list(validation=dvalidation),
                               params = list(max_depth=max_depth[r], 
                                             min_child_weight=min_child_weight[q], 
                                             gamma=gamma[p],
                                             eta=eta[s], 
                                             booster="gbtree", 
                                             objective="reg:linear",
                                             eval_metric="rmse"),
                               maximize = TRUE,
                               early_stopping_rounds = 15,
                               verbose = 0
            )
            
            sum.iters <- sum.iters + model$best_iteration
            
            # Forecast on validation set
            Yhat.valid[validIndex] <- predict(model, newdata=dvalidation)
            # Compute validation error
            error <- Yhat.valid[validIndex] - Y.valid[validIndex]
            rmse.val[i] <- sqrt(mean(error^2)) 
          }
          rmse.mean[p,q,r,s] <- mean(rmse.val)
          cat("---- Mean validation error:", mean(rmse.val), "\n", "\n")
        }
      }
      # Update progress bar
      #cat(p, q, r, "\n")
      #setTxtProgressBar(pb, s*r*p*q)
    }
    #setTxtProgressBar(pb, s*r*p*q)
  }
  #close(pb)
  
  # Index of optimal hyperparameters
  hat.index <- which(rmse.mean==min(rmse.mean), arr.ind=TRUE)[1,]
  gamma.hat <- gamma[hat.index[1]]
  min_child_weight.hat <- min_child_weight[hat.index[2]]
  max_depth.hat <- max_depth[hat.index[3]]
  eta.hat <- eta[hat.index[4]]
  iters.hat <- round(sum.iters / (length(max_depth)*length(min_child_weight)*length(gamma)*length(eta)*k))
  
  # Model with optimal hyperparameters
  window.train.index <- (nrow(df.train)-N.warm+1):nrow(df.train) # rolling training set window
  dtrain <- xgb.DMatrix(data=data.matrix(df.train[window.train.index, paste(factors)]), 
                        label=df.train[window.train.index, 1])
  model.hat <- xgb.train(data=dtrain,
                         nrounds=iters.hat,
                         params = list(max_depth=max_depth.hat, 
                                       min_child_weight=min_child_weight.hat,
                                       gamma=gamma.hat,
                                       eta=eta.hat, 
                                       booster="gbtree", 
                                       objective="reg:linear",
                                       eval_metric="rmse"),
                         verbose = 1)
  
  
  # make copy of fixed scheme
  model.fixed <- model.hat
  
  ### Training set
  Yhat.train <- predict(model.fixed, 
                        newdata=data.matrix(df.train[window.train.index,paste(factors)]))
  Yhat.train <- ts(Yhat.train, end=end(df.train$inflation.target), frequency=4)  
  plot(Yhat.train, 
       col="Red", 
       ylab="Inflation", 
       main="Training Set",
       ylim=range(c(Yhat.train, df.train$inflation.target)))
  par(new=TRUE)
  plot(ts(df.train$inflation.target[window.train.index], end=end(df.train$inflation.target), frequency=4),
       ylab="",
       ylim=range(c(Yhat.train, df.train$inflation.target)))
  
  ### Test: fixed scheme
  Yhat.fixed <- predict(model.hat, newdata=data.matrix(df.test[,paste(factors)]))
  Yhat.fixed <- ts(Yhat.fixed, 
                   start=start(df.test$inflation.target), end=end(df.test$inflation.target), 
                   frequency = 4)
  plot(Yhat.fixed, col="Red", ylim=range(c(Yhat.fixed, inflation.test.target)),
       ylab="Inflation", main="Test Set (fixed scheme)")
  par(new=TRUE)
  plot(df.test$inflation.target, ylim=range(c(Yhat.fixed, inflation.test.target)),
       ylab="")
  
  error.fixed <- Yhat.fixed - df.test$inflation.target
  rmse.fixed <- sqrt(mean(error.fixed^2))
  mae.fixed <- mean(abs(error.fixed))
  
  
  ### Test: Updating scheme
  retrain.count <- 0
  Yhat.test <- numeric(length(df.test$inflation.target)) 
  
  for (i in 1:length(Yhat.test)) {
    
    Yhat.test[i] <- predict(model.hat, newdata=data.matrix(df.test[i, paste(factors)]))
    
    trainData <- data.matrix(df.aug[window.train.index+(i-forecast.ahead+1),])
    dtrain <- xgb.DMatrix(data=trainData[, paste(factors)], label=trainData[, 1])
    
    if (i%%4==0) {
      model.hat <- xgb.train(data = dtrain,
                             nrounds = iters.hat,
                             params = list(max_depth=max_depth.hat, 
                                           min_child_weight=min_child_weight.hat,
                                           gamma=gamma.hat,
                                           eta=eta.hat, 
                                           booster="gbtree", 
                                           objective="reg:linear"),
                             verbose = 1)
      retrain.count <- retrain.count + 1
      cat("Model retrained", retrain.count, "times (", i, "forecasts made )", "\n", "\n")
    }
  }
  
  Yhat.test <- ts(Yhat.test, start=c(1998,1), end=c(2017,1), frequency = 4)
  plot(Yhat.test, col="Red", ylim=range(c(Yhat.test, df.test$inflation.target)),
       ylab="Inflation",
       main="Test Set: updating scheme")
  par(new=TRUE)
  plot(df.test$inflation.target, ylim=range(c(Yhat.test, inflation.test.target)), xlab="", ylab="")
  
  error.update <- Yhat.test - df.test$inflation.target
  rmse.update <- sqrt(mean(error.update^2))
  mae.update <- mean(abs(error.update))
  
  # Function returns
  output <- list(Yhat.fixed=Yhat.fixed,
                 Yhat.update=Yhat.test,
                 error.fixed=error.fixed,
                 rmse.fixed=rmse.fixed,
                 mae.fixed=mae.fixed,
                 error.update=error.update,
                 rmse.update=rmse.update,
                 mae.update=mae.update,
                 mean.validation.rmse=rmse.mean,
                 gbm.fixed=model.fixed,
                 gbm.update=model.hat,
                 gamma.hat=gamma.hat,
                 min_child_weight.hat=min_child_weight.hat,
                 max_depth.hat=max_depth.hat,
                 eta.hat=eta.hat,
                 retrain.count=retrain.count)
}