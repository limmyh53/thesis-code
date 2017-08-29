#################################################################################################
# SVR                                                                                           #
# Preliminaries --  load 'e1071' library                                                        #
#                   source 'Models/importer/prem_v1.R'                                          #
#                                                                                               #
# Arguemnts --      warm.end: vector specifying end of warm up period in training set           # 
#                             this also specifies the rolling window length                     #
#                   val.size: size of each validation set (default=4)                           #
#                       lags: lag order of inputs                                               #
#             forecast.ahead: forecast horizon (support up to 4Q-ahead)                         #
#                       cost: cost parameter                                                    #
#                      gamma: gamma parameter                                                   #
#                update.rate: if update is set to TRUE, this determines therate at which model  #
#                             is retrained (default=4)                                          #
#################################################################################################

SVR <- function(warm.end, 
                val.size=4, 
                lags, 
                forecast.ahead, 
                cost, 
                gamma=1, 
                kernel,
                update.rate=4) {
  #=====================================================
  # Set warm up period
  #=====================================================
  # Set length of warm up period
  N.warm <- length(window(df.train$inflation.target,
                          start=start(df.train$inflation.target),            
                          end=warm.end))   
  # Create index of initial warm up training set
  warmIndex <- 1:N.warm
  
  #=====================================================
  # Create folds for WFCV
  #=====================================================
  # Set number of folds
  k <- nrow(df.train[-warmIndex,])/val.size
  # Split the remaining training set (after rmeoving warm up period) into folds
  folds <- cut(seq(1, length(df.train$inflation.target[-warmIndex])),
                   breaks=k,
                   labels=FALSE)
  # Check folds are in even chunks
  # for (i in 1:max(folds)) {
  #   print(sum(folds==i))
  # }
  
  #=====================================================
  # Perform WFCV
  #=====================================================
  # Use factors to sort input features dynamically
  inf.factors <- character(lags)
  unemp.factors <- character(lags)
  oil.factors <- character(lags)
  for (l in 1:lags) {
    inf.temp <- paste("inflation.lag",l+(forecast.ahead-1),sep="")
    unemp.temp <- paste("unemp.lag",l+3,sep="")
    oil.temp <- paste("oil.lag",l+3,sep="")
    inf.factors[l] <- inf.temp
    unemp.factors[l] <- unemp.temp
    oil.factors[l] <- oil.temp
  }
  factors <- rbind(inf.factors, unemp.factors, oil.factors)
  
  Yhat.valid <- numeric(nrow(df.train)) # store validation forecasts   
  Y.valid <- df.train$inflation.target  # real values
  
  # Create empty (MATLAB style cell) array to store validation RMSEs
  rmse <- matrix(list(), length(gamma), length(cost))
  rmse.val <- numeric(k)
  rmse.mean <- matrix(numeric(length(gamma)*length(cost)), length(gamma), length(cost))
 
  # Create progress bar
  pb <- txtProgressBar(min=1, max=length(rmse), style=3)
  # Store validation rmse for each combination of gamma and cost
  for (n in 1:length(gamma)) {
    for (j in 1:length(cost)) {
      for (i in 1:k) {
        # Segment data by using which() function
        validIndex <- which(folds==i, arr.ind=TRUE) + max(warmIndex)
        validData <- df.train[validIndex,]
        trainIndex <- (val.size*(i-1) + 1):(validIndex[1]-1)
        trainData <- df.train[trainIndex,]
        
        # Train
        svr.model <- svm(as.formula(paste("inflation.target~", paste(factors, collapse="+"))),
                         data=trainData, kernel=kernel, type='eps-regression',
                         cost=cost[j], gamma=gamma[n])
        
        # Forecast on validation set
        Yhat.valid[validIndex] <- predict(svr.model, newdata=validData)
        
        # Compute validation error
        error <- Yhat.valid[validIndex] - Y.valid[validIndex]
        rmse.val[i] <- sqrt(mean(error^2))
      }
      
      # Store each vector
      rmse[[n,j]] <- rmse.val
      # Store average validation rmse for each permutation
      rmse.mean[n,j] <- mean(rmse[[n,j]])
    }
    # Update progress bar
    setTxtProgressBar(pb, n*j)
  }
  close(pb)
  
  # Pick optimal hyperparameters
  hat.index <- which(rmse.mean==min(rmse.mean), arr.ind=TRUE)
  gamma.hat <- gamma[hat.index[1]]
  cost.hat <- cost[hat.index[2]]
  plot(c(rmse.mean))
  
  #=====================================================
  # Fit fixed scheme using optimal number of neighbours
  #=====================================================
  # Roll training data
  window.train.index <- (nrow(df.train)-N.warm+1):nrow(df.train) # rolling training set window

  svr.model <- svm(as.formula(paste("inflation.target~", paste(factors, collapse="+"))),
                   data=df.train[window.train.index,], 
                   kernel=kernel, 
                   type='eps-regression',
                   cost=cost.hat, 
                   gamma=gamma.hat)
  
  svr.fixed <- svr.model
  
  # Plot of forecast on training set
  Yhat.train <- svr.fixed$fitted
  Yhat.train <- ts(Yhat.train, end=end(df.train$inflation.target), frequency=4)
  plot(Yhat.train,
       col='Red', 
       ylab='Inflation', 
       main='Training Set',
       ylim=range(c(Yhat.train, df.train$inflation.target)))
  par(new=TRUE)
  plot(ts(df.train$inflation.target[window.train.index], end=end(df.train$inflation.target), frequency=4), 
       ylab='', 
       ylim=range(c(Yhat.train, df.train$inflation.target)))
  
  # Test set performance of fixed scheme
  Yhat.fixed <- predict(svr.fixed, newdata=df.test[,paste(factors)])
  Yhat.fixed <- ts(Yhat.fixed, 
                  start=start(df.test$inflation.target), 
                  end=end(df.test$inflation.target), 
                  frequency = 4)
  plot(Yhat.fixed, 
       col='Red', 
       ylim=range(c(Yhat.fixed, inflation.test.target)),
       ylab='Inflation', 
       main='Test Set (fixed scheme)')
  par(new=TRUE)
  plot(df.test$inflation.target, 
       ylim=range(c(Yhat.fixed, inflation.test.target)),
       ylab='')
  
  # Error statistics
  error.fixed <- Yhat.fixed - df.test$inflation.target
  rmse.fixed <- sqrt(mean(error.fixed^2))
  mae.fixed <- mean(abs(error.fixed))

  #=====================================================
  # Fit updating scheme using optimal lambda
  #=====================================================
  # Number of times retrained
  retrain.count <- 0
  # Store forecasts
  Yhat.update <- numeric(length(df.test$inflation.target)) 
  for (i in 1:length(Yhat.update)) {
    
    Yhat.update[i] <- predict(svr.model, newdata=df.test[i, paste(factors)])

    # Roll forward
    if (i%%update.rate==0) {
      # re-train after producing four periods
      svr.model <- svm(as.formula(paste("inflation.target~", paste(factors, collapse="+"))),
                       data=df.aug[window.train.index+(i-forecast.ahead+1),], 
                       kernel=kernel, 
                       type='eps-regression',
                       cost=cost.hat, 
                       gamma=gamma.hat)
      
      # Print number of time model has been retrained
      retrain.count <- retrain.count + 1
      # Print forecast vs actual
      cat("Model retrained", retrain.count, "times", "(", i, "forecasts made )", "\n")
      
    } # End if
  } # End for
  
  
  # Convert forecast vector to time series object for ease of reading
  Yhat.update <- ts(Yhat.update, 
                    start=start(df.test$inflation.target), 
                    end=end(df.test$inflation.target), 
                    frequency = 4)
  
  # Plot forecast vs actual
  plot(Yhat.update, 
       col='Red', 
       ylim=range(c(Yhat.update, inflation.test.target)),
       ylab='Inflation',
       main='Test Set (updating scheme)')
  par(new=TRUE)
  plot(df.test$inflation.target, 
       ylim=range(c(Yhat.update, inflation.test.target)), 
       xlab='', 
       ylab='')
  
  # Error statistics
  error.update <- Yhat.update - df.test$inflation.target
  rmse.update <- sqrt(mean(error.update^2))
  mae.update <- mean(abs(error.update))
  
  # Function returns
  output <- list(Yhat.fixed=Yhat.fixed,
                 Yhat.update=Yhat.update,
                 error.fixed=error.fixed,
                 rmse.fixed=rmse.fixed,
                 mae.fixed=mae.fixed,
                 error.update=error.update,
                 rmse.update=rmse.update,
                 mae.update=mae.update,
                 mean.validation.rmse=rmse.mean,
                 svr.fixed=svr.fixed,
                 svr.update=svr.model,
                 cost.hat=cost.hat,
                 gamma.hat=gamma.hat,
                 retrain.count=retrain.count)

return(output)
}


