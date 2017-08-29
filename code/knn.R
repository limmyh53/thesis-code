#################################################################################################
# KNN regression                                                                                #
# Preliminaries --  load 'caret' library                                                        #
#                   source 'Models/importer/prem_v1.R'                                          #
#                                                                                               #
# Arguemnts --      warm.end: vector specifying end of warm up period in training set           # 
#                             this also specifies the rolling window length                     #
#                   val.size: size of each validation set (default=4)                           #
#                       lags: lag order of inputs                                               #
#             forecast.ahead: forecast horizon (support up to 4Q-ahead)                         #
#                 neighbours: vector of number of neighbours to be tuned using WFCV             #
#                update.rate: if update is set to TRUE, this determines therate at which model  #
#                             is retrained (default=4)                                          #
#################################################################################################

KNNReg <- function(warm.end, 
                   val.size=4, 
                   lags, 
                   forecast.ahead, 
                   neighbours,
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
  rmse <- matrix(numeric(length(neighbours)*k), ncol=length(neighbours)) # vector to store rmse
  
  # Create progress bar
  pb <- txtProgressBar(min=1, max=length(rmse), style=3)
  for (j in 1:length(neighbours)) {
    for (i in 1:k) {
      # Segment data by using which() function
      validIndex <- which(folds==i, arr.ind=TRUE) + max(warmIndex)
      validData <- df.train[validIndex,]
      trainIndex <- (val.size*(i-1) + 1):(validIndex[1]-1)
      trainData <- df.train[trainIndex,]
      
      # Train
      knn.model <- knnregTrain(train=trainData[paste(factors)], test=validData[paste(factors)], 
                         y=trainData[,1], k=neighbours[j])
      
      # Collect forecasts on validation set      
      Yhat.valid[validIndex] <- knn.model
      # Compute and store validation error
      error <- Yhat.valid[validIndex] - Y.valid[validIndex]
      rmse[i,j] <- sqrt(mean(error^2))
    }
    # Update progress bar
    setTxtProgressBar(pb, i*j+i)
  }
  close(pb)
  
  rmse.valid <- colMeans(rmse)     # Compute average validation error
  plot(x=neighbours, y=rmse.valid) # Plot lambdas against average validation error
  neighbours.hat <- neighbours[which.min(rmse.valid)] # Pick optimal number of neighbours

  #=====================================================
  # Fit fixed scheme using optimal number of neighbours
  #=====================================================
  # Roll training data
  window.train.index <- (nrow(df.train)-N.warm+1):nrow(df.train)
  # Fit using optimal number of neighbours
  knn.model <- knnregTrain(train=df.train[window.train.index,paste(factors)], 
                           y=df.train[window.train.index,1], k=neighbours.hat,
                           test=df.train[window.train.index,paste(factors)])
  
  # Plot of forecast on training set
  Yhat.train <- knn.model
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
  
  # Fit using optimal neighbours
  knn.model <- knnregTrain(train=df.train[window.train.index, paste(factors)], 
                           y=df.train[window.train.index,1], k=neighbours.hat,
                           test=df.test[,paste(factors)])
  
  # Test set performance of fixed scheme
  Yhat.fixed <- knn.model
  Yhat.fixed <- ts(Yhat.fixed, 
                  start=start(df.test$inflation.target), 
                  end=end(df.test$inflation.target), 
                  frequency=4)
  
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
  # Set temp to TRUE after 4 forecasts, this signals model to be retrained
  temp <- FALSE
  # Number of times retrained
  retrain.count <- 0
  # Store forecasts
  Yhat.update <- numeric(length(df.test$inflation.target)) 
  for (i in 1:length(Yhat.update)) {
    
    if (temp==TRUE) {
      retrain.count <- retrain.count + 1
      knn.model <- knnregTrain(train=df.aug[window.train.index+(4*retrain.count), -1], 
                               y=df.aug[window.train.index+(4*retrain.count), 1],
                               test=df.test[i,-1], 
                               k=neighbours.hat)
      Yhat.update[i] <- knn.model
      # Print number of time model has been retrained
      cat("Model retrained", retrain.count, "times", "(", i-1, "forecasts made )", "\n")
      
    }
    else {
      knn.model <- knnregTrain(train=df.aug[window.train.index+(4*retrain.count), paste(factors)], 
                               y=df.aug[window.train.index+(4*retrain.count), 1],
                               test=df.test[i, paste(factors)], 
                               k=neighbours.hat)
      Yhat.update[i] <- knn.model
    }

    # Print forecast vs actual
    cat("Forecast:", Yhat.update[i], "Actual:", df.test$inflation.target[i], "\n")
    
    if (i%%update.rate==0) {
      temp <- TRUE
    }
    else {
      temp <- FALSE
    }
  }
  
  
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
                 rmse.val=rmse,
                 neighbours.hat=neighbours.hat,
                 retrain.count=retrain.count,
                 N.warm=N.warm)

return(output)
}


