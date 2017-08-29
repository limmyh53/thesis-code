#################################################################################################
# Ridge regression                                                                              #
# Preliminaries --  load 'MASS' library                                                         #
#                   source 'Models/importer/prem_v1.R'                                          #
#                                                                                               #
# Arguemnts --      warm.end: vector specifying end of warm up period in training set           # 
#                             this also specifies the rolling window length                     #
#                   val.size: size of each validation set (default=4)                           #
#                       lags: lag order of inputs                                               #
#             forecast.ahead: forecast horizon (support up to 4Q-ahead)                         #
#                    lambdas: vector of ridge reg parameters to be tuned using WFCV             #
#                update.rate: if update is set to TRUE, this determines therate at which model  #
#                             is retrained (default=4)                                          #
#################################################################################################

RidgeReg <- function(warm.end, 
                     val.size=4, 
                     lags, 
                     forecast.ahead, 
                     lambdas, 
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
  rmse <- matrix(numeric(length(lambdas)*k), ncol=length(lambdas)) # vector to store rmse
  
  # Create progress bar
  pb <- txtProgressBar(min=1, max=length(rmse), style=3)
  for (j in 1:length(lambdas)) {
    for (i in 1:k) {
      # Segment data by using which() function
      validIndex <- which(folds==i, arr.ind=TRUE) + max(warmIndex)
      validData <- df.train[validIndex,]
      trainIndex <- (val.size*(i-1) + 1):(validIndex[1]-1)
      trainData <- df.train[trainIndex,]
      
      # Train
      ridge.reg <- lm.ridge(as.formula(paste("inflation.target~", paste(factors, collapse="+"))), 
                            data=trainData, lambda=lambdas[j])
      
      # Collect forecasts on validation set
      Yhat.valid[validIndex] <- as.matrix(validData[paste(factors)]) %*% coef(ridge.reg)[-1] + coef(ridge.reg)[1]
      # Compute and store validation error
      error <- Yhat.valid[validIndex] - Y.valid[validIndex]
      rmse[i,j] <- sqrt(mean(error^2))
    }
    # Update progress bar
    setTxtProgressBar(pb, i*j+i)
  }
  close(pb)
  
  rmse.valid <- colMeans(rmse)  # Compute average validation error
  # plot(y=rmse.valid,
  #      x=lambdas,
  #      ylab='Average validation error', 
  #      xlab = expression(paste(lambda))) # Plot lambdas against average validation error
  lambda.hat <- lambdas[which.min(rmse.valid)]  # Pick optimal lambdas
  
  #=====================================================
  # Fit fixed scheme using optimal lambda
  #=====================================================
  # Roll training data
  window.train.index <- (nrow(df.train)-N.warm+1):nrow(df.train)
  # Fit using optimal lambda
  ridge.reg <- lm.ridge(as.formula(paste("inflation.target~", paste(factors, collapse="+"))), 
                        data=df.train[window.train.index,], lambda=lambda.hat)
  # Parameters learnt for fixed scheme
  coef.fixed <- coef(ridge.reg)  
  
  # Plot of forecast on training set
  Yhat.train <- 
    as.matrix(df.train[window.train.index, paste(factors)]) %*% coef(ridge.reg)[-1] + coef(ridge.reg)[1]
  Yhat.train <- ts(Yhat.train, end=end(df.train$inflation.target), frequency=4)
  plot(Yhat.train, 
       col='Red', 
       ylim=range(c(Yhat.train, df.train$inflation.target)),
       ylab='Inflation', 
       main='Training Set')
  par(new=TRUE)
  plot(ts(df.train$inflation.target[window.train.index], end=end(df.train$inflation.target), frequency=4), 
       ylab='', 
       ylim=range(c(Yhat.train, df.train$inflation.target)))
  
  # Test set performance of fixed scheme
  Yhat.fixed <- as.matrix(df.test[,paste(factors)]) %*% coef(ridge.reg)[-1] + coef(ridge.reg)[1]
  Yhat.fixed <- ts(Yhat.fixed, 
                   start=start(df.test$inflation.target), 
                   end=end(df.test$inflation.target), 
                   frequency=4)
  
  # Plot of forecast on test set
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
    Yhat.update[i] <- as.matrix(df.test[i, paste(factors)]) %*% coef(ridge.reg)[-1] + coef(ridge.reg)[1]
    # Print forecast vs actual
    cat("Forecast:", Yhat.update[i], "Actual:", df.test$inflation.target[i], "\n")
    # Roll forward after every four forecasts
    if (i%%update.rate==0) {
      # Retrain
      ridge.reg <- lm.ridge(as.formula(paste("inflation.target~", paste(factors, collapse="+"))), 
                            data=df.aug[window.train.index+(i-forecast.ahead+1),], lambda=lambda.hat)
      
      retrain.count <- retrain.count + 1
      # Print number of time model has been retrained
      cat("Model retrained", retrain.count, "times (", i, "forecasts made )", "\n")
      
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
                 rmse.val=rmse,
                 lambda.hat=lambda.hat,
                 coef.fixed=coef.fixed,
                 coef.update=coef(ridge.reg),
                 retrain.count=retrain.count,
                 N.warm=N.warm)

return(output)
}


