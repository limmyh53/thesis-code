######################################################
### Preliminaries                                  ###
### - Import data                                  ###
### - Create data frame for training and test sets ###
### 7 quarterly lags                               ###
######################################################

### IMPORT DATA ###
CPI.raw <- read.csv('Data/RTDSM/cpiQvMd.csv')
unemp.raw <- read.csv('Data/Unemp/SeriesReport-20170707165216_abf9e0.csv')
oil.raw <- read.csv('Data/Oil/WTISPLC.csv')

# Compute inflation using latest CPI vintage data which is given by CPI17Q2
CPI <- ts(CPI.raw$CPI17Q2, start=c(1947,01), frequency=12)    # CPI time series
# Convert to quarterly CPI
CPI <- aggregate(CPI, nfrequency=4, mean)
# Inflation = Annualised quarter on quarter percentage change
inflation <- CPI/lag(CPI, -1) - 1
inflation <- ((1 + inflation)^4 - 1)*100

# Visualise inflation data (red is test set)
inflation.train <- window(inflation, start=c(1948,1), end=c(1997,4))
inflation.test <- window(inflation, start=c(1998,1), end=c(2017,1))
plot(inflation, xlim=c(1948, 2017), ylim=range(inflation), col='Black', 
     ylab='Inflation', main='CPI Inflation')
par(new=TRUE)
plot(inflation.test, xlim=c(1948, 2017), ylim=range(inflation), col='Red', xlab='', ylab='')

# Training set time keeping
start.train.target <- c(1949,4); end.train.target <- c(1997,4)
start.train.lag1 <- c(1949, 3); end.train.lag1 <- c(1997,3)
start.train.lag2 <- c(1949, 2); end.train.lag2 <- c(1997,2)
start.train.lag3 <- c(1949, 1); end.train.lag3 <- c(1997,1)
start.train.lag4 <- c(1948, 4); end.train.lag4 <- c(1996,4)
start.train.lag5 <- c(1948, 3); end.train.lag5 <- c(1996,3)
start.train.lag6 <- c(1948, 2); end.train.lag6 <- c(1996,2)
start.train.lag7 <- c(1948, 1); end.train.lag7 <- c(1996,1)

# Preprocess unemployment data
unemp <- c(t(as.matrix(unemp.raw[1:70, -1])))  # Vectorise raw unemployement data
unemp <- unemp[!is.na(unemp)]   # remove NA values (the NAs are there as placeholders for Aug 2017 onwards which are not yet released)
unemp <- ts(unemp, start=c(1948,01), frequency = 12)  # Convert to time series object
# Convert to quarterly
unemp <- aggregate(unemp, nfrequency=4, mean)
# train set
unemp.train.lag1 <- window(unemp, start=start.train.lag1, end=end.train.lag1)
unemp.train.lag2 <- window(unemp, start=start.train.lag2, end=end.train.lag2)
unemp.train.lag3 <- window(unemp, start=start.train.lag3, end=end.train.lag3)
unemp.train.lag4 <- window(unemp, start=start.train.lag4, end=end.train.lag4)
unemp.train.lag5 <- window(unemp, start=start.train.lag5, end=end.train.lag5)
unemp.train.lag6 <- window(unemp, start=start.train.lag6, end=end.train.lag6)
unemp.train.lag7 <- window(unemp, start=start.train.lag7, end=end.train.lag7)

# Preprocess Oil data
oil <- ts(oil.raw$WTISPLC, start=c(1946,1), frequency = 12)  # as time series object
# Convert to quarterly
oil <- aggregate(oil, nfrequency=4, mean)
# Percentage change in oil price = Annualised quarter on quarter percentage change
oil.percent <- oil/lag(oil, -1) - 1
oil.percent <- ((1 + oil.percent)^4 - 1)*100
# train set
oil.train.lag1 <- window(oil.percent, start=start.train.lag1, end=end.train.lag1)
oil.train.lag2 <- window(oil.percent, start=start.train.lag2, end=end.train.lag2)
oil.train.lag3 <- window(oil.percent, start=start.train.lag3, end=end.train.lag3)
oil.train.lag4 <- window(oil.percent, start=start.train.lag4, end=end.train.lag4)
oil.train.lag5 <- window(oil.percent, start=start.train.lag5, end=end.train.lag5)
oil.train.lag6 <- window(oil.percent, start=start.train.lag6, end=end.train.lag6)
oil.train.lag7 <- window(oil.percent, start=start.train.lag7, end=end.train.lag7)

# Train set
inflation.train.target <- window(inflation, start=start.train.target, end=end.train.target)
inflation.train.lag1 <- window(inflation, start=start.train.lag1, end=end.train.lag1)
inflation.train.lag2 <- window(inflation, start=start.train.lag2, end=end.train.lag2)
inflation.train.lag3 <- window(inflation, start=start.train.lag3, end=end.train.lag3)
inflation.train.lag4 <- window(inflation, start=start.train.lag4, end=end.train.lag4)
inflation.train.lag5 <- window(inflation, start=start.train.lag5, end=end.train.lag5)
inflation.train.lag6 <- window(inflation, start=start.train.lag6, end=end.train.lag6)
inflation.train.lag7 <- window(inflation, start=start.train.lag7, end=end.train.lag7)

df.train <- data.frame(inflation.target = inflation.train.target, 
                       inflation.lag1 = inflation.train.lag1,
                       inflation.lag2 = inflation.train.lag2,
                       inflation.lag3 = inflation.train.lag3,
                       inflation.lag4 = inflation.train.lag4,
                       inflation.lag5 = inflation.train.lag5,
                       inflation.lag6 = inflation.train.lag6,
                       inflation.lag7 = inflation.train.lag7,
                       unemp.lag1 = unemp.train.lag1,
                       unemp.lag2 = unemp.train.lag2,
                       unemp.lag3 = unemp.train.lag3,
                       unemp.lag4 = unemp.train.lag4,
                       unemp.lag5 = unemp.train.lag5,
                       unemp.lag6 = unemp.train.lag6,
                       unemp.lag7 = unemp.train.lag7,
                       oil.lag1 = oil.train.lag1,
                       oil.lag2 = oil.train.lag2,
                       oil.lag3 = oil.train.lag3,
                       oil.lag4 = oil.train.lag4,
                       oil.lag5 = oil.train.lag5,
                       oil.lag6 = oil.train.lag6,
                       oil.lag7 = oil.train.lag7)

# Test set (hold-out) time keeping
start.test.target <- c(1998, 1); end.test.target <- c(2017,1)
start.test.lag1 <- c(1997, 4); end.test.lag1 <- c(2016,4)
start.test.lag2 <- c(1997, 3); end.test.lag2 <- c(2016,3)
start.test.lag3 <- c(1997, 2); end.test.lag3 <- c(2016,2)
start.test.lag4 <- c(1997, 1); end.test.lag4 <- c(2016,1)
start.test.lag5 <- c(1996, 4); end.test.lag5 <- c(2015,4)
start.test.lag6 <- c(1996, 3); end.test.lag6 <- c(2015,3)
start.test.lag7 <- c(1996, 2); end.test.lag7 <- c(2015,2)

inflation.test.target <- window(inflation, start=start.test.target, end=end.test.target)
inflation.test.lag1 <- window(inflation, start=start.test.lag1, end=end.test.lag1)
inflation.test.lag2 <- window(inflation, start=start.test.lag2, end=end.test.lag2)
inflation.test.lag3 <- window(inflation, start=start.test.lag3, end=end.test.lag3)
inflation.test.lag4 <- window(inflation, start=start.test.lag4, end=end.test.lag4)
inflation.test.lag5 <- window(inflation, start=start.test.lag5, end=end.test.lag5)
inflation.test.lag6 <- window(inflation, start=start.test.lag6, end=end.test.lag6)
inflation.test.lag7 <- window(inflation, start=start.test.lag7, end=end.test.lag7)
# oil input for test
oil.test.lag1 <- window(oil.percent, start=start.test.lag1, end=end.test.lag1)
oil.test.lag2 <- window(oil.percent, start=start.test.lag2, end=end.test.lag2)
oil.test.lag3 <- window(oil.percent, start=start.test.lag3, end=end.test.lag3)
oil.test.lag4 <- window(oil.percent, start=start.test.lag4, end=end.test.lag4)
oil.test.lag5 <- window(oil.percent, start=start.test.lag5, end=end.test.lag5)
oil.test.lag6 <- window(oil.percent, start=start.test.lag6, end=end.test.lag6)
oil.test.lag7 <- window(oil.percent, start=start.test.lag7, end=end.test.lag7)
# unemp input for test
unemp.test.lag1 <- window(unemp, start=start.test.lag1, end=end.test.lag1)
unemp.test.lag2 <- window(unemp, start=start.test.lag2, end=end.test.lag2)
unemp.test.lag3 <- window(unemp, start=start.test.lag3, end=end.test.lag3)
unemp.test.lag4 <- window(unemp, start=start.test.lag4, end=end.test.lag4)
unemp.test.lag5 <- window(unemp, start=start.test.lag5, end=end.test.lag5)
unemp.test.lag6 <- window(unemp, start=start.test.lag6, end=end.test.lag6)
unemp.test.lag7 <- window(unemp, start=start.test.lag7, end=end.test.lag7)

df.test <- data.frame(inflation.target = inflation.test.target, 
                      inflation.lag1 = inflation.test.lag1,
                      inflation.lag2 = inflation.test.lag2,
                      inflation.lag3 = inflation.test.lag3,
                      inflation.lag4 = inflation.test.lag4,
                      inflation.lag5 = inflation.test.lag5,
                      inflation.lag6 = inflation.test.lag6,
                      inflation.lag7 = inflation.test.lag7,
                      unemp.lag1 = unemp.test.lag1,
                      unemp.lag2 = unemp.test.lag2,
                      unemp.lag3 = unemp.test.lag3,
                      unemp.lag4 = unemp.test.lag4,
                      unemp.lag5 = unemp.test.lag5,
                      unemp.lag6 = unemp.test.lag6,
                      unemp.lag7 = unemp.test.lag7,
                      oil.lag1 = oil.test.lag1,
                      oil.lag2 = oil.test.lag2,
                      oil.lag3 = oil.test.lag3,
                      oil.lag4 = oil.test.lag4,
                      oil.lag5 = oil.test.lag5,
                      oil.lag6 = oil.test.lag6,
                      oil.lag7 = oil.test.lag7)

# Augmented set. Useful for re-training for test
inflation.aug.target <- window(inflation, start=start.train.target, end=end.test.target)
inflation.aug.lag1 <- window(inflation, start=start.train.lag1, end=end.test.lag1)
inflation.aug.lag2 <- window(inflation, start=start.train.lag2, end=end.test.lag2)
inflation.aug.lag3 <- window(inflation, start=start.train.lag3, end=end.test.lag3)
inflation.aug.lag4 <- window(inflation, start=start.train.lag4, end=end.test.lag4)
inflation.aug.lag5 <- window(inflation, start=start.train.lag5, end=end.test.lag5)
inflation.aug.lag6 <- window(inflation, start=start.train.lag6, end=end.test.lag6)
inflation.aug.lag7 <- window(inflation, start=start.train.lag7, end=end.test.lag7)

unemp.aug.lag1 <- window(unemp, start=start.train.lag1, end=end.test.lag1)
unemp.aug.lag2 <- window(unemp, start=start.train.lag2, end=end.test.lag2)
unemp.aug.lag3 <- window(unemp, start=start.train.lag3, end=end.test.lag3)
unemp.aug.lag4 <- window(unemp, start=start.train.lag4, end=end.test.lag4)
unemp.aug.lag5 <- window(unemp, start=start.train.lag5, end=end.test.lag5)
unemp.aug.lag6 <- window(unemp, start=start.train.lag6, end=end.test.lag6)
unemp.aug.lag7 <- window(unemp, start=start.train.lag7, end=end.test.lag7)

oil.aug.lag1 <- window(oil.percent, start=start.train.lag1, end=end.test.lag1)
oil.aug.lag2 <- window(oil.percent, start=start.train.lag2, end=end.test.lag2)
oil.aug.lag3 <- window(oil.percent, start=start.train.lag3, end=end.test.lag3)
oil.aug.lag4 <- window(oil.percent, start=start.train.lag4, end=end.test.lag4)
oil.aug.lag5 <- window(oil.percent, start=start.train.lag5, end=end.test.lag5)
oil.aug.lag6 <- window(oil.percent, start=start.train.lag6, end=end.test.lag6)
oil.aug.lag7 <- window(oil.percent, start=start.train.lag7, end=end.test.lag7)


df.aug <- data.frame(inflation.target = inflation.aug.target, 
                     inflation.lag1 = inflation.aug.lag1,
                     inflation.lag2 = inflation.aug.lag2,
                     inflation.lag3 = inflation.aug.lag3,
                     inflation.lag4 = inflation.aug.lag4,
                     inflation.lag5 = inflation.aug.lag5,
                     inflation.lag6 = inflation.aug.lag6,
                     inflation.lag7 = inflation.aug.lag7,
                     unemp.lag1 = unemp.aug.lag1,
                     unemp.lag2 = unemp.aug.lag2,
                     unemp.lag3 = unemp.aug.lag3,
                     unemp.lag4 = unemp.aug.lag4,
                     unemp.lag5 = unemp.aug.lag5,
                     unemp.lag6 = unemp.aug.lag6,
                     unemp.lag7 = unemp.aug.lag7,
                     oil.lag1 = oil.aug.lag1,
                     oil.lag2 = oil.aug.lag2,
                     oil.lag3 = oil.aug.lag3,
                     oil.lag4 = oil.aug.lag4,
                     oil.lag5 = oil.aug.lag5,
                     oil.lag6 = oil.aug.lag6,
                     oil.lag7 = oil.aug.lag7)
