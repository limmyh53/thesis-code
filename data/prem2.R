### IMPORT DATA ####
spf.raw <- read.csv('SPF/Mean_CPI_Level.csv')
CPI.raw <- read.csv('SPF/cpiQvMd.csv')
unemp.raw <- read.csv('Unemp/SeriesReport-20170707165216_abf9e0.csv')
oil.raw <- read.csv('Oil/WTISPLC.csv')

# Compute inflation using latest CPI vintage data which is given by CPI17Q2
CPI <- ts(CPI.raw$CPI17Q2, start=c(1947,01), frequency=12)    # CPI time series
inflation <- 100*(CPI/lag(CPI, -12) - 1)     # inflation = annual percentage change in CPI
# Convert to quarterly
inflation <- aggregate(inflation, nfrequency=4, mean)

# Visualise inflation data (red is test set)
inflation.train <- window(inflation, start=c(1948,1), end=c(1997,4))
inflation.test <- window(inflation, start=c(1998,1), end=c(2017,1))
plot(inflation, xlim=c(1948, 2017), ylim=range(inflation), col='Black')
par(new=TRUE)
plot(inflation.test, xlim=c(1948, 2017), ylim=range(inflation), col='Red')

# Preprocess unemployment data
unemp <- c(t(as.matrix(unemp.raw[1:70, -1])))                  # Vectorise raw unemployement data
unemp <- ts(unemp, start=c(1948,01), frequency = 12)           # Convert to time series object
# Convert to quarterly
unemp <- aggregate(unemp, nfrequency=4, mean)
# train set
unemp.train <- window(unemp, start=c(1948,1), end=c(1997,4))
unemp.train.lag <- window(unemp.train, start=c(1948,1), end=c(1997,3))
# test set
unemp.test <- window(unemp, start=c(1998,1), end=c(2017,1))
unemp.test.lag <- window(unemp.test, start=c(1998,1), end=c(2016,4))

# Preprocess Oil data
oil <- ts(oil.raw$WTISPLC, start=c(1946,1), frequency = 12)  # as time series object
oil.percent <- 100*(oil/lag(oil, -12) - 1) # percentage change in oil price
# Convert tot quarterly
oil.percent <- aggregate(oil.percent, nfrequency=4, mean)
# train set
oil.train <- window(oil.percent, start=c(1948,1), end=c(1997,4))    # train set
oil.train.lag <- window(oil.train, start=c(1948,1), end=c(1997,3))
# test set
oil.test <- window(oil.percent, start=c(1998,1), end=c(2017,1))
oil.test.lag <- window(oil.test, start=c(1998,1), end=c(2016,4))

# Train set
inflation.train.target <- window(inflation.train, start=c(1948,2), end=c(1997,4))
inflation.train.lag1 <- window(inflation.train, start=c(1948,1), end=c(1997,3))

df.train <- data.frame(inflation.target = inflation.train.target, 
                       inflation.lag = inflation.train.lag1,
                       unemp.lag = unemp.train.lag,
                       oil.lag = oil.train.lag)

# Test set (hold-out)
inflation.test.target <- window(inflation.test, start=c(1998,2), end=c(2017,1))
inflation.test.lag1 <- window(inflation.test, start=c(1998,1), end=c(2016,4))

df.test <- data.frame(inflation.target = inflation.test.target, 
                      inflation.lag = inflation.test.lag1,
                      unemp.lag = unemp.test.lag,
                      oil.lag = oil.test.lag)

inflation.aug.target <- window(inflation, start=c(1948,2), end=c(2017,1))
inflation.aug.lag1 <- window(inflation, start=c(1948,1), end=c(2016,4))
unemp.aug.lag <- window(unemp, start=c(1948,1), end=c(2016,4))
oil.aug.lag <- window(oil.percent, start=c(1948,1), end=c(2016,4))


df.aug <- data.frame(inflation.target = inflation.aug.target, 
                     inflation.lag = inflation.aug.lag1,
                     unemp.lag = unemp.aug.lag,
                     oil.lag = oil.aug.lag)



