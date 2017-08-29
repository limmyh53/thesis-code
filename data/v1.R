##############################################
### N LAG Regression: M QUARTERS AHEAD   ###
### UPDATING SCHEME                 ###
##############################################

InflationClass <- setRefClass(
  "InflationClass",
  methods = list(
    importer = function()
    {
      # This function imports the relevant datasets
      spf.raw <<- read.csv('SPF/Mean_CPI_Level.csv')
      CPI.raw <<- read.csv('SPF/cpiQvMd.csv')
      unemp.raw <<- read.csv('Unemp/SeriesReport-20170707165216_abf9e0.csv')
      oil.raw <<- read.csv('Oil/WTISPLC.csv')
      
      # Compute inflation using latest CPI which is given by CPI17Q2
      CPI <<- ts(data.frame(CPI = CPI.raw$CPI17Q2), 
                start=c(1947,01), frequency=12)    # CPI time series
      inflation <<- 100*(CPI/lag(CPI, -12) - 1)     # inflation = annual percentage change in CPI
    }
  )
)