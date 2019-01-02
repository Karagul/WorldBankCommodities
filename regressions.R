library(xts)
timeLag <- 120
# read in the data from the csv file
x <- read.csv('WorldBankData.csv')

# replace '..' and '0.00' with NA
x[x=='..'] <- NA
x[x=='0.00'] <- NA

# create a date vector
#dateDum <- as.Date(x[,1])
dateDum=as.Date(x[,1], '%m/%d/%Y')
# take out the date column
x <- x[ ,-1 ]

# calculate the deflator
deflator <- x$usd / x$PCEPI * tail(x$PCEPI,1) / tail(x$usd,1)
# getting the plots

i=2
lastPrice <- 56
rollLength <- 11
    if (is.na(tail(x[,i],1))) next
    series <- as.numeric( as.character(x[,i]) ) 
    com <- na.omit(series * deflator)
    
    du1 <- com[-(1:timeLag)]
    if ( timeLag >= length(com) ) next
    du2 <- com[1:(length(com)-timeLag)]
    ret <- du1 / du2
    
    avg <- mean(com) ; std <- sd(com)
    numSD <- format( ( tail(com,1) - avg ) / std, digits = 3 )
    ddum <- 'over'
    if (numSD < 0.0) ddum <- 'under'
    print( paste( names(x[i]), 'is', numSD, 'stnd devs', ddum, 'the average   '))
    
    newCom <- rollmean( com, rollLength)
    addDum <- (rollLength - 1) / 2
    newCom <- c( head(com,addDum), newCom, tail(com,addDum))
    du2    <- newCom[1:(length(com)-timeLag)]
    regY  <- log(ret)
    regX1 <- log(du2)
    l <- length(ret)
    ll <- l -60
    regX2 <-  seq(1:l)
    maxTime<- regX2[ll]
    regX2[ll:l]<- maxTime
    regX3 <- regX2 * regX2
    regX4 <- regX3 * regX2
    OLSregression1 <- lm( regY ~ regX1)
    OLSregression2 <- lm( regY ~ regX1 + regX2) # + regX3) # + regX4)
    regX1 <- log(lastPrice)
    regX2 <- (tail(regX2,1))
    regX3 <- regX2^2
    regX4 <- regX2 * regX3
    newdata1=data.frame( regX1, regX2 )
    newdata2=data.frame( regX1, regX2) #, regX3)#, regX4 )
    print(summary(OLSregression1))
    print(summary(OLSregression2))
    currForecast1 <- predict(OLSregression1,newdata = newdata1)
    currForecast1 <- format( exp(currForecast1), digits = 3)
    print(paste('Price Only Forecast =', currForecast1))
    currForecast2 <- predict(OLSregression2,newdata = newdata2)
    currForecast2 <- format( exp(currForecast2), digits = 3)
    print(paste('All Variable Forecast =', currForecast2))
