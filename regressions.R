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

i=12
    if (is.na(tail(x[,i],1))) next
    series <- as.numeric( as.character(x[,i]) ) 
    com <- series * deflator
    XTScom <- xts(com,dateDum)
    XTScom <- na.omit(XTScom)
    blue = FALSE
    if ( index(head(XTScom,1)) < '1972-01-01' ) {
        blue = TRUE
        blueLen <- length(XTScom['/1972-01-01'])
    }
    com <- as.numeric(XTScom)
    
    du1 <- com[-(1:timeLag)]
    if ( timeLag >= length(com) ) next
    du2 <- com[1:(length(com)-timeLag)]
    ret <- du1 / du2
    title <- names(x[i])
    xTitle <- 'Price in 2017 $'
    xTitle <- paste('Price in', format(Sys.Date(), '%Y'), '$')
    xAxis  <- c(min(com), max(com))
    yAxis  <- c(min(ret), max(ret))
    yTitle <- paste( timeLag/12, 'Year Forward Return', sep='' )
    g=length(ret)
    if (g>320) br = g-320 else br=5
    if (blue == TRUE) {
        plot(ret[blueLen:br]~du2[blueLen:br], pch=1, main=title, log='xy'
             , xlab=xTitle, ylab=yTitle, xlim=xAxis, ylim=yAxis)
        points(ret[br:g] ~ du2[br:g], col='red')
        points(ret[1:blueLen] ~ du2[1:blueLen], col='blue')
    }    
    if (blue == FALSE) {
        plot(ret[1:br]~du2[1:br], pch=1, main=title, log='xy'
             , xlab=xTitle, ylab=yTitle, xlim=xAxis, ylim=yAxis)
        points(ret[br:g] ~ du2[br:g], col='red')
    }    
    
    abline( v = tail(com,1) , col='red',lwd=2 )
    grid(col='blue', lwd = 1, lty = 3)
    avg <- mean(com) ; std <- sd(com)
    numSD <- format( ( tail(com,1) - avg ) / std, digits = 3 )
    ddum <- 'over'
    if (numSD < 0.0) ddum <- 'under'
    print( paste( title, 'is', numSD, 'stnd devs', ddum, 'the average   '))
    
    regY  <- log(ret)
    regX1 <- log(du2)
    regX2 <-  seq(1:length(ret))
    regX3 <- regX2 * regX2
    regX4 <- regX3 * regX2
    OLSregression1 <- lm( regY ~ regX1)
    OLSregression2 <- lm( regY ~ regX1 + regX2 + regX3 + regX4)
    regX1 <- log(2.3)
    regX2 <- tail(regX2,1)
    regX3 <- tail(regX3,1)
    regX4 <- tail(regX4,1)
    newdata1=data.frame( regX1, regX2 )
    newdata2=data.frame( regX1, regX2, regX3, regX4 )
    summary(OLSregression1)
    summary(OLSregression2)
    currForecast1 <- predict(OLSregression1,newdata = newdata1)
    currForecast1 <- format( exp(currForecast1), digits = 3)
    print(paste('Price Only Forecast =', currForecast1))
    currForecast2 <- predict(OLSregression2,newdata = newdata2)
    currForecast2 <- format( exp(currForecast2), digits = 3)
    print(paste('All Variable Forecast =', currForecast2))
