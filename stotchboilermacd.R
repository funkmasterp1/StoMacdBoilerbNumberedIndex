library(quantmod)
#x for Boilinger Bands
####################
## BOLINGER BANDS ##
####################

getSymbols("AMZN", src="yahoo", from="2013-01-01")
x <- na.omit(merge(AMZN, BBands(Cl(YHOO))))

x$sig <- NA

# Flat where Close crossed the mavg
x$sig[c(FALSE, diff(sign(Cl(x) - x$mavg), na.pad=FALSE) != 0)] <- 0
x$sig[Cl(x) > x$up] <- -1 # short when Close is above up
x$sig[Cl(x) < x$dn] <- 1 # long when Close is below dn
x$sig[1] <- 0 # flat on the first day
x$sig[nrow(x)] <- 0 # flat on the last day


# Fill in the signal for other times
x$sig <- na.locf(x$sig) # wherever sig is NA, copy previous value to next row

# Now Lag your signal to reflect that you can't trade on the same bar that 
# your signal fires
x$sig <- Lag(x$sig)
x$sig[1] <- 0 # replace NA with zero position on first row

####################
### STOCHASTICS ####
####################

y <- na.omit(merge(YHOO, stoch(Cl(YHOO))))

y$sig <- NA

fast.over.slow <- y$fastD > y$slowD
y$sig <- rep(0,nrow(y))
y$sig[which(diff(fast.over.slow) == 1 & y$slowD < 0.2)] <- 1
y$sig[which(diff(fast.over.slow) == -1 & y$slowD > 0.8)] <- -1
y$sig <- Lag(y$sig)
y$sig[1] <- 0

# Fill in the signal for other times
y$sig <- na.locf(y$sig) # wherever sig is NA, copy previous value to next row

# Now Lag your signal to reflect that you can't trade on the same bar that 
# your signal fires
y$sig <- Lag(y$sig)
y$sig[1] <- 0 

####################
###### MACD ########
####################

z <- na.omit(merge(YHOO, MACD(Cl(YHOO))))

z$sig <- NA

# Flat where  between crosses. Not sure how to write 
z$sig[c(FALSE, diff(sign(z$signal == z$macd), na.pad=FALSE) != 1)] <- 1
z$sig[z$signal > z$macd] <- -1 # short when Close is above up
z$sig[z$signal < z$macd] <- 1 # long when Close is below dn
z$sig[1] <- 0 # flat on the first day
z$sig[nrow(z)] <- 0 # flat on the last day

# Fill in the signal for other times
z$sig <- na.locf(z$sig) # wherever sig is NA, copy previous value to next row

# Now Lag your signal to reflect that you can't trade on the same bar that 
# your signal fires
z$sig <- Lag(z$sig)
z$sig[1] <- 0 

# Merge xyz by date and create new signal when all three conditions are met


all <- merge(x$sig,y$sig,z$sig)
all[is.na(all)] <- 0

all <- cbind(all,rowSums(all))


all[which(all[,4] == -2),]
#http://stackoverflow.com/questions/30364782/creating-trading-signals-in-r
