# 
# Beergarden Analysis 
# (c) 2018 Etherisc GmbH
# @author: Philipp Terhorst, Christoph Mussenbrock
# revision V03, transfer excel to R
#

#load libraries
library(plyr) 
library(anytime)

# read raw data
beergarden <- read.csv("beergarden.csv")
getYear <- function(date) {
  return (as.POSIXlt(anydate(date))$year + 1900)  
}

# only select hours from 9:00 - 20:00
hoursRaw <- 10:19
hours <- 2+hoursRaw

# only select 
daysOfWeek <- c(6,0)

# only select months (January = 0) 
monthsRaw <- 5:9
monthsOfYear <- monthsRaw - 1

# select min or max per day
minMax <- max

# define thresholds
minRain <- 0.4
maxRain <- 999.0

# define % cost, tax
costs <- 0.2
tax <- 0.19

# define insured sum
insuredSum <- 100000.0

# inputEventThreshold
eventThreshold <- 0 # 0 in case of using threshold proposal, otherwise Threshold is used for probabiltiy calculation

# calculate historical data
wdays <- as.POSIXlt(beergarden$date)$wday
months <- as.POSIXlt(beergarden$date)$mon

calcPremium <- function(hoursOfDay, daysOfWeek, monthsOfYear, insuredSum, eventThreshold, minRain, verbose = FALSE) {
  selectedDays <- beergarden[wdays %in% daysOfWeek & months %in% monthsOfYear, ]
  presel <- data.frame(cbind(year=getYear(selectedDays[, 1]), maxRain = apply(selectedDays[,hoursOfDay], 1, minMax, na.rm=TRUE)))
  eventsPerYear <- count(presel[presel$maxRain >= minRain & presel$maxRain <= maxRain ,], "year")
  eventsPerYear <- merge(eventsPerYear, data.frame(year=1996:2017), by="year", all=TRUE)  # only full years should be considered
  
  eventsPerYear[is.na(eventsPerYear)] <- 0.0    # Maybe another option? it is not clear if the year has had events or data leck!
  maxEventsPerYear <- max(eventsPerYear$freq)   # for monitoring reasons
  minEventsPerYear <- min(eventsPerYear$freq)   # for monitoring reasons
  meanEventsPerYear <- mean(eventsPerYear$freq) # mean value for automated eventThreshold calculation
  stdDev <- sd(eventsPerYear$freq)
  
  # define target number of events
  if (eventThreshold <= 0) {eventThreshold <- ceiling(meanEventsPerYear + 2*stdDev)}   #as proposal
  
  totalEvents <- round(length(daysOfWeek) * 52.0 * length(monthsOfYear)/12.0)
  totalEvents <- as.integer(totalEvents)                                      #to avoid errors for proability calculation
  probability <- 1-pbinom(eventThreshold-1, totalEvents, mean(eventsPerYear$freq)/totalEvents)
  totalHits <- length(presel[presel$maxRain >= minRain & presel$maxRain <= maxRain,]$year)
  totalHoursCount <- length(presel$year)
  netPremium <- insuredSum * probability
  grossPremium <- netPremium * (1+costs)
  grossPremiumTaxed <- grossPremium * (1 + tax)
  
  if (verbose) {
    # Output Parameters
    cat("Selected months    : ", monthsRaw, "\n")
    cat("Selected days      : ", daysOfWeek, " (Sunday = 0)\n")
    cat("Selected hours     : ", hoursRaw, "\n")
    cat("Insured sum        : ", insuredSum, "\n")
    cat("Min. rain          : ", minRain, "\n")
    cat("Max. rain          : ", maxRain, "\n")
    cat("Costs              : ", costs, "\n")
    cat("Event Threshold #  : ", eventThreshold, "\n")
    
    #Output results
    cat("Min. rainy days    : ", min(eventsPerYear$freq), "\n")
    cat("Max. rainy days    : ", max(eventsPerYear$freq), "\n")
    cat("Mean rainy days    : ", mean(eventsPerYear$freq), "\n")
    cat("Total Events p.a.  : ", totalEvents, "\n")
    cat("Total hour count   : ", totalHoursCount, "\n")
    cat("Total hits         : ", totalHits, "\n")
    cat("Standard Deviation : ", stdDev, "\n")
    cat("Probability        : ", probability, "\n")
    cat("Premium (net)      : ", netPremium, "\n")
    cat("Premium (gross)    : ", grossPremium, "\n")
    cat("Premium (gross+tax): ", grossPremiumTaxed, "\n")
  }  

  return (grossPremiumTaxed  )
}
#* @param p.insuredSum:numeric The insured sum
#* @param p.minRain:numeric Rain Threshold
#* @param p.eventThreshold:numeric Number of days threshold
#* @get /run
run <- function(p.insuredSum, p.minRain, p.eventThreshold) {
  cat("API Call incoming, params: insuredSum=", p.insuredSum, "; minRain=", p.minRain, "; eventThreshold=", p.eventThreshold, "\n")
  result <- calcPremium(
    hoursOfDay = hours, 
    daysOfWeek = daysOfWeek, 
    monthsOfYear = monthsOfYear, 
    insuredSum = as.numeric(p.insuredSum), 
    minRain = as.numeric(p.minRain), 
    eventThreshold = as.numeric(p.eventThreshold)
    )
  return (round(result*10,0)) # round it to zero decimals
}
