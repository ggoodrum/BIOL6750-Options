# Author: Greg Goodrum
# Course: BIOL 6750 - Programming for Biologists
# Section: Session 12 - A Brief Tour of Data and Modeling in R
# ------------------------------------------------------------

# 1. (a) Load the flights_2008.csv dataset using data.table

library(data.table)
airports <- fread("~/USU/Coursework/Programming for Biologist (BIOL 6750)/Code_Share/BIOL6750-Options/airports.csv")
flightdata <- fread('~/Downloads/2008.csv', stringsAsFactors = FALSE)

# This is method to load ata from the web source instead of a disk file
library(data.table)
library(suppdata)
flightdatatest <- fread(suppdata("http://stat-computing.org/dataexpo/2009/2008.csv.bz2"))


# 1. (b) Calculate the mean delay for each departure and arrival airport.
#        Rank the airports by departure/arrival delays.

# Use data.table subsetting to calculate mean delay for departing flights from origin airport
# and arrival delays at destination airports
Mean.Arr.Delay = flightdata[Origin!="", mean(ArrDelay, na.rm=TRUE), by = .(Dest)]
Mean.Dep.Delay = flightdata[Origin!="", mean(DepDelay, na.rm=TRUE), by = .(Origin)]

# Use the setorder function to rank each set by
Ranked.Mean.Arr.Delay = setorder(Mean.Arr.Delay, -V1)
Ranked.Mean.Dep.Delay = setorder(Mean.Dep.Delay, -V1)

# Can rename columns using colnames
colnames(Ranked.Mean.Arr.Delay)[1] <- 'Airport'
colnames(Ranked.Mean.Arr.Delay)[2] <- 'AvgArrDly(min)'
colnames(Ranked.Mean.Dep.Delay)[1] <- 'Airport'
colnames(Ranked.Mean.Dep.Delay)[2] <- 'AvgDepDly(min)'

# Combine averages into a single table based on shared field
# cor.test requires lists of the same length, so this maintains
# only the records that have both arrival and depature info

AllDelays <- merge(Ranked.Mean.Arr.Delay,Ranked.Mean.Dep.Delay, by = 'Airport')

# Rank AllDelays by either category

AllDelay.Rank.Dep <- setorder(AllDelays, -'AvgDepDly(min)')
AllDelay.Rank.Arr <- setorder(AllDelays, -'AvgArrDly(min)')


# 1. (c) Use cor.test to see if there is a correlation between the arrival and
#        departure delays for each airport.  Work from the summarized data
#        calculated above, not the raw data.

cor.test(AllDelays$`AvgDepDly(min)`, AllDelays$`AvgArrDly(min)`)

# The test returns a correlation coefficient of 0.781582, which indicates a strong
# positive linear relationship between arrival delay and departure delay.


# 2. Use expand.grid, prog.bar, and seq to improve code.  Fix the lack of replication
#    and address conflation of effect size with number of samples in their power analysis.

# Load required libraries
library(geiger)

# Setup simulation parameters
sims <- data.frame(expand.grid(effect.sizes = c(-5,-4,-3,-2,-1,0,1,2,3,4,5), n.samples = c(5,10,15,20,25,30,35,40)))

# Create value holder for p.value results
p.values <- vector()

# Define progress bar for loop
prog.bar <- function(x, y){
  if(y < 100){
    cat(".")}
  else {
    z <- Filter(function(z) z>=0, seq(1,y,length.out=100)-x)
    if(length(z) > 0)
      tryCatch(if(z[1] < 1) if((length(z) %% 10)==0) cat("|") else cat("."),
               error=function(z) cat("."))
  }
}

# For every combination of effect size and n.samples
for (i in 1:nrow(sims)){
  # Run the progress bar
  prog.bar(i, nrow(sims))
  # Draw a set of random numbers using n.samples
  one.sample <- rnorm(sims$n.samples[i], 0)
  # Draw another set of with a difference from the current effect size
  other.sample <- rnorm(sims$n.samples[i], sims$effect.sizes[i])
  # Perform a t-test
  test <- t.test(one.sample, other.sample)
  # Append the resulting
  p.values <- append(p.values, test$p.value)
}

# Plot the p-values against the effect sizes
plot(p.values ~ sims$effect.sizes)


# ---- Below this line is reference code from the handout ----


# These are my effect sizes
effect.sizes <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
# These are the number of samples I'll draw each time
n.samples <- c(5,10,15,20,25,30,35,40)
# I want to store the p-values from a t-test in this
p.values <- 0
# Loop over the effect sizes
for(eff.siz in effect.sizes){
  # Loop over the number of samples
  for(n in n.samples){
    # Draw one set of random numbers (using n.samples)
    one.sample <- rnorm(n, 0)
    # Draw another set (with a difference from effecti.sizes)
    other.sample <- rnorm(n, eff.siz)
    # Do a t-test
    test <- t.test(one.sample, other.sample)
    # Add (append) the p-value from the test to my set of values
    p.values <- append(p.values, test$p.value)
  } }
# Plot the p-values against the effect.size to see how big an effect
#   I can measure in my experiment
plot(p.values ~ effect.sizes)
# IT DOESN'T WORK! R is stupid and wrong

prog.bar <- function(x, y){
  if(y < 100){
    cat(".")}
  else {
      z <- Filter(function(z) z>=0, seq(1,y,length.out=100)-x)
      if(length(z) > 0)
        tryCatch(if(z[1] < 1) if((length(z) %% 10)==0) cat("|") else cat("."),
                 error=function(z) cat("."))
  }
}

for(i in 1:10000){
  prog.bar(i, 10000)
}


