# Author: Greg Goodrum
# Course: BIOL 6750 - Programming for Biologists
# Section: Session 12 - A Brief Tour of Data and Modeling in R
# ------------------------------------------------------------

# 1. (a) Load the flights_2008.csv dataset using data.table

library(data.table)
airports <- fread("~/USU/Coursework/Programming for Biologist (BIOL 6750)/Code_Share/BIOL6750-Options/airports.csv")
flightdata <- fread('~/Downloads/2008.csv')

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

# Combine averages into a single table, then ready for use in cor.test


# 1. (c) Use cor.test to see if there is a correlation between the arrival and
#        departure delays for each airport.  Work from the summarized data
#        calculated above, not the raw data.


# 2. Use expand.grid, prog.bar, and seq to improve code.  Fix the lack of replication
#    and address conflation of effect size with number of samples in their power analysis.
