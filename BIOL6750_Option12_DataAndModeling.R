# Author: Greg Goodrum
# Course: BIOL 6750 - Programming for Biologists
# Section: Session 12 - A Brief Tour of Data and Modeling in R
# ------------------------------------------------------------

# 1. (a) Load the flights_2008.csv dataset using data.table

library(data.table)
data <- fread("https://usu.instructure.com/courses/498153/files/folder/datasets/airports.csv")
data <- fread("~/USU/Coursework/Programming for Biologist (BIOL 6750)/Code_Share/BIOL6750-Options/airports.csv")

# 1. (b) Calculate the mean delay for each departure and arrival airport.
#        Rank the airports by departure/arrival delays.


# 1. (c) Use cor.test to see if there is a correlation between the arrival and
#        departure delays for each airport.  Work from the summarized data
#        calculated above, not the raw data.


# 2. Use expand.grid, prog.bar, and seq to improve code.  Fix the lack of replication
#    and address conflation of effect size with number of samples in their power analysis.
