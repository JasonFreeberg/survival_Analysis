
### Master file for Exploratory Analysis

library(ggplot2)
library(plyr)
library(dplyr)

transitioned <- read.csv(file="transitioned.csv", header=T)
censored <- read.csv(file="censored.csv", header=T)
full <- rbind(transitioned, censored)

source("tables.R")

# Example:
# source("someRFile.R")
# This script (explore.R) will then run everything in someRFile.R

