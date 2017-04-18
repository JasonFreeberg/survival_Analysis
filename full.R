library(ggplot2)
library(survival)
library(plyr)
library(dplyr)

transitioned <- read.csv(" .csv")
censored <- read.csv(" .csv")
full <- rbind(transitioned,censored)
full$event <- as.numeric(full$Partial_code_ff)
full$BMInew <- ifelse(full$BMInew > 100 | full$BMInew < 12, NA, full$BMInew)
full <- na.omit(full)

longFormat = "%d/%m/%Y"
shortFormat = "%d/%m/%y"
full$Socio.Econdate <- as.character(full$Socio.Econdate)
full$Socio.Econdate <- ifelse(full$Socio.Econdate != "", full$Socio.Econdate, NA)
longYear <- grep(pattern = ".+\\d{4}$", x=full$Socio.Econdate)
shortYear <- !(seq(1, nrow(full)) %in% longYear)
long <- full[longYear, ]
short <- full[shortYear, ]
long$Socio.Econdate <- as.Date(long$Socio.Econdate, format=longFormat)
short$Socio.Econdate <- as.Date(short$Socio.Econdate, format=shortFormat)

full <- rbind(long, short)
ordered <- full %>%
  arrange(Socio.Econdate)

duplicates <- duplicated(ordered$UNQID_tmp)
full <- ordered[!duplicates, ]
# 
