library(survival)
library(ggplot2)
#library(survminer)
library(plyr)
library(dplyr)

transitioned <- read.csv("transitioned.csv")
censored <- read.csv("censored.csv")

full <- rbind(transitioned,censored)
full$event <- as.numeric(full$Partial_code_ff)
full$event <- ifelse(full$event == 2, 0, 1)
full$BMInew <- ifelse(full$BMInew > 100 | full$BMInew < 12, NA, full$BMInew)
full <- na.omit(full)

full <- full[full$AGE_PRE>0,]
full <- full[full$DURATION>0,]

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

bmiQuants <- quantile(full$BMInew, seq(0, 1, 0.25))
full$bmiCat <- cut(full$BMInew, unique(bmiQuants), include.lowest=TRUE)
full$bmiCat <- factor(full$bmiCat, unique(levels(full$bmiCat))[c(1,2,3,4)], labels=c("Low", "Medium", "High", "Obese"))

full$SC <- ifelse(full$sex == 1 & full$cigar == 1, "MS",
           ifelse(full$sex == 2 & full$cigar == 1, "FS",
           ifelse(full$sex == 1 & full$cigar == 0, "MN", "FN")))
full$SC <- as.factor(full$SC)

obj <- Surv(full$DURATION, full$event)

# 2 models
m4 <- coxph(obj ~ full$AGE_PRE + full$BMInew + full$SC) # reduced
m3 <- coxph(obj ~ full$BMInew +  full$AGE_PRE + full$sex + full$cigar + full$alcohol + full$Socio.Economic) # full model
BIC(m4) # compare
BIC(m3)
