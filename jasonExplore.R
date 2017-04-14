

#  When this script is ran through Explore.R, the following is redundant
#transitioned <- read.csv(file="transitioned.csv", header=T)
#censored <- read.csv(file="censored.csv", header=T)
#full <- rbind(transitioned, censored)

density <- ggplot(data=full, mapping=aes(x=DURATION, fill=event)) +
              geom_density(alpha=0.5) +
              ggtitle("Density Plot of Surival Times") +
              xlim(0, 10000) +
              xlab("Time (days)") +
              scale_y_continuous(labels = scales::percent)

# Need to remove BMI outliers
full2 <- full
full2$BMInew <- ifelse(full2$BMInew > 100 | full2$BMInew < 15, NA, full2$BMInew)
full2 <- na.omit(full2)
bmiQuants <- quantile(full2$BMInew, seq(0, 1, 0.25))
full2$bmiCat <- cut(full2$BMInew, unique(bmiQuants), include.lowest=TRUE)
full2$bmiCat <- factor(full2$bmiCat, unique(levels(full2$bmiCat))[c(1,2,3,4)], labels=c("Low", "Medium", "High", "Obese"))

# Exploratory KM Estimates
survObject <- Surv(full$DURATION, event=full$Partial_code_ff)
survObject2 <- Surv(full2$DURATION, event=full2$Partial_code_ff)
bmiKM <- survfit(survObject2 ~ full2$bmiCat)
alcKM <- survfit(survObject ~ full$alcohol)
cigKM <- survfit(survObject ~ full$cigar)

ggsurv(alcKM, plot.cens=F) +
  ggtitle("KM Estimates Statified by Alcohol Consumption") +
  scale_fill_discrete(name="New Legend Title")

ggsurv(bmiKM, plot.cens=F) +
  ggtitle("Stratified by BMI Range")

ggsurv(cigKM, plot.cens = F) +
  ggtitle("Stratified by Cigarette Usage")
