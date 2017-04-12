

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

# Exploratory KM Estimates
survObject <- Surv(full$DURATION, event=full$Partial_code_ff)
bmiKM <- NULL
alcKM <- survfit(survObject ~ full$alcohol)

ggsurv(alcKM, plot.cens = F) +
  
