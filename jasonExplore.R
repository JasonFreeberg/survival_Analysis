
library(ggplot2)

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

surv1 <- Surv(full$duration, event=full$event)
basicKM <- survfit(surv1 ~ 1)
kmPlot1 <- ggsurv(surv1)
