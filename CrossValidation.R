setwd("/Users/Jiang_Ziyi/survival_Analysis")

source("full.R")

set.seed(196)

trainSize = 0.75
trainIndx <- sample(1:nrow(full), size = trainSize*nrow(full), replace = F)
train <- full[trainIndx, ]
test <- full[-trainIndx, ]

# Double check that train+test == full
assertthat::are_equal((nrow(train) + nrow(test)), nrow(full) )

# Make two survival objects
# trainSurv <- Surv(train$DURATION, train$event)
# testSurv <- Surv(test$DURATION, test$event)

# Remake models on train
reducModel <- coxph(Surv(DURATION,event) ~ AGE_PRE + BMInew + SC, data = train)
fullModel  <- coxph(Surv(DURATION,event) ~ BMInew + AGE_PRE + sex + cigar+ alcohol + Socio.Economic, data = train)

cox.zph(reducModel)
cox.zph(fullModel)

summary(test$AGE_PRE)

pred1 <- predict(reducModel, newdata = test, type = "lp")
pred2 <- predict(fullModel, newdata = test, type = "expected")

prob1 <- exp(-pred1)
