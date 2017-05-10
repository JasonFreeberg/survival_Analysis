setwd("/Users/Jiang_Ziyi/survival_Analysis")

source("full.R")

set.seed(196)

trainSize = 0.75
trainIndx <- sample(1:nrow(full), size = trainSize*nrow(full), replace = F)
train <- full[trainIndx, ]
test <- full[-trainIndx, ]

# Double check that train+test == full
assertthat::are_equal((nrow(train) + nrow(test)), nrow(full) )

# Remake models on train
reducModel <- coxph(Surv(DURATION,event) ~ AGE_PRE + BMInew + SC, data = train)
fullModel  <- coxph(Surv(DURATION,event) ~ BMInew + AGE_PRE + sex + cigar+ alcohol + Socio.Economic, data = train)

# Predict on the testing sets with both models
test$pred1 <- exp(predict(reducModel, newdata = test, type = "lp"))
test$pred2 <- exp(predict(fullModel, newdata = test, type = "lp"))

base1 <- basehaz(reducModel)
base2 <- basehaz(fullModel)

r <- 1 - sum(train$event)/nrow(train)
r <- .5
r1 <- mean(exp(-predict(reducModel, type = "expected")))
r2 <- mean(exp(-predict(fullModel, type = "expected")))


for (i in 1:nrow(test)){
  test$time.pred1[i] <- base1[base1$hazard >= 0.9/test$pred1[i], 2][1]
  test$time.pred2[i] <- base2[base2$hazard >= 0.9/test$pred2[i], 2][1]
}

mae1 <- mean(abs(test$time.pred1-test$DURATION))
mae2 <- mean(abs(test$time.pred2-test$DURATION))

mean(full$DURATION)