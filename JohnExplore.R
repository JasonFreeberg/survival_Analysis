# 4-10-17
# JohnExplore.R week 1
# John Randazzo

#dealing with BMI - remove all below 15 or above 100:

a <- boxplot(full$BMInew)
potential_outliers <- a$out
outlier_na <- ifelse(potential_outliers > 100 | potential_outliers < 15, NA, potential_outliers)

#now to apply to dataset:

full$new_bmi <- ifelse(BMInew > 100 | BMInew < 15, NA, full$BMInew)
full$new_bmi

# here is the new dataset: omits all NA values

new <- na.omit(full)
mean(new$new_bmi)
mean(full$new_bmi, na.rm = TRUE)

# let us explore the relationships between bmi and other covariates

# frequency analysis

library(lattice)

histogram(full$Socio.Economic)
histogram(full$sex)
histogram(full$alcohol)
histogram(full$cigar)

men <- new[new$sex == 2,]
histogram(men$Socio.Economic)
histogram(men$alcohol)
histogram(men$cigar)

women <- new[new$sex == 1,]
histogram(women$Socio.Economic)
histogram(women$alcohol)
histogram(women$cigar)

smoker <- new[new$cigar == 1,]
histogram(smoker$Socio.Economic)
histogram(smoker$sex)
histogram(smoker$alcohol)

nonsmoker <- new[new$cigar == 0,]
histogram(nonsmoker$Socio.Economic)
histogram(nonsmoker$sex)
histogram(nonsmoker$alcohol)

drinker <- new[new$alcohol == 1,]
histogram(drinker$Socio.Economic, col = "blue")
histogram(drinker$sex)
histogram(drinker$cigar)

nondrinker <- new[new$alcohol == 0,]
histogram(nondrinker$Socio.Economic, col = "red")
histogram(nondrinker$sex)
histogram(nondrinker$cigar)

# i will not be including most of these in my write up, only ones i find to be particularly interesting

#categorize bmi

quantile(new$new_bmi)

new$bmi.cat[new$new_bmi < 23.9] <- 0
new$bmi.cat[new$new_bmi > 23.9 & new$new_bmi < 30.2] <- 1
new$bmi.cat[new$new_bmi > 30.2] <- 2

thin <- new[new$bmi.cat == 0,]
norm <- new[new$bmi.cat == 1,]
fat <- new[new$bmi.cat == 2,]

# i will use ggplot2 to superimpose graphs to compare measures across different subpopulations (e.g. smoker, women, drinker, etc)