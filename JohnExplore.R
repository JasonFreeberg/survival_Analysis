# 4-10-17
# JohnExplore.R week 1
# John Randazzo

#full <- rbind(transitioned,censored)
#full$event <- as.numeric(full$Partial_code_ff)
#library(ggplot2)
#library(survival)



# dealing with BMI - remove all below 15 or above 100:

a <- boxplot(full$BMInew)
potential_outliers <- a$out
outlier_na <- ifelse(potential_outliers > 100 | potential_outliers < 15, NA, potential_outliers)

# now to apply to dataset:

full$new_bmi <- ifelse(BMInew > 100 | BMInew < 15, NA, full$BMInew)
full$new_bmi

# here is the new dataset: omits all NA values

new <- na.omit(full)
mean(new$new_bmi)
mean(full$new_bmi, na.rm = TRUE)


# categorize bmi

quantile(new$new_bmi)

new$bmi.cat[new$new_bmi < 23.9] <- 0
new$bmi.cat[new$new_bmi > 23.9 & new$new_bmi < 30.2] <- 1
new$bmi.cat[new$new_bmi > 30.2] <- 2

thin <- new[new$bmi.cat == 0,]
norm <- new[new$bmi.cat == 1,]
fat <- new[new$bmi.cat == 2,]

# let us explore the relationships between covariates

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
histogram(drinker$Socio.Economic)
histogram(drinker$sex)
histogram(drinker$cigar)

nondrinker <- new[new$alcohol == 0,]
histogram(nondrinker$Socio.Economic, col = "red")
histogram(nondrinker$sex,drinker$sex )
histogram(nondrinker$cigar)

# here are some X^2 tests for independence in our categorical predictors:

attach(new)
tbl1 = table(bmi.cat,Socio.Economic)
tbl1
chisq.test(tbl1)

tbl2 = table(bmi.cat,sex)
tbl2
chisq.test(tbl2)

tbl3 = table(bmi.cat,cigar)
tbl3
chisq.test(tbl3)

tbl4 = table(bmi.cat,alcohol)
tbl4
chisq.test(tbl4)

tbl5 = table(sex, alcohol)
tbl5
chisq.test(tbl5)

tbl6 = table(sex,cigar)
tbl6
chisq.test(tbl6)

tbl6 = table(sex,Socio.Economic)
tbl6
chisq.test(tbl6)

tbl7 <- table(Socio.Economic, alcohol)
tbl7
chisq.test(tbl7)

tbl8 <- table(Socio.Economic, cigar)
tbl8
chisq.test(tbl8)

tbl9 <- table(alcohol,cigar)
tbl9
prop.test(tbl9)

tbl100 = table(cigar,sex)
tbl100
prop.test(tbl100)


detach(new)

# bruh... miniscule p-values galore... none of these pass independence...


# finally, here is an attempt at making pretty overlayed histograms.
# none of these work how i want them to.
# every answer i find on stackexchange leads me to an error message... lowkey may need to update Rstudio
# gg ggplot
library(lattice)

ggplot(full, aes(x=Socio.Economic)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(full,aes(x=Socio.Economic)) + 
  geom_bar(data=men,aes(y=..count../sum(..count..))) +
  geom_bar(data=women,aes(y=..count../sum(..count..))) + 
  scale_y_continuous(labels = percent_format())

#install.packages("reshape2")
library(reshape2)
datm <- melt(cbind(new, ind = rownames(new)), id.vars = "sex")

ggplot(datm,aes(x = sex, y = Socio.Economic)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())

ggplot(full, aes(x = sex, fill = Socio.Economic)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent_format())