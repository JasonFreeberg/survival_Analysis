#transitioned <- read.csv(file="transitioned.csv", header=T)
#censored <- read.csv(file="censored.csv", header=T)
#full <- rbind(transitioned, censored)

table.sex <- rbind(table(transitioned$sex),table(censored$sex),table(full$sex)) #Summarizing sex counts
rownames(table.sex) <- c("Trasnsitioned","Censored","All")
colnames(table.sex) <- c("Male","Female")
table.sex
prop.table(table.sex,1) #Show row percentages of sex

table.socio <- rbind(table(transitioned$Socio.Economic),
                     table(censored$Socio.Economic),
                     table(full$Socio.Economic)) #Summarizing socio.economic status counts
table.socio <- as.data.frame.matrix(table.socio) #Convert the table to a data frame
table.socio$mean <- c(mean(transitioned$Socio.Economic),
                      mean(censored$Socio.Economic),
                      mean(full$Socio.Economic)) #Add a mean colume to the data frame
rownames(table.socio) <- c("Trasnsitioned","Censored","All")
table.socio

table.BMI <- rbind(summary(transitioned$BMInew),
                   summary(censored$BMInew),
                   summary(full$BMInew)) #Summarizing the BMI
rownames(table.BMI) <- c("Trasnsitioned","Censored","All")
table.BMI

table.cigalc <- matrix(c(mean(transitioned$cigar),mean(transitioned$alcohol),
                         mean(censored$cigar),mean(censored$alcohol),
                         mean(full$cigar),mean(full$alcohol)),ncol=2,byrow=TRUE) #Summarizing smoker and alcohol user percentage
rownames(table.cigalc) <- c("Trasnsitioned","Censored","All")
colnames(table.cigalc) <- c("Smoker","Used Alcohol")
table.cigalc

table.duration <- rbind(summary(transitioned$DURATION),
                   summary(censored$DURATION),
                   summary(full$DURATION)) #Summarizing the duration
rownames(table.duration) <- c("Trasnsitioned","Censored","All")
table.duration