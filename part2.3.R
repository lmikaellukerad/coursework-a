#######
# Part 2 question 3
#######
library(QuantPsyc)

#read fisherman data
fdata <- read.csv("fishermen_mercury.csv", header = TRUE, sep = ",")

#question 3.2
hist(fdata$TotHg, breaks = 30, col="blue", main = "Histogram of total mg/g mercury", xlab="total Hg in mg/g")

#question 3.3
plot(fdata$fisherman, fdata$TotHg, xlab = "fisherman (1=yes, 0=no)", ylab = "total Hg in mg/g")
plot(fdata$age, fdata$TotHg, xlab = "Age in years", ylab = "total Hg in mg/g")
plot(fdata$restime, fdata$TotHg, xlab = "residence time in years", ylab = "total Hg in mg/g")
plot(fdata$height, fdata$TotHg, xlab = "height in cm", ylab = "total Hg in mg/g")
plot(fdata$weight, fdata$TotHg, xlab = "weight in kg", ylab = "total Hg in mg/g")
plot(fdata$fishmlwk, fdata$TotHg, xlab = "fish meals a week", ylab = "total Hg in mg/g")
plot(fdata$fishpart, fdata$TotHg, xlab = "fish part eaten", ylab = "total Hg in mg/g")


#question 3.4
fmodel <- lm(TotHg ~ fisherman + age + restime + height + weight + fishmlwk + fishpart, data=fdata)
fsummary <- summary(fmodel)
fci <- confint(fmodel, level=0.95)
fbeta <- lm.beta(fmodel)

#question 3.5/6
plot(fmodel)