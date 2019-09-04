## This is a R script for “Statistical Analysis” empirical exercise
## 10/06/18
## Min Jiang
v1hh <- read.csv("C:/studydata/Statistical Analysis/R/v1hh.csv")##insert data
View(v1hh)
##1.1	Find	the	sample	size
n <- length(v1hh$mpid_hh)
##1.2	Find	the	mean and	standard	deviation for	the	variables
summary(v1hh)
sd(v1hh$templecompany_hh)
sd(v1hh$lendmoney_hh)
sd(v1hh$keroricego_hh)
##1.3	Find	the	correlation	coefficient	between	keroricego_hh and samecaste_hh
cor(v1hh$keroricego_hh, v1hh$samecaste_hh)
##1.4 How	many	pairs (in	total) have	the	relation	"lendmoney_hh" in	this	village?
sum(v1hh$lendmoney_hh) 
##1.5	Is	the	average	number	relation	"lendmoney_hh"	greater	than	0. 035 at	5%	significance	level?	
t.test(v1hh$lendmoney_hh, mu = 0.035, alternative = c("greater"))
##1.6.Construct	a	confidence level for	the	population	mean	for	lendmoney_hh, at confidence level	
##95%
meanlendmoney <- mean(v1hh$lendmoney_hh)
n <- length(v1hh$lendmoney_hh)
s <- sd(v1hh$lendmoney_hh)
se <- s/sqrt(n)
c <- qt(0.975,n-2)
CI <- c(meanlendmoney-c*se, meanlendmoney+c*se)

##2.Simple linear	regressions
lm1 <- lm(keroricego_hh ~ samecaste_hh, data = v1hh)
summary(lm1)
plot(keroricego_hh ~ samecaste_hh, data = v1hh, pch = 16, cex = 1.3,col = "blue", main = "keroricego vs samecaste", xlab = "samecaste_hh", ylab = "keroricego_hh")
abline(lm1,lty=2)
##2.2
lm2 <- lm(templecompany_hh ~ samecaste_hh, data = v1hh)
summary(lm2)
plot(templecompany_hh  ~ samecaste_hh, data = v1hh,pch = 16, cex = 1.3,col = "blue", main = "templecompany vs samecaste", xlab = "samecaste_hh", ylab = "templecompany_hh")
abline(lm2,lty=2)

cor(v1hh$templecompany_hh, v1hh$samecaste_hh)

##3.Multiple linear	regressions	   
lm3 <- lm(keroricego_hh ~ templecompany_hh + lendmoney_hh, data = v1hh)
summary(lm3)
##3.3
keroricego_hh_fit <- predict(lm3)
which(v1hh$templecompany_hh == 1 & v1hh$lendmoney_hh == 1)

keroricego_hh_fit[245]
keroricego_hh_fit[2003]
keroricego_hh_fit[4185]
keroricego_hh_fit[5512]














