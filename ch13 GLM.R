#ch13 
##13.1 ogistic 

data(Affairs,package="AER") 
summary(Affairs) 
table(Affairs$affairs)
#transfer to binary var. 
Affairs$ynaffair[Affairs$affairs > 0] <- 1 
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes")) 
table(Affairs$ynaffair) 
#logistic regression of ynaffair
attach(Affairs) 
fit.full <- glm(ynaffair ~ gender+age+yearsmarried+children+religiousness+education+occupation+rating,
                family=binomial()) 
summary(fit.full)
#remove insignificant vars 
fit.reduced <- glm(ynaffair ~ age+yearsmarried+religiousness+rating,family = binomial())
summary(fit.reduced) 
#compare in anova 
anova(fit.reduced,fit.full,test="Chisq") 
#exlain coefficients  
coef(fit.reduced) 
exp(coef(fit.reduced)) 
#possibility 
testdata <- data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age),
                        yearsmarried=mean(Affairs$yearsmarried),
                        religiousness=mean(Affairs$religiousness)) 
testdata
testdata$prob <- predict(fit.reduced,newdata = testdata,
                         type="response") 
testdata
#quasibinomial distribution 
deviance(fit.reduced)/df.residual(fit.reduced) 
pchisq(summary(fit.od)$dispersion * fit$df.residual,
       fit$df.residual,lower=F)  
fit <- glm(ynaffair ~ age+yearsmarried+religiousness+
             rating,family = binomial())
fit.od <- glm(ynaffair ~  age+yearsmarried+religiousness+
                rating,family = quasibinomial()) 
pchisq(summary(fit.od)$dispersion*fit$df.residual,
       fit$df.residual,lower=F) #p is insignificant


##13.2 poisson 
data(breslow.dat,package = "robust") 
names(breslow.dat)
summary(breslow.dat[c(6,7,8,10)]) 
#boxplot 
opar <- par(no.readonly=TRUE) 
par(mfrow=c(1,2)) 
attach(breslow.dat) 
hist(sumY, breaks=20, xlab="Seizure Count", 
     main="Distribution of Seizures") 
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons") 
par(opar)
#poisson 
fit <- glm(sumY ~ Base+Age+Trt,family = poisson())
summary(fit) 
#explain coefs 
coef(fit) 
exp(coef(fit))
deviance(fit)/df.residual(fit) 
install.packages("qcc") 
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY,type="poisson") #overdispersion exists! 





 