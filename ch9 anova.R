#ch9 方差分析
library(pacman) #批量安装包
p_load(car,gplots,HH,rrcov,multcomp,effects,MASS,mvoutlier)  
#1.单因素方差分析
library(multcomp) 
attach(cholesterol) 
head(cholesterol)
table(trt) 
aggregate(response,by=list(trt),FUN=mean) #各族均值
aggregate(response,by=list(trt),FUN=sd) 
fit <- aov(response ~ trt) 
summary(fit) #检验组间差异
plotmeans(response ~ trt,xlab="Treatment",ylab="Response",
          main="Mean Plot\nwith 95% CI") 
detach(cholesterol) 

#2.多重比较
#Tukey HSD 
'''ANOVA对各疗法的F检验表明五种药物疗法效果不同，
但是并没有告诉你哪种疗法与其他疗法不同。
多重比较可以解决这个问题'''
TukeyHSD(fit) #对各组均值差异成对检验
par(las=2) 
par(mar=c(5,8,4,2)) 
plot(TukeyHSD(fit)) #图中置信区间包括0的说明不显著
#multcomp:适用于线性模型和广义线性模型
par(mar=c(5,4,6,2)) 
tuk <- glht(fit,linfct=mcp(trt="Tukey")) 
plot(cld(tuk,level=.05),col="lightgrey") 

#3.评估检验的假设条件
#正态性假设
qqPlot(lm(response ~ trt,data=cholesterol),
       simulate=TRUE,main="Q-Q Plot",labels=FALSE) 
#方差齐性检验
bartlett.test(response ~ trt,data=cholesterol)
outlierTest(fit) #方差齐性分析对离群点非常敏感 
##没有证据说明胆固醇数据中含有离群点（当p>1时将产生NA）


#4.单因素协方差分析
data(litter,package="multcomp") 
attach(litter) 
table(dose) 
aggregate(weight,by=list(dose),FUN=mean) 
fit <- aov(weight ~ gesttime+dose) 
summary(fit)  

library(effects) 
effect("dose",fit) #去除协变量效应后的组均值

#对用户定义的对照的多重比较（未用药vs三种用药）
contrast <- rbind("no drug vs. drug" = c(3,-1,-1,-1)) 
summary(glht(fit,linfct=mcp(dose=contrast))) 

#评估检验的假设条件
##检验回归斜率的同质性
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)
library(HH) 
ancova(weight ~ gesttime + dose,data=litter) 



#5.双因素方差分析
attach(ToothGrowth)
head(ToothGrowth)
table(supp,dose)
aggregate(len,by=list(supp,dose),FUN=mean)
aggregate(len,by=list(supp,dose),FUN=sd) 
dose <- factor(dose) 
fit <- aov(len ~ supp*dose) 
summary(fit) 
#可视化处理
#1. interaction.plot()
interaction.plot(dose,supp,len,type="b",
                 col=c("red","blue"),pch=c(16,18),
                 main="Interaction between Dose and Supplement Type")
#2. plotmeans() 
library(gplots) 
plotmeans(len ~ interaction(supp,dose,sep=""),
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red","darkgreen"),
          main="interaction Plot with 95%CIs",
          xlab = "Treatment and Dose Combination") 
#3. interaction2wt() 
library(HH) 
interaction2wt(len ~ supp*dose) #主效应和交互效应


#6.重复测量方差分析
CO2$conc <- factor(CO2$conc) 
w1b1 <- subset(CO2,Treatment=="chilled")
fit <- aov(uptake ~ conc*Type + Error(Plant/(conc)),w1b1)
summary(fit) 
par(las=2) 
par(mar = c(10,4,4,2)) 
with(w1b1, interaction.plot(conc,Type,uptake, 
                            type="b", col=c("red","blue"), pch=c(16,18), 
                            main="Interaction Plot for Plant Type and Concentration")) 
boxplot(uptake ~ Type*conc, data=w1b1, col=(c("gold", "green")), 
          main="Chilled Quebec and Mississippi Plants", 
          ylab="Carbon dioxide uptake rate (umol/m^2 sec)")


#7.多元方差分析
##单因素多元方差分析
library(MASS) 
attach(UScereal) 
shelf <- factor(shelf) #因子变量--分组变量
y <- cbind(calories,fat,sugars) #合并
aggregate(y,by=list(shelf),FUN=mean) 
fit <- manova(y ~ shelf) 
summary(fit) 
summary.aov(fit) 

#评估假设检验
n <- nrow(y) 
p <- ncol(y) 
cov <- cov(y) 
d <- mahalanobis(y,center,cov) 
coord <- qqplot(qchisq(ppoints(n),df=p), 
                d, main="Q-Q Plot Assessing Multivariate Normality", 
                ylab="Mahalanobis D2")
abline(a=0,b=1) 
identify(coord$x,coord$y,labels=row.names(UScereal)) 
#检验离群点
library(mvoutlier) 
outliers <- aq.plot(y) 
outliers 






















