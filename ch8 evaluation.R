#8.3 回归诊断p171
##plot(fit)
library(wooldridge) 
data(women)
attach(women) 
fit <- lm(weight ~ height) 
par(mfrow=c(2,2)) 
plot(fit) 
newfit <- lm(weight ~ height + I(height^2),data=women[-c(13,15),]) 

##car包
##1. 正态性
library(car) 
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")]) 
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states) 
qqPlot(fit, labels=row.names(states), id.method="identify", 
       simulate=TRUE, main="Q-Q Plot") 
#观察离群点
states["Nevada",] 
fitted(fit)["Nevada"] #模型预测值
residuals(fit)["Nevada"] #实际值
rstudent(fit)["Nevada"] 
#学生化残差图的函数
residplot <- function(fit, nbreaks=10) { 
  z <- rstudent(fit) 
  hist(z, breaks=nbreaks, freq=FALSE, 
       xlab="Studentized Residual", 
       main="Distribution of Errors") 
  rug(jitter(z), col="brown") 
  curve(dnorm(x, mean=mean(z), sd=sd(z)), 
        add=TRUE, col="blue", lwd=2) 
  lines(density(z)$x, density(z)$y, 
        col="red", lwd=2, lty=2) 
  legend("topright", 
         legend = c( "Normal Curve", "Kernel Density Curve"), 
         lty=1:2, col=c("blue","red"), cex=.7) 
} 
residplot(fit) 

##2. 误差独立性：Durbin-Watson test
#时间序列数据通常呈现自相关 
durbinWatsonTest(fit) 
#p值不显著：无相关性，误差项之间独立

##3. 线性: partial residual plot 
#自变量和因变量之间是否存在非线性关系 
library(car)
crPlots(fit) 
#如果呈现非线性关系，需要在模型中增加非线性项

##4. 同方差性
library(car) 
ncvTest(fit) 
spreadLevelPlot(fit) 
#p不显著：满足方差不变假设

##线性模型假设的综合检验 
install.packages("gvlma") 
library(gvlma) 
gvmodel <- gvlma(fit) 
summary(gvmodel) 

##多重共线性
library(car)
vif(fit) 
sqrt(vif(fit)) > 2 #检测标准：大于2就存在多重共线性



#8.4 异常值处理
#1. 离群点：模型预测效果不佳的观测点
library(car) 
outlierTest(fit) 
#Nevada被判定为离群点 

#2. 高杠杆值点：与其他预测变量有关的离群点
hat.plot <- function(fit) { 
  p <- length(coefficients(fit)) 
  n <- length(fitted(fit)) 
  plot(hatvalues(fit), main="Index Plot of Hat Values") 
  abline(h=c(2,3)*p/n, col="red", lty=2) 
  identify(1:n, hatvalues(fit), names(hatvalues(fit))) 
} 
hat.plot(fit)
'''对于一个给定的数据集，帽子均
值为p/n，其中p是模型估计的参数数目（包含截距项），
n是样本量。一般来说，若观测点的帽子值大于帽子均值的2或3倍，
就可以认定为高杠杆值点'''

#3. 强影响点;对模型参数估计值影响有些比例失衡的点 
'''若移除模型的一个观测点时
模型会发生巨大的改变，需要检测是否存在强影响点'''
#cook's D > 4/(n-k-1) 
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2) 
plot(fit,which=4,cook.levels=cutoff) 
abline(h=cutoff,lty=2,col="red") 
#变量添加图 avPlots() 
library(car) 
avPlots(fit,ask=FALSE,id.method='identify') 
#整合离群点、强影响点和高杠杆值
influencePlot(fit,id.method="identify",main="Influence Plot",
              sub="Circle size is proportional too Cook's distance") 
'''纵坐标超过+2或小于–2的州可被认为是离群点，
水平轴超过0.2或0.3的州有高杠杆值（通常为预测值的组合）
圆圈大小与影响成比例，
圆圈很大的点可能是对模型参数的估计造成的不成比例影响的强影响点'''

#---------------------------------------------------------------------------------------------------
#8.5 补救措施
#1.变量变换
##box-cox
library(car) 
summary(powerTransform(states$Murder)) 
#结果建议使用murder^0.6来正态化变量murder
#box-tidwell 
boxTidwell(Murder ~ Population+Illiteracy,data = states) 
'''但是对
Population（p=0.75）和Illiteracy（p=0.54）的计分检验又表明变量并不需要变换'''


#---------------------------------------------------------------------------------------------------
#8.6 模型选择
##1. anova() 
attach(states) 
fit1 <- lm(Murder ~ Population+Illiteracy) 
fit2 <- lm(Murder ~ Population+Illiteracy+Income+Frost) 
anova(fit2,fit1) #fit1嵌套在fit2中
'''对是否应该添加Income和Frost到线性模型
中进行了检验,结果不显著，可以删除'''

##2. AIC
'''优先选择较小的AIC：用较小的参数获得了足够的拟合度'''
AIC(fit1,fit2) #不需要嵌套

##3.变量选择
##逐步回归法 stepwise method 
library(MASS) 
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data =states) 
stepAIC(fit,direction="backward") 
'''每一步中，AIC列提供了删除一个行中变量后
模型的AIC值'''

##全子集回归
library(leaps) 
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")]) 
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost,
                    data=states,nbest=4) 
plot(leaps,statistic="cp",
     main="Cp Plot for all subsets regression") 
abline(1,1,lty=2,cpl="red")  

#---------------------------------------------------------------------------------------------------
#8.7 深层次分析
#交叉验证:自制shrinkage函数
install.packages("bootstrap")
library(bootstrap)
shrinkage <- function(fit,k=10){ 
  #创建了一个包含预测变量和预测值的矩阵，可获
  #得初始R平方以及交叉验证的R平方
  require(bootstrap) 
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  
  x <- fit$model[,2:ncol(fit$model)] 
  y <- fit$model[,1] 
  
  results <- crossval(x,y,theta.fit,theta.predict,ngroup=k) 
  r2 <- cor(y,fit$fitted.values)^2 
  r2cv <- cor(y,results$cv.fit)^2 
  cat("original r_square =",r2,"\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
  }
#对states数据所有预测变量进行回归，然后再用shrinkage()函数做10重交叉验证
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                       "Illiteracy", "Income", "Frost")]) 
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states) 
shrinkage(fit) 
#交叉验证R^2=0.446 
#挑选变量
fit2 <- lm(Murder ~ Population + Illiteracy,data=states) 
shrinkage(fit2)

#相对重要性：relweights() 
'''计算预测变量的相对权重''' 
relweights <- function(fit,...){ 
  R <- cor(fit$model) 
  nvar <- ncol(R) 
  rxx <- R[2:nvar, 2:nvar] 
  rxy <- R[2:nvar, 1] 
  svd <- eigen(rxx) 
  evec <- svd$vectors 
  ev <- svd$values 
  delta <- diag(sqrt(ev)) 
  lambda <- evec %*% delta %*% t(evec) 
  lambdasq <- lambda ^ 2 
  beta <- solve(lambda) %*% rxy 
  rsquare <- colSums(beta ^ 2) 
  rawwgt <- lambdasq %*% beta ^ 2 
  import <- (rawwgt / rsquare) * 100 
  import <- as.data.frame(import) 
  row.names(import) <- names(fit$model[2:nvar]) 
  names(import) <- "Weights" 
  import <- import[order(import),1, drop=FALSE] 
  dotchart(import$Weights, labels=row.names(import), 
           xlab="% of R-Square", pch=19, 
           main="Relative Importance of Predictor Variables", 
           sub=paste("Total R-Square=", round(rsquare, digits=3)), 
           ...) 
  return(import) 
}
relweights(fit,col="blue") 
#Illiteracy有最大的相对重要程度
#---------------------------------------------------------------------------------------------------
















