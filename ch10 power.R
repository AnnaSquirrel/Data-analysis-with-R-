#ch10 power  analysis 
#设定四个量（样本大小、显著性水平、功效和效应值）中的三个
library(pwr)
#pwr.t.test(n=,d=,sig.level=,power=,type=,alternative=) 
library(pwr) 

#1.t检验
pwr.t.test(d=0.8,sig.level = 0.05,power=0.9,type="two.sample",
           alternative="two.sided") 

pwr.t.test(n=20,d=0.5,sig.level = 0.01,type = "two.sample",
           alternative = "two.sided")
##两组样本大小不同
pwr.t2n.test(n1=,n2=,d=,sig.level = ,power=,alternative = ) 


#2.方差分析 pwr.anova.test() 
pwr.anova.test(k=,n=,f=,sig.level = ,power=) 
pwr.anova.test(k=5, f=.25, sig.level=.05, power=.8)
#总样本大小=k*n 

#3.相关性
pwr.r.test(n=, r=, sig.level=, power=, alternative=)
pwr.r.test(r=.25, sig.level=.05, power=.90, alternative="greater")

#4.线性模型
pwr.f2.test(u=, v=, f2
            =, sig.level=, power=) 
#u和v分别是分子自由度和分母自由度，f2是效应值 
pwr.f2.test(u=3, f2=0.0769, sig.level=0.05, power=0.90)

#5.比例检验
pwr.2p.test(h=, n=, sig.level=, power=) 
pwr.2p2n.test(h=, n1=, n2=, sig.level=, power=) 
pwr.2p.test(h=ES.h(0.65,0.6),sig.level = 0.05,power=0.9,
            alternative = "greater")  

#6.卡方检验：评价两个类别变量的关系
pwr.chisq.test(w=, N=, df=, sig.level=, power=) 
prob <- matrix(c(.42, .28, .03, .07, .10, .10), byrow=TRUE, nrow=3)
ES.w2(prob) 






