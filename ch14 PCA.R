#ch14 
library(psych)
attach(USJudgeRatings)
#calculate PC numbers 
fa.parallel(USJudgeRatings[,-1],fa="pc",
            n.iter=100,show.legend=FALSE,main="Scree Plot with parallel analysis") 
#principal(r,nfactors=,rotate=,scores=) 
library(psych)
pc <- principal(USJudgeRatings[,-1],nfactors=1) 
pc 
fa.parallel(Harman23.cor$cov,n.obs=302,fa="pc",n.iter=100,
            show.legend = FALSE,main="Scree plot with parallel analysis") 
#rotation for PC with high variance 
pc <- principal(Harman23.cor$cov,nfactors = 2,rotate="none") 
pc 
rc <- principal(Harman23.cor$cov,nfactors=2,rotate="varimax")
rc
#component loadings 
rc <- principal(Harman23.cor$cov,nfactors=2,rotate="varimax") 
round(unclass(rc$weights),2) 
PC1 = 0.28*height + 0.30*arm.span + 0.30*forearm + 0.29*lower.leg - 
  0.06*weight - 0.08*bitro.diameter - 0.10*chest.girth - 
  0.04*chest.width
PC2 = -0.05*height - 0.08*arm.span - 0.09*forearm - 0.06*lower.leg + 
  0.33*weight + 0.32*bitro.diameter + 0.34*chest.girth + 
  0.27*chest.width
options(digits=2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances) 
correlations  

#EFA 
covariances <- ability.cov$cov 
correlations <- cov2cor(covariances)
fa.parallel(correlations,n.obs=112,fa="both",n.iter=100,
            main="Scree plots with parallel analysis") #show PCA and EFA 
#extract common component(varimax)
fa.varimax <- fa(correlations,nfactors=2,rotate="varimax",fm="pa")
fa.varimax 
#extract common component(promax)
install.packages("GPArotation")
library(GPArotation)
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa") 
fa.promax
fa.diagram(fa.promax,simple = FALSE) #graph
fa.diagram(fa.promax,simple = TRUE) #graph
fa.promax$weights #score 






