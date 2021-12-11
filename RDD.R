install.packages("rddensity")
install.packages("magrittr")
library(rdrobust)
library(rddensity)
library(magrittr)
data("rdrobust_RDsenate")
attach(rdrobust_RDsenate)
##step1--------------------------------------------------------------------------------------------
#method1
hist(x=rdrobust_RDsenate$margin,
     breaks = 30,
     col = "red",
     xlab = "margin",ylab = "Frequence",
     main = "Histogram of Rating variable")
abline(v = 0) 
#method2 
library(ggplot2)
ggplot(rdrobust_RDsenate) +
  geom_histogram(aes(x = margin, fill = margin < 0), color = 'black', binwidth = 4, boundary = 4) +
  geom_vline(xintercept = 0, size = 1) +
  labs(title = 'Histogram of the Rating varibale', y = 'Number of Observation', x = 'Margin') + 
  scale_fill_manual(name = '', values = c('red', 'blue'),
                    labels = c('Control', 'Treat')) +
  theme_bw() +
  theme(legend.position = c(.9, .9),
        legend.background = element_blank())
#method3 
rdplotdensity(rddensity(rdrobust_RDsenate$margin), X = rdrobust_RDsenate$margin)

##step2---------------------------------------------------------------------------------------------------------
rdrobust_RDsenate$treatment <- ifelse(rdrobust_RDsenate$margin < 0, 0, 1)
#转换为虚拟变�?
rdrobust_RDsenate$treatment <- ifelse(rdrobust_RDsenate$margin < 0, 0, 1)
rdrobust_RDsenate$color <- ifelse(rdrobust_RDsenate$margin < 0, 'blue', 'red')

plot(x = rdrobust_RDsenate$margin, y = rdrobust_RDsenate$treatment, col = rdrobust_RDsenate$color,
     type = 'p', pch = 16, xlab = 'Margin', ylab = 'Treatment',
     main = 'Relationship between rating variable and treatment')
abline(v = 0)

##step3------------------------------------------------------------------------------------------------------
plot(x=rdrobust_RDsenate$margin,y=rdrobust_RDsenate$vote,type='p',
     col = rdrobust_RDsenate$color, pch=16,cex=0.8,
     xlab = 'Margin',
     ylab = 'Vote')
abline(v=0) 

##step4---------------------------------------------------------------------------------------------------------
plot(x = rdrobust_RDsenate$margin, y = rdrobust_RDsenate$vote, type = 'p',
     col = rdrobust_RDsenate$color, pch = 16, cex = 0.8, xlab = 'Margin', ylab = 'Vote')
abline(v = 0)
#choose bins 
bins <- rdplot(rdrobust_RDsenate$vote,rdrobust_RDsenate$margin,
               c=0,p=4,
               nbins=c(20,20),binselect='esmv',kernel='uniform') 
summary(bins) 

##step5-------------------------------------------------------------------------------------------------------------
#1.
rdrobust_RDsenate$margin_del <- rdrobust_RDsenate$margin - 0

fit_1 <- lm(vote ~ margin_del + treatment, data = rdrobust_RDsenate) # linear
fit_2 <- lm(vote ~ margin_del * treatment, data = rdrobust_RDsenate) # linear interaction
fit_3 <- lm(vote ~ margin_del + I(margin_del ^ 2) + treatment, data = rdrobust_RDsenate) # quadratic
fit_4 <- lm(vote ~ (margin_del + I(margin_del ^ 2)) * treatment, data = rdrobust_RDsenate) # quadratic interaction
fit_5 <- lm(vote ~ margin_del + I(margin_del ^ 2) + I(margin_del ^ 3) + treatment, data = rdrobust_RDsenate) # cubic
fit_6 <- lm(vote ~ (margin_del + I(margin_del ^ 2) + I(margin_del ^ 3)) * treatment, data = rdrobust_RDsenate) # cubic interaction

stargazer::stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, type = 'html', style = 'all')
#check stargazer output 
stargazer::stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, type = 'text', style = 'all')
#AIC 
AIC(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6) 

#2.
#CCT
rdbwselect(y=rdrobust_RDsenate$vote,x=rdrobust_RDsenate$margin,all=TRUE) %>% summary()
loc_fit_1 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = 0, p = 1,
                      kernel = 'triangular', bwselect = 'msetwo') # 
loc_fit_2 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = 0, p = 2,
                      kernel = 'triangular', bwselect = 'msetwo') 
loc_fit_3 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = 0, p = 1,
                      kernel = 'triangular', bwselect = 'cerrd')
loc_fit_4 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = 0, p = 2,
                      kernel = 'triangular', bwselect = 'certwo')

summary(loc_fit_1)

##step6---------------------------------------------------------------------------------------------------
sen_cut_1 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = 1, p = 1,
                      kernel = 'triangular', bwselect = 'msetwo')
sen_cut_2 <- rdrobust(rdrobust_RDsenate$vote, rdrobust_RDsenate$margin, c = -1, p = 1,
                      kernel = 'triangular', bwselect = 'msetwo') 
summary(sen_cut_1) 
summary(sen_cut_2) 
summary(sen_hole_2) 
summary(sen_hole_3) 


