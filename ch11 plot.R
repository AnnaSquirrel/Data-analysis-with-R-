#ch11 中级绘图

#1.散点�?
library(pacman)
p_load(gplot) 
attach(mtcars) 
head(mtcars) 
plot(wt,mpg,main="Basic Scatter Plot of MPG vs. Weight",
     xlab = "Car Weight(lbs/1000)",
     ylab = "Miles Per Gallen",
     pch=19) 
abline(lm(mpg~wt),col="red",lwd=2,lty=1)
lines(lowess(wt,mpg),col="blue",lwd=2,lty=1) 

library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars, lwd=2, span=0.75, 
            main="Scatter Plot of MPG vs. Weight by # Cylinders", 
            xlab="Weight of Car (lbs/1000)", 
            ylab="Miles Per Gallon", 
            legend.plot=TRUE, 
            id.method="identify", 
            labels=row.names(mtcars), 
            boxplots="xy" 
) 

#2.散点图矩�?
pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Basic Scatter Plot Matrix") 

#3.高密度散点图
#重叠严重的数据点
set.seed(1234) 
n <- 10000 
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2) 
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2) 
mydata <- rbind(c1, c2) 
mydata <- as.data.frame(mydata) 
names(mydata) <- c("x", "y")
#标准散点�?
with(mydata,plot(x,y,pch=19,main="Scatter Plot with 10000 obs")) 
#核密度估计，颜色表示分布 smoothScatter() 
with(mydata,smoothScatter(x,y,main="Scatter plot colored by smooth densities"))
#六边形单�? hexbin() 
library(hexbin) 
with(mydata,{bin <- hexbin(x,y,xbins=50)
            plot(bin,main="Hexagonal Binning")})

#4.三维散点�?
library(scatterplot3d)
attach(mtcars) 
scatterplot3d(wt,disp,mpg,main="basic 3D scatter plot") 
s3d <- scatterplot3d(wt,disp,mpg,
              pch=16,
              highlight.3d = TRUE,
              type="h",
              main="3D scatter plot with vertical lines") 
fit <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit)

#5.气泡�?
symbols(x,y,circle=radius) 
r <- sqrt(disp/pi) 
symbols(wt,mpg,circle=r,inches=0.30,
        fg="white",bg="lightblue",
        main="bubble plot")
text(wt,mpg,rownames(mtcars),cex=0.6) 


#6.折线�?
opar <- par(no.readonly=TRUE) 
par(mfrow=c(1,2)) 
t1 <- subset(Orange, Tree==1) 
plot(t1$age, t1$circumference, 
     xlab="Age (days)", 
     ylab="Circumference (mm)", 
     main="Orange Tree 1 Growth") 
plot(t1$age, t1$circumference, 
     xlab="Age (days)", 
     ylab="Circumference (mm)", 
     main="Orange Tree 1 Growth", 
     type="b") #b:连线
par(opar)

Orange$Tree <- as.numeric(Orange$Tree) 
ntrees <- max(Orange$Tree) 
xrange <- range(Orange$age) 
yrange <- range(Orange$circumference) 
plot(xrange,yrange,
     type="n",
     xlab="Age(days)",
     ylab="Circumference") 
colors <- rainbow(ntrees) 
linetype <- c(1:ntrees)
plotchar <- seq(18,18+ntrees,1) 
for (i in 1:ntrees) {
  tree <- subset(Orange,Tree=i)
  lines(tree$age,tree$circumference,
        type="b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pch=plotchar[i]) 
}
title("Tree Growth","Example of line plot") 
legend(xrange[1],yrange[2],
       1:ntrees,
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree") 


#7.correlation plot
options(digits = 2)
cor(mtcars) 
install.packages("corrgram") 
library(corrgram)
#corrgram(x, order=, panel=, text.panel=, diag.panel=) 
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Corrgram of mtcars intercorrelations")
corrgram(mtcars,lower.panel = panel.shade,
         upper.panel = NULL,text.panel = panel.txt,
         main="Car Mileage Data") 
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1", 
                           "darkkhaki", "darkgreen"))
corrgram(mtcars,order=TRUE,col.regions = cols,
         lower.panel = panel.shade,
         upper.panel = panel.conf,
         text.panel = panel.txt,
         main="A Corrgram (or Horse) of a different color") 
ftable(Titanic) 
mosaic(table) 
mosaic(formula,data=) 
library(vcd) 
mosaic(Titanic,shade=TRUE,legend=TRUE) 
library(vcd) 
mosaic(~Class+Sex+Age+Survived,data=Titanic,shade=TRUE,legend=TRUE) 













