#ch17 classification 
pkgs <- c("rpart","rpart.plot","party","randomForest","e1071") 
install.packages(pkgs,depend=TRUE) 

loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc,ds,sep="") 
breast <- read.table(url,sep=",",header = FALSE,na.strings = "?")
names(breast) <-  c("ID", "clumpThickness", "sizeUniformity", 
                    "shapeUniformity", "maginalAdhesion",
                    "singleEpithelialCellSize", "bareNuclei", 
                    "blandChromatin", "normalNucleoli", "mitosis", "class")
df <- breast[-1]
df$class <- factor(df$class,level=c(2,4),
                   labels=c("benign","malignant")) 
set.seed(1234) 
train <- sample(nrow(df),0.7*nrow(df)) #subsets 
df.train <- df[train,] #train set 
df.validate <- df[-train,] #test set 
table(df.train$class) 
table(df.validate$class)

#logistic regression 
fit.logit <- glm(class~.,data=df.train,family = binomial())
summary(fit.logit) 
prob <- predict(fit.logit,df.validate,type="response") 
logit.pred <- factor(prob>0.5,levels=c(FALSE,TRUE),
                     labels=c("benign","malignant"))
logit.perf <- table(df.validate$class,logit.pred,
                    dnn=c("Actual","Predicted")) 
logit.perf
accuracy = (129+69)/200 
logit.fit.reduced <- step(fit,logit) 


#decision tree 
library(rpart)
set.seed(1234) 
dtree <- rpart(class ~.,data=df.train,method="class",
               parms=list(split="information")) 
dtree$cptable
plotcp(dtree) 
dtree.pruned <- prune(dtree,cp=0.0125) 
library(rpart.plot)
prp(dtree.pruned,type=2,extra=104,
    fallen.leaves=TRUE,main="Decision Tree") 
dtree.pred <- predict(dtree.pruned,df.validate,type="class")
dtree.perf <- table(df.validate$class,dtree.pred,
                    dnn=c("Actual","Predicted")) 
dtree.perf 

#conditional inference tree 
library(party) 
fit.ctree <- ctree(class~.,data=df.train)
plot(fit.ctree,main="Condtional inference tree") 
ctree.pred <- predict(fit.ctree,df.validate,type="response") 
ctree.perf <- table(df.validate$class,ctree.pred,
                    dnn=c("Actual","Predicted")) 
ctree.perf 


#random forest 
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~.,data=df.train,
                           na.action=na.roughfix,
                           importance=TRUE) 
fit.forest 
importance(fit.forest,type=2) 
forest.pred <- predict(fit.forest,df.validate)
forest.perf <- table(df.validate$class,forest.pred,
                     dnn=c("Actual","Predicted")) 
forest.perf

#SVM 
set.seed(1234)
packages <- c("kernlab","e1071") 
install.packages(packages,depend=TRUE) 
library(e1071)
fit.svm <- svm(class~.,data=df.train)
fit.svm
svm.pred <- predict(fit.svm,na.omit(df.validate)) 
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred,dnn=c("Actual","Predicted")) 
svm.perf 
#with RBF core
tuned <- tune.svm(class~., data=df.train, 
                  gamma=10^(-6:1),
                  cost=10^(-10:10))
tuned 
fit.svm <- svm(class~.,data=df.train,gamma=0.001,cost=1) 
svm.pred <- predict(fit.svm,na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class,
                  svm.pred,dnn=c("actual","predicted")) 
svm.perf 
#accuracy 
performance <- function(table,n=2){
  if(!all(dim(table)==c(2,2))) 
  stop("must be a 2*2 table")
tn = table[1,1]
fp = table[1,2]
fn = table[2,1]
tp = table[2,2]
sensitivity <- tp/(tp+fn) 
specificity <- tn/(tn+fp) 
ppp <- tp/(tp+fp) 
npp <- tn/(tn+fn) 
hitrate <- (tp+tn)/(tp+tn+fp+fn) 
result <- paste("Sensitivity = ", round(sensitivity, n) , 
                "\nSpecificity = ", round(specificity, n), 
                "\nPositive Predictive Value = ", round(ppp, n),
                "\nNegative Predictive Value = ", round(npp, n),
                "\nAccuracy = ", round(hitrate, n), "\n", sep="") 
cat(result) 
}
performance(logit.perf) 
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf) 
performance(svm.perf) 

#rattle 
install.packages("rattle") 
install.packages("RGtk2")
library(rattle)
rattle() 
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/" 
ds <- "postoperative-patient-data/post-operative.data" 
url <- paste(loc, ds, sep="") 
patient <- read.table(url, sep=",", header=FALSE) 
head(patient)
names(patient) <- c("L-CORE", "L-SURF", "L-O2", "L-BP", 
                     "SURF-STBL", "CORE-STBL", "BP-STBL", "COMFORT", "decision ADM-DECS") 
library(rattle)
rattle() 
