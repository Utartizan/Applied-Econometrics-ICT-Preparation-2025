# ============================================================================= #
set.seed(1234)
#
# load packages (install if not installed previously)
library(party)
library(Ecdat)
library(caret)
# load mortgage applications dataset
data(Hdma)
?Hdma # mortgage applications dataset descriptions 
#
# fix spelling error and leave only completed observations in the dataset 
names(Hdma)[11] <- "condo"
Hdma=Hdma[complete.cases(Hdma),]
#
# ============================================================================= #
#
# run:
reg1=lm(as.numeric(deny)-1~., data=Hdma)                                        # linear probability model
reg2=glm(deny~., data=Hdma, family=binomial(link='logit'))                      # logit model
reg3=glm(deny~., data=Hdma, family=binomial(link='probit'))                     # probit model
#
# obtain and discuss output for:
summary(reg1)                                                                   # linear probability model 
summary(reg2)                                                                   # logit model     
summary(reg3)                                                                   # probit model
#
# obtain predictions for:
lpm.pred=fitted(reg1)                                                           # linear probability model    
log.pred=fitted(reg2)                                                           # logit model 
pro.pred=fitted(reg3)                                                           # probit model
#
# plot and discuss predictions for:
par(mfrow=c(1,2))
plot(lpm.pred, ylim=c(-0.2,1.25), pch="x", xlab="", ylab="Prob.", main="LPM")   # linear probability model
abline(h=c(0,1), lty=2, col="red", lwd=2)
plot(log.pred, ylim=c(-0.2,1.25), pch="+", col="blue", xlab="", ylab="Prob.", 
                                                       main="Logit and Probit") # logit model
points(pro.pred, ylim=c(-0.2,1.25), pch="*", col="green")                       # probit model
abline(h=c(0,1), lty=2, col="red", lwd=2)
#
# There are several options to evaluate goodness of fit for classification tasks.
# One of the simplest is to construct a confusion matrix from the caret package;
# also can be constructed manually with table() command.
#
reg1.conf=confusionMatrix(as.factor(lpm.pred>0.5), as.factor(Hdma$deny=="yes"))
reg2.conf=confusionMatrix(as.factor(log.pred>0.5), as.factor(Hdma$deny=="yes"))
reg3.conf=confusionMatrix(as.factor(pro.pred>0.5), as.factor(Hdma$deny=="yes"))
#
# ============================================================================= #
#
tree1=ctree(deny~., data=Hdma)
tree1.pred=predict(tree1)
#
tree1.conf=confusionMatrix(tree1.pred, Hdma$deny)
#
# ============================================================================= #
#
# discuss confusion matrices output for:
reg1.conf                                                                       # linear probability model
reg2.conf                                                                       # logit model
reg3.conf                                                                       # probit model
tree1.conf                                                                      # reg. tree model
#
# ============================================================================= #
#
# Does ethnical origin matter? There is hope, if you know how to interpret your
# output well. So, let's drop the variable black from the dataset and perform
# similar steps as above ...
Hdma1=Hdma[,-12]
#
# run models for the new dataset:
reg4=glm(deny~., data=Hdma1, family=binomial(link='logit'))                     # logit model
tree2=ctree(deny~., data=Hdma1)                                                 # regression tree 
#
# obtain predictions for:
reg4.pred=fitted(reg4)                                                          # new logit model 
tree2.pred=predict(tree2)                                                       # new regression tree
#
# obtain confusion matrices for:
reg4.conf=confusionMatrix(as.factor(reg4.pred>0.5),                             # new logit model 
                          as.factor(Hdma1$deny=="yes")) 
tree2.conf=confusionMatrix(tree2.pred, Hdma1$deny)                              # new regression tree
#
# discuss confusion matrices for:
reg4.conf                                                                       # new logit model
tree2.conf                                                                      # new regression tree
#
# ============================================================================= #