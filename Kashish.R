#################################################################################################
library(car)
library(splines)
library(gam)
library(reshape2)
library(ggplot2)
library(Metrics)
library(tree)

#data input
MyData = read.csv(file="E:/Spring 2018/Methods for Data Science/Project/sgemm_product.csv", header=TRUE, sep=",")

#exploratory analysis
summary(MyData$MWG)
summary(MyData$SA)
summary(MyData$VWN)
summary(MyData$Run_time) 

#exploring summary plot for target variable
boxplot(MyData$Run_time)

#plotting histogram for few predictors 
hist(MyData$MWG,col = "gold")
hist(MyData$NDIMC,col="green")
hist(MyData$Run_time,col="green")
summary(MyData$Run_time)

#checking linearity based on individual variable
plot(MyData$Run_time,MyData$NDIMC)
barplot(table(MyData$STRN))

#correlation plot between variables
pairs(MyData)

#preprocessing
MyData$STRM=as.factor(MyData$STRM)
MyData$STRN=as.factor(MyData$STRN)
MyData$SA=as.factor(MyData$SA)
MyData$SB=as.factor(MyData$SB)
MyData$MWG=as.factor(MyData$MWG)

# visualizing to check correlation between the variables
correlation=round(cor(MyData),2)

#plotting correlation plot
melted_correlation= melt(correlation)
ggplot(data = melted_correlation, aes(x=Var1, y=Var2, fill=value)) + 
geom_tile()


#MyData$Run_time =  ( MyData$Run_time - mean(MyData$Run_time)) / sd(MyData$Run_time)

#converting integer values as factors
col_names <- sapply(MyData, function(col) length(unique(col)) < 4)
MyData[ , col_names] <- lapply(MyData[ , col_names] , factor)


#splitting into training and testing into 80% training and 20% testing
samplesize=floor(0.80*nrow(MyData))
set.seed(111)
train = sample(seq_len(nrow(MyData)),size=samplesize)
training=MyData[train,]
testing=MyData[-train,]

#modeling of linear model
linearmodel=lm(Run_time~.,data = training)
summary(linearmodel)
linearmodel=update(linearmodel,~.-STRN)

# removing insignificant variable from above model
fit1=lm(Run_time~MWG,data = training)
summary(fit1)
plot(fit1)

#checking collinearity
vif(linearmodel)

#visualization 
par(mfrow=c(2,2))
plot(linearmodel)
plot(predict (linearmodel), residuals (linearmodel)) 
plot(predict (linearmodel), rstudent (linearmodel))

# checking for leverage statistics

plot(hatvalues (linearmodel)) 
which.max(hatvalues (linearmodel))

#applying non linear method
par(mfrow=c(1,1))
gam.1=gam( Run_time ~ s(MWG) + s(NWG) + KWG + MDIMC +NDIMC + MDIMA + 
             NDIMB + KWI + s(VWM) + s(VWN) + STRM  + SA + SB,data=training)
summary(gam.1)
plot(gam.1,se=TRUE)
gam.2=gam(Run_time ~ MWG + NWG + KWG + MDIMC +NDIMC + MDIMA + 
            NDIMB + KWI + s(VWM) + s(VWN) + STRM  + SA + SB,data=training)
gam.3=gam(Run_time ~ MWG + NWG + KWG + MDIMC +NDIMC + MDIMA + 
            NDIMB + KWI + VWM + s(VWN) + STRM  + SA + SB,data=training)
gam.4=gam(Run_time ~ MWG + NWG + KWG + MDIMC +NDIMC + MDIMA + 
            NDIMB + KWI + VWM + VWN + STRM  + SA + SB,data=training)
#comparing variance analysis of different models
anova(linearmodel,gam.1,gam.2,gam.3,gam.4)

summary(gam.2)

#applying regression tree 
tree_1=tree(Run_time~., data = training)
summary(tree_1)
plot(tree_1)
text(tree_1,pretty=0)

require(rpart)
fit=rpart(Run_time~., data = training)
#analysing importance of variable in regression tree
fit$variable.importance
summary(fit)
plot(fit)
text(fit,pretty=0)

#checking the terminal nodes of the tree with the  lowest deviation value
cv=cv.tree(tree_1)
plot(cv$size,cv$dev, type = 'b') 

#prediction using Linear model on testing data
pred=predict(linearmodel,newdata = testing)
Linear_RMSE=sqrt( sum( (testing$Run_time - pred)^2) / nrow(testing) )
Linear_RMSE

#predicting using  different GAM model
pred1=predict(gam.1,newdata = testing)
Gam1_RMSE=sqrt( sum( (testing$Run_time - pred1)^2) / nrow(testing) )
Gam1_RMSE

pred2=predict(gam.2,newdata = testing)
Gam2_RMSE=sqrt( sum( (testing$Run_time - pred2)^2) / nrow(testing) )
Gam2_RMSE

pred3=predict(gam.3,newdata = testing)
Gam3_RMSE=sqrt( sum( (testing$Run_time - pred3)^2) / nrow(testing) )
Gam3_RMSE

#predicting using tree
prediction=predict(tree_1,newdata = testing)
TREE_RMSE=sqrt( sum( (testing$Run_time - prediction)^2) / nrow(testing) )
TREE_RMSE

#plotting comparison of  RMSE value for all three moedels
RMSE=c(Linear_RMSE,TREE_RMSE,Gam1_RMSE)
x=c("Linearregression","Regression_TRee","General Additive Model")
qplot(x, RMSE,type ="b")
ggplot(data.frame(x, comparison), aes(x,RMSE))+geom_point(aes(colour="red",size=20))
#############################################################################################################

