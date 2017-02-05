library(ISLR)
data("Hitters")
str(Hitters)
summary(Hitters)
View(Hitters)
library(ggplot2)
par(mfrow=c(1,1))
hist(Hitters$Salary)
qplot(log(Hitters$Salary))
Hitters$Salary<-log(Hitters$Salary)
summary(Hitters)
Hitters<-na.omit(Hitters)
# Split the Data
library(caTools)
set.seed(3000)
split<-sample.split(Hitters$Salary,0.8)
Train<-subset(Hitters,split==T)
Test<-subset(Hitters,split==F)
## Apply CART 
library(rpart)
model<-rpart(Salary~.,data = Train,minbucket=5)
pred<-predict(model,newdata = Test)
RMSE<-sqrt(sum((pred-Test$Salary)^2)/nrow(Test))
RMSE
library(rpart.plot)
prp(model)
model$cptable
plotcp(model)
library(caret)
numFolds<-trainControl(method="cv",number=10)
cpGrid<-expand.grid(cp=seq(0.005,0.1,0.001))
set.seed(100)
model_cv<-train(Salary~.,data = Train,trControl=numFolds,tuneGrid=cpGrid,method="rpart")
model_cv
plot(model_cv)
prp(model_cv$finalModel)
# Make Predictions
pred_cv<-predict(model_cv,newdata = Test)
#RMSE 
RMSE<-sqrt(sum((pred_cv-Test$Salary)^2)/nrow(Test))
RMSE
# Apply Randomforest
library(randomForest) 
set.seed(50)
model_rf<-randomForest(Salary~.,data=Train,nodesize=10,ntree=200,mtry=3) 
plot(model_rf) 
model_rf
# Applying cv on random forest
model_rfcv<-rfcv(Train[,c(1:18,20)],Train$Salary,cv.fold = 10)
model_rfcv$error.cv
#Making Predictions
pred_rf<-predict(model_rf,newdata = Test)
RMSE_rf<-sqrt(sum((pred_rf-Test$Salary)^2)/nrow(Test))
RMSE_rf
SSE_rf<-sum((pred_rf-Test$Salary)^2)
Rsq<-1-SSE/SST
# Boosted Regression Tree
library(xgboost)
library(Matrix)
sparse<-sparse.model.matrix(Salary~.-1,data = Train)
sparse@Dimnames
dtrain<-xgb.DMatrix(data=sparse,label=Train$Salary)
sparse_test<-sparse.model.matrix(Salary~.-1,data = Test)
mat<-as.matrix(sparse_test)
dtest<-xgb.DMatrix(data=sparse_test,label=Test$Salary)
getinfo(dtrain,"label")
# Building the model
watchlist=list(train=dtrain,test=dtest)
model_xgb<-xgb.train(data=dtrain,nrounds=100,objective="reg:linear",verbose = 2,eta=0.1,max_depth=6,watchlist = watchlist)
model_xgb
