rm(list = ls())
#task 1
library(caret)
library(tidyverse)
library(car)
library(rpart)

#reading env and fish data, and summarizing fish abundance data by sites, 
#and combining env and total fish to a new data frame named as "env_fish".

data(doubs,package="ade4")
env_fish<-doubs$env %>%
  mutate(total_fish = rowSums(doubs$fish[1:27]))

env_fish

#task2
#visualizing the features of the new env_fish set using scatterplot(). 
featurePlot(x=env_fish[, -12],
            y=env_fish[, 12],
            plot = "scatter",
            type=c("p","smooth"),
            layout=c(3,4))
#task3
#delete sites which have no fishes .
env_fish<-subset(env_fish,total_fish!=0)

#  removing all rows where any column contains an outlier.
for(i in 1:12){
  r<-which(env_fish[ ,i] %in% boxplot.stats(env_fish[ ,i])$out)
  print(r)
}

env_fish<-env_fish[-c(1,2,3,6,14,22:25,28,29), ]

#task4
#identifying near zero-variance, outlies of the env variables.
#and excluding them for analysis.
nearZeroVar(env_fish,name=T,saveMetrics= TRUE)
#task5
#detecting the collinearity among env variables or removing highly correlated features
#(with an absolute correlation of 0.75 or higher) 
env_fishCor <-  cor(env_fish[ ,-12])
highlyCor<- findCorrelation(env_fishCor, cutoff = .75)
env_fish1 <- env_fish[,-highlyCor] 
comboInfo <- findLinearCombos(env_fish1)
comboInfo
env_fish1[, -comboInfo$remove]
dim(env_fish1)

#task6
#splitting data into training and test sets
set.seed(111)
train_index<-createDataPartition(env_fish1$total_fish,p=0.8,list = F)
#createDataPartition(x,time,p,list=F/T,groups = min(5,length(x)))
#x是包含了多分类信息的向量，time表示需要进行抽样的次数
#p表示需要从数据中抽取的样本比例，list表示结果是否是为list形式，默认为T，
#groups表示如果输出变量是数值型数据，则默认按分位数分组进行取样。
training<-env_fish1[train_index,]
test<-env_fish1[-train_index,]

#visualizing the features and targets of the training set
#featurePlot(trainx[,1:2],trainy,plot="box")

x<-as.matrix(env_fish1[ ,1:6])
y<-as.factor(env_fish1$total_fish)
#visualizing the features and targets of the training set
featurePlot(x,y,plot="density")

#task7
set.seed(222)
names(getModelInfo())
#train(x, y, method = "rf", preProcess = NULL, ...,
     # weights = NULL, metric = ifelse(is.factor(y), "Accuracy",  "RMSE"),
      #maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE),
      #trControl = trainControl(), tuneGrid = NULL,
      #tuneLength = ifelse(trControl$method == "none", 1, 3))

#Creating and evaluating a baseline model between the environmental variables
#and the total fish abundance with the tree-based algorithm
model1<-rpart(formula = total_fish ~.,
      data = training,
      control=rpart.control(minsplit=2),
      method = "anova")
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model1)
#task8
model1.pred <- predict(model1, test)#预测模型在测试集中的表现
#评估模型优劣
#install.packages("Metrics")
library(Metrics)
rmse(actual=test$total_fish,
     predicted=model1.pred)

#Build the model with random forests
fitControl<-trainControl(
  method = "repeatedcv",
  number = 30,
  repeats = 30)
set.seed(333)
model2<-train(total_fish~.,data=training,
              method="rf",
              trControl=fitControl,
              metric="RMSE",
              verbose=T)
model2
#Comparing the rf and rpart models
# found that the rf model is more suitable because the RMSE is smaller

