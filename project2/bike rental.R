
#.................................project- Bike rental count ...............................................#
rm(list=ls(all=T))
# setting up working directory for project cab prediction 
setwd("R:/vishakha r progaram/projects")
getwd()
#loading required libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#installing packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#................................Loading data......................................................#
#importing data from directory 

Data_d = read.csv("day.csv",header = T)

View(Data_d)

#............................Explorin Data Analysis..............................................#
str(Data_d)
class(Data_d)
names(Data_d)
summary(Data_d)

#remove some unwanted variable
# isntant = giving information of index so not important variable
# dteday = having date tym there is no reqire of this variable
# casual, registered = by combining this two variable we getiing cnt 

Data_d = subset(Data_d, select= -c(instant,dteday,registered,casual))

str(Data_d)
names(Data_d)
head(Data_d)

#checking for nunique value of each variable
apply(Data_d, 2,function(x) length(table(x)))


#grouping the categorical nad numerical variable 
cat_var = c('season','yr','mnth','holiday','weekday','workingday','weathersit') 
num_var = c('temp','atemp','hum','windspeed','cnt')



Data1 = Data_d
#.................................Data Pre Processing .........................................#

# 1.Missing value analysis
# checking missing value of data

apply(Data_d,2,function(x){sum(is.na(x))})

# there is no  missing value 

#2. Outliyer Analysis
#visualization of outlier with plot

for (i in 1:length(num_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_var[i]), x = "cnt"), data = subset(Data_d))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=num_var[i],x="count")+
           ggtitle(paste("Box plot of count for",num_var[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

# Here we have found that hum and windspeed has some outlier 
# outlier treatment 
# convert outlier into NA
for(i in num_var){
  val_outlier = Data_d[,i][Data_d[,i] %in% boxplot.stats(Data_d[,i])$out]
  print(length(val_outlier))
  Data_d[,i][Data_d[,i] %in% val_outlier] = NA
}

#imputing outlier with help of knn method
Data_d = knnImputation(Data_d, k = 5)
sum(is.na(Data_d))

#...................................Data Understanding with vizualisation..............................#
# method which will plot barplot of a columns with respect to other column
#ploting graph season vs cnt
ggplot(Data_d, aes(x = Data_d$season, y = Data_d$cnt))+
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Number of bikes rented with respect to season", x = "Seasons", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#here found that season 3, has the highest count of bikes and season 1 has lowest count of bikes

#ploting graphs yr vs cnt
ggplot(Data_d, aes(x = Data_d$yr, y = Data_d$cnt))+
  geom_bar(stat = "identity", fill = "grey")+
  labs(title = "Number of bikes rented with respect to yr", x = "yr", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#plot conlclude that 1=2011 has highestcount of renting bike than 0 = 2010

#ploting graphs month vs cnt
ggplot(Data_d, aes(x = Data_d$mnth, y = Data_d$cnt))+
  geom_bar(stat = "identity", fill = "pink")+
  labs(title = "Number of bikes rented with respect to mnth", x = "mnth", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#plot conclude that month 8 and 9 has highest count of renting bike and month 1 has lowest count of bike renting

#ploting of graph weekday vs cnt
ggplot(Data_d, aes(x = weekday, y = cnt))+
  geom_bar(stat = "identity", fill = "green")+
  labs(title = "Number of bikes rented with respect to weekday", x = "weekday", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#plot conclude that bike count is highest in the week day of 4th and 5th of week day

#Count with respect to temperature and humidity together
ggplot(Data_d,aes(temp,cnt)) + 
  geom_point(aes(color=hum),alpha=0.5) +
  labs(title = "Bikes count vs temperature and humidity", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
  

#it is found that when normalized temperature is between 0.5 to 0.75 and humidity is between 0.4 to 0.8, count is high


# Count with respect to windspeed and weather together
ggplot(Data_d, aes(x = windspeed, y = cnt))+
  geom_point(aes(color= weathersit ), alpha=0.5) +
  labs(title = "Bikes count vs windspeed and weather", x = "Windspeed", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

# It is found that count is at peak, when windspeed is from 0.1 to 0.3 and weather is from 1.0 to 1.5.


# Count with respect to temperature and season together

ggplot(Data_d, aes(x = temp, y = cnt))+
  geom_point(aes(color=season),alpha=0.5) +
  labs(title = "Bikes count vs temperature and season", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
# it is found that count is maximum when temperature is 0.50 to 0.75 & season 2 to season 4


#.......................................Feature Selection.........................................#

# feature selection by the checking 
#correlation among the variable in the case of numerical variable
#Correlation Plot
corrgram(Data_d[,num_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis between numeric variables")
#it is found that temperature and atemp are highly correlated with each other.

# in given condition our target variable is continous 
#for checking correlation, using anova test 
for(i in cat_var){
  print(i)
  Anova_test_result = summary(aov(formula = cnt~Data_d[,i],Data_d))
  print(Anova_test_result)
}

 
#it is found that holiday, weekday and workingday has p value > 0.05.we go for null hypothesis.
#by the checking the hypothesisi we have found and selecting significant varialbe
#so we droping unwanted variable
 
Data_d = subset(Data_d,select = -c(atemp,workingday,weekday,holiday))

#so new variable now 
cat_var = c('season','yr','mnth','weathersit') 
num_var = c('temp','hum','windspeed','cnt')


#checking  multicollinearity

variable_n = Data_d[,num_var]

#importing reqired library
library(usdm)

vifcor(variable_n, th = 0.7)

#No variable from the 4 input variables has collinearity problem

#...........................................feature Scaling.....................................#

#checking for normality visualisation

hist(Data_d$temp, col="Navyblue", xlab="Temperature", ylab="Frequency",
     main="Temperature Distribution")

hist(Data_d$hum, col="Yellow", xlab="Humidity", ylab="Frequency",
     main="Humidity Distribution")

hist(Data_d$windspeed,col="Dark green",xlab="Windspeed",ylab="Frequency",
     main="Windspeed Distribution")

#all histogram showing that data is symmetric in nature

# Identify range and check min max of the variables to check noramility

for(i in num_var){
  print(summary(Data_d[,i]))
}
#data is found as normalized, no need to scaling
Data2 = Data_d


#.........................................Model development.....................................#

#collectint required data to perform model development  

rmExcept("Data_d")

#creating some reqired dummies variable  for categorical variable
cat_var = c("season","yr","mnth","weathersit")

library(dummies)
Data_d = dummy.data.frame(Data_d, cat_var)


#mape error
MAPE = function(y,y1){
  mean(abs((y-y1)/y))*100
}

#R Square 

Rsquare = function(x,x1){
  cor(x,x1)^2
}
 
#saving the data for cross validation 

cv_data = Data_d

#so spliting the data into trian and test subset for modeling
set.seed(123)
train_index = sample(1:nrow(Data_d),0.8*nrow(Data_d))
train1= Data_d[train_index,]
test1= Data_d[-train_index,]



#............................Decision tree...................................................#

#deploying decison tree model 
DTModel = rpart(cnt~., train1, method = "anova" , minsplit=5)

summary(DTModel)

# Predictions decesion tree

DTTest = predict(DTModel, test1[-25])


summary(DTModel)

#mape
DTMape_Test = MAPE(test1[,25], DTTest)

DTMape_Test #26.4225

#RSquare

DT_RSquare = Rsquare(test1[,25], DTTest)
DT_RSquare  #0.7612102

#Accuracy 
Accuracy_DTModel=(100-DTMape_Test)
Accuracy_DTModel#73.5775
#............................linear Regression..............................................#

# deploying linear regression model
lm_model = lm(cnt ~. , data = train1)


summary(lm_model)

#prediction of lm_mode
LMTest= predict(lm_model, test1[,-25])


#mape
LRMape_Test = MAPE(test1[,25], LMTest)
LRMape_Test 
#21.57545

#RSquare
LR_RSquare = Rsquare(test1[,25],LMTest)
LR_RSquare
#0.8191175

#Accuracy 
Accuracy_lmModel=(100-LRMape_Test)
Accuracy_lmModel#78.43208


#.................................Random Forrest................................................#
#Deploying random forrest 
RFModel = randomForest(cnt~., train1, ntree = 500, importance = TRUE)

summary(RFModel)

importance(RFModel, type = 1)


# Predictions of random forrest

RFTest = predict(RFModel, test1[-25])


# MAPE

RFMape_Test = MAPE(test1[,25], RFTest)
RFMape_Test  
# 19.38623

#RSquare

RF_RSquare = Rsquare(test1[,25], RFTest)
RF_RSquare   
#0.8678384

#Accuracy
Accuracy_Rfmodel =(100-RFMape_Test)
Accuracy_Rfmodel#80.53485




#.............................XGBOOST algorithm................................................#
library(xgboost)

train_data_matrix = as.matrix(sapply(train1[-25],as.numeric))
test_data_data_matrix = as.matrix(sapply(test1[-25],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train1$cnt,nrounds = 15,verbose = FALSE)

summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)

xgMape_Test = MAPE(test1[,25], xgb_predictions)
xgMape_Test#18.27699

xg_RSquare = Rsquare(test1[,25], xgb_predictions)
xg_RSquare#0.8602509


#Accuracy
Accuracy_xgbmodel =(100-xgMape_Test)
Accuracy_xgbmodel#81.56554

#....................................Cross Validation process.....................................#

#Load Data
library(caret)


cv_data

#divide data

set.seed(123)
train_index2 = sample(1:nrow(cv_data),0.8*nrow(cv_data))
train_KF = cv_data[train_index2,]
test_KF = cv_data[-train_index2,]

#Random Forest Cross Validation

RF_KF = caret::train(cnt~.,
              data = train_KF,
              method = "rf",
              tuneGrid = expand.grid(mtry = c(2,3,4)),
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE,))



print(RF_KF)


knitr::kable(head(RF_KF$results), digits = 3)

print(RF_KF$bestTune)



RFpreds = predict(RF_KF, test_KF[-25])

RFpreds_MAPE = MAPE(test_KF[,25], RFpreds)
RFpreds_MAPE#21.71615

RFPreds_RSquare = Rsquare(test_KF[,25], RFpreds)
RFPreds_RSquare# 0.8723197



#Decision Tree Cross Validation


DT_KF = caret::train(cnt~.,
              data = train_KF,
              method = "gbm",
              tuneGrid = expand.grid(n.trees = 200, 
                                     interaction.depth = c(1,2,3), 
                                     shrinkage = 0.1,
                                     n.minobsinnode = 10 ),
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE))

print(DT_KF)


knitr::kable(head(DT_KF$results), digits = 3)

print(DT_KF$bestTune)


DTpreds = predict(DT_KF, test_KF[-25])

DTpreds_MAPE = MAPE(test_KF[,25], DTpreds)
DTpreds_MAPE#19.01021

DTPreds_RSquare = Rsquare(test_KF[,25], DTpreds)
DTPreds_RSquare#0.8693922



#Linear Regression CV


LR_KF = caret::train(cnt~.,
              data = train_KF,
              method = "lm",
              tuneGrid = expand.grid(intercept = TRUE),
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE))

print(LR_KF)

knitr::kable(head(LR_KF$results), digits = 3)

print(LR_KF$bestTune)

LRpreds = predict(LR_KF, test_KF[-25])

LRpreds_MAPE = MAPE(test_KF[,25], LRpreds)
LRpreds_MAPE#21.56792

LRPreds_RSquare = Rsquare(test_KF[,25], LRpreds)
LRPreds_RSquare#0.8191175


#xgboost cv
library(caTools)

parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,
                               subsample=1
)


XG_KF = caret::train(cnt~.,
                     data = train_KF,
                     method = "xgbTree",
                     tuneGrid=parametersGrid,
                     trControl = trainControl(method = "cv",
                                              number = 5,
                                              verboseIter = FALSE))
              
              
print(XG_KF)

knitr::kable(head(XG_KF$results), digits = 3)

print(XG_KF$bestTune)

XGpreds = predict(XG_KF, test_KF[-25]) 

XGpreds_MAPE = MAPE(test_KF[,25], XGpreds)
XGpreds_MAPE#18.70519

XGpreds_RSquare = Rsquare(test_KF[,25], XGpreds)
XGpreds_RSquare#0.8738172
