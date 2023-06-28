# Libraries
library(corrplot)
library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)
install.packages("stargazer")
library(stargazer) #Create a ready to use table in LaTeX style
# https://www.r-bloggers.com/2013/01/stargazer-package-for-beautiful-latex-tables-from-r-statistical-models-output/
library(MASS)


setwd("/Users/manuelscionti/Desktop/Yelp/")
source("DailyLevelData_analysis_functions.r")

# Loading the dataset
yelp_data<-read.csv("/Users/manuelscionti/Desktop/Yelp/df_complete.csv")

########## EXPLORATORY ANALYSIS ###################

dim(yelp_data) #dimension df
sum(!complete.cases(yelp_data)) #number of NaN values (0)
names(yelp_data) #names columns
str(yelp_data) #statistical info, variable type...
summary(yelp_data)

# ---- Cleaning & Preprocessing ------

#Let's drop some not useful variables for our ML analysis
yelp_data <- subset(yelp_data, select = -c(X
                                             ,WE
                                             ,business_id.x
                                             ,date.x
                                             ,bus_name
                                             ,city
                                             ,wifi
                                             ,tv
                                             ,noise
                                             ,goodforgroups
                                             ,outdoorseating
                                             ,creditcardpayment
                                             ,alcohol
                                             ,bikeparking
                                             ,weekdays
                                             ,review_count.y) )

yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="Noch_in")

# 1) Cast all the variables with their right datatype
yelp_data$business_park <- factor(yelp_data$business_park, levels = c('TRUE', 'FALSE'), labels = c("1", "0"))
yelp_data$business_park <- as.numeric(yelp_data$business_park)-1
yelp_data$business_cat <- factor(yelp_data$business_cat)
yelp_data$Quarter <- factor(yelp_data$Quarter)
yelp_data$business_open=factor(yelp_data$business_open)
yelp_data$business_open <- as.numeric(yelp_data$business_open)-1


str(yelp_data)
View(yelp_data)

physical_attr <- c("ch_in","business_price", "business_open", "business_park","business_cat"
                   ,"wifi_dummy","tv_dummy","bikeparking_dummy","goodforgroups_dummy"
                   ,"outdoorseating_dummy","creditcardpayment_dummy","noise_level","alcohol_dummy","n_photo")

social_attr <- c("ch_in","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip"
                ,"stars","review_count.x","avg_sentiment_score_review","sum_elite_status","max_friends_count"
                ,"male","female","sum_fans","avg_stars")

external_attr <- c("ch_in","business_lat","business_long","PRCP","SNOW","SNWD","TMAX",
                   "TMIN","TOBS","TOBS_1","TOBS_2","TOBS_3","TOBS_4","Quarter","weekend")

yelp_data_physical=subset(yelp_data,select=physical_attr)
yelp_data_social=subset(yelp_data,select=social_attr)
yelp_data_external=subset(yelp_data,select=external_attr)


#LaTeX tables - output must be copied in a LaTeX editor (Overleaf)
stargazer(yelp_data_physical)
stargazer(yelp_data_social)
stargazer(yelp_data_external)



# Select only the numeric columns - physical
numeric_columns_physical <- yelp_data_physical %>% 
  select_if(function(x) is.numeric(x)) 


correlation.matrix <- cor((numeric_columns_physical))
# LaTeX table
stargazer(correlation.matrix, title="Correlation Matrix - Physical Attribues")
# Correlation plot
corrplot(correlation.matrix, method="color", type="upper",tl.col = "black",)

# Select only the numeric columns - social
numeric_columns_social <- yelp_data_social %>% 
  select_if(function(x) is.numeric(x)) 


correlation.matrix <- cor((numeric_columns_social))
# LaTeX table
stargazer(correlation.matrix, title="Correlation Matrix - Social Attribues")
# Correlation plot
corrplot(correlation.matrix, method="color", type="upper",tl.col = "black",) 


# Select only the numeric columns - external
numeric_columns_external <- yelp_data_external %>% 
  select_if(function(x) is.numeric(x)) 


correlation.matrix <- cor((numeric_columns_external))
# LaTeX table
stargazer(correlation.matrix, title="Correlation Matrix - External Attribues")
# Correlation plot
corrplot(correlation.matrix, method="color", type="upper",tl.col = "black",) 


####### STEPWISE VARIABLE SELECTION ########
# It takes a lot of time

# library(MASS)
# 
# # Specify your initial full model with all potential predictor variables
# full_model <- glm(ch_in~-ch_in_string, data = yelp_data, family = binomial)
# 
# # Perform stepwise variable selection using AIC
# stepwise_model <- stepAIC(full_model, direction = "both")
# 
# # View the selected variables in the final model
# selected_variables <- stepwise_model$anova$terms
# print(selected_variables)


# A simple regression analysis ----
m1=glm(ch_in~cum_n_tips+cum_max_friends+cum_max_us_fans+cum_max_us_tip+I((male+1)/(female+1))+
         business_price+business_park+business_open+business_cat+n_photo+
         PRCP+SNOW+SNWD+TMAX+TMIN+TOBS_3+Quarter+weekend, data = yelp_data, family = "binomial")
car::vif(m1)
summary(m1)



############# MACHINE LEARNING #######################
# Split randomly
set.seed(66)
yelp_data_na=yelp_data
# list of variables in your model
varsin=c("ch_in_string","ch_in","SNWD","Quarter","business_price","business_open","business_cat","business_park","TOBS","PRCP","n_photo","female","male","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip","weekend")
yelp_data=subset(yelp_data,select=varsin)

# set "/1" for full dataset size
datasetsize=nrow(yelp_data)/10 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))


# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))

# class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)

if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)

############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")

BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))

# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)

################################ ML MODELS #################################

######### LOGISTIC REGRESSION
library(caret)
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)

############ Naive Bayes
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")

x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))

# the variable importance
imp_NB <- (varImp(x.modelNB))
plot(imp_NB, main='NB MODEL')


# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']

NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")

TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

############ KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- train(BaseFormula_dum, data = x.trainnorm, method="knn")

x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))

# the variable importance
imp_KNN <- (varImp(x.modelKNN))
plot(imp_KNN, main="KNN MODEL")




# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']

KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")

TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=12000, tolerance=.01,
                    trControl = trainControl(classProbs =  TRUE))

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")


x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance

imp_SVM <- (varImp(x.modelSVM))
plot(x.modelSVM, main="SVM MODEL")


# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

########## Neural network
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

imp_NNet<- (varImp(x.modelNNet))
plot(imp_NNet, main="NNet MODEL")

# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

########## TREE
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
imp_Tree <- (varImp(x.modelTree))
plot(imp_Tree, main="TREE MODEL")

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
imp_Bagging <- (varImp(x.modelBagging))
plot(imp_Bagging, main="BAGGING MODEL")


BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)



############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
imp_Boosting <- (varImp(x.modelBoosting))
plot(imp_Boosting, main="BOOSTING MODEL")


# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")

x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
imp_RF <- (varImp(x.modelRF))
plot(imp_RF, main="RANDOM FOREST MODEL")

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


# SOME Summarizing plots:

OverallTDL <- c(LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", legend = c("TDL","GINI"), main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit,data=x.evaluate,class="ch_in")

ggplot(lift_obj)

stargazer(x.modelLogit,x.modelNB,x.modelKNN, title='comparison', align =TRUE)

####### MODEL TUNING ####### (WORK IN PROGRESS)

# Take the best model and tune it

# Random Forest
rf_grid <-expand.grid(.mtry=c(2,3,4)) # different settings of mtry can be tested
training <- train(model, data=xsell_train, method="rf", ntree=500, sampsize=40000,  tuneGrid=rf_grid)
training


library(randomForest)
# let'S use e1071: a more flexible algorithm for RF tuning
library(e1071)
nodesize.tuning <- c(50,100,200)
ntree.tuning <- c(200,500,1000)
set.seed(1234) # fix random number generator seed for reproducibility

rf.tuning <- tune.randomForest(model, 
                               data=xsell_train, 
                               replace=TRUE, 
                               sampsize=40000,  
                               mtry=2,
                               nodesize=nodesize.tuning,
                               ntree = ntree.tuning)
rf.tuning

# Use the tuning results to re-run the model

set.seed(1234) # fix random number generator seed for reproducibility
rf_opt <- randomForest(model, data=xsell_train, 
                       ntree=500,       # number of trees
                       mtry=2,          # number variables selected at each node
                       nodesize=50,     # minimum node size
                       maxnodes=3,     # max amount of nodes
                       replace=TRUE,    # sample selection type
                       sampsize=20000)   # size of each sample

# Add prediction to validation data
xsell_valid$pred_rfopt <- predict(rf_opt,newdata=xsell_valid, type="response", na.action=na.pass)


#### now lets calculate the forecast and check the AUC of both versions
xsell_valid$pred_rf <- predict(rf, newdata=xsell_valid,type="response", na.action=na.pass)

summary(xsell_valid$pred_rf)
summary(xsell_valid$pred_rfopt)

library(pROC) 
### Compare models with ROC  Doing this, pls rund the "normal" RF model form session 6 and compare ####
roc_rf <- roc(xsell_valid$xsell,xsell_valid$pred_rf, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)
roc_rfopt <- roc(xsell_valid$xsell,xsell_valid$pred_rfopt, percent=TRUE, plot=TRUE, print.auc=TRUE,grid=TRUE)

plot(roc_rf,col="red",print.auc=TRUE)
plot(roc_rfopt,col="blue",print.auc=TRUE,add=TRUE)



