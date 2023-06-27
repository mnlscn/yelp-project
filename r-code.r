# Libraries
library(visdat)
library(skimr)
library(DataExplorer)
library(corrplot)
library(bestglm)

# Loading the dataset
df<-read.csv("/Users/manuelscionti/Desktop/Yelp/df_complete.csv")


######### EXPLORATIVE ANALYSIS #############

dim(df) #dimension df
sum(!complete.cases(df)) #number of NaN values (0)
names(df) #names columns
str(df) #statistical info, variable type...
summary(df)


#----------- VARIABLE ANALYSIS -------------
#Let's drop some not useful variables for our ML analysis
working_df = df #create a new df for backup
working_df <- subset(working_df, select = -c(X
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
                                             ,weekdays) )


# 1) Cast all the variables with their right datatype
#working_df$ch_in <- factor(working_df$ch_in, levels = c(1, 0), labels = c(1, 0))
working_df$business_park <- factor(working_df$business_park, levels = c('TRUE', 'FALSE'), labels = c(1, 0))
working_df$business_price <- factor(working_df$business_price)
#working_df$business_open <- factor(working_df$business_open, levels = c(1, 0), labels = c(1, 0))
working_df$business_cat <- factor(working_df$business_cat)
working_df$Quarter <- factor(working_df$Quarter)
#working_df$weekend <- factor(working_df$weekend)
# working_df$wifi_dummy <- factor(working_df$wifi_dummy)
# working_df$tv_dummy <- factor(working_df$tv_dummy)
# working_df$bikeparking_dummy <- factor(working_df$bikeparking_dummy)
# working_df$goodforgroups_dummy <- factor(working_df$goodforgroups_dummy)
# working_df$outdoorseating_dummy <- factor(working_df$outdoorseating_dummy)
# working_df$creditcardpayment_dummy <- factor(working_df$creditcardpayment_dummy)
# working_df$alcohol_dummy <- factor(working_df$alcohol_dummy)
working_df$noise_level <- factor(working_df$noise_level)


###### DUMMY TRANSFORMATION ######
library(dplyr)

# Specify the categorical variables with multiple levels
categorical_vars <- c("business_price","business_cat","noise_level","Quarter")

# Create dummy variables for the specified categorical variables
working_df_dummies <- data.frame(model.matrix(~ . - 1, data = working_df[, categorical_vars]))

# Remove the original categorical variables
working_df <- working_df[, !names(working_df) %in% categorical_vars]


# Add the dummy variables to the original dataset
working_df <- cbind(working_df, working_df_dummies)
str(working_df)

# Cast the whole dataset as numeric
working_df <- as.data.frame(lapply(working_df, as.numeric))
str(working_df)

# View the updated data frame with dummy variables
View(working_df)

# 2) Exploratory analysis

#vis_dat(working_df,warn_large_data=F)
#vis_miss(working_df,warn_large_data=F) #NaN check 
#skim(working_df)
#plot_bar(working_df)
#plot_histogram(working_df)




# ----- Machine Learning -------
# 3) Check minority output-class (SMOTE if necessery - check literature which is better)
# Check all the ML best practices 
# Chooses some models
# combine smartly with R code of other course
# si può runnare l'agoritmo su tutte le variabili, poi le dividiamo per gruppi
# per capire quale subset è più importante e magari si prendono quelle più importanti 
# per ogni gruppo (TO CHECK)

# A simple regression analysis ----
m1=glm(ch_in~., data = working_df, family = "binomial")
car::vif(m1)
summary(m1)

# ------------------
set.seed(66)
yelp_data=working_df
varsin=c("ch_in","business_price1","business_price2","business_price3","TOBS","PRCP","business_long","male","female","business_park","business_open","n_photo","review_count.x","weekend","stars","wifi_dummy","tv_dummy","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip","bikeparking_dummy","goodforgroups_dummy","outdoorseating_dummy","noise_level2","noise_level3","business_catAsian","business_catOthers","business_catMexican")
yelp_data=subset(yelp_data,select=varsin)

# Compute the correlation matrix
correlation_matrix <- cor(yelp_data)

# Create the correlation plot
library(ggcorrplot)
ggcorrplot(correlation_matrix, 
           hc.order = TRUE, 
           type = "upper", 
           lab = FALSE, 
           lab_size = 3.5, 
           tl.cex = 8, 
           tl.col = "black",
           method = "square", 
           colors = c("blue", "white", "red"),
           title = "Correlation Plot")


# Splitting in training, validation, test
spec = c(train = .6, test = .2, validation = .2)

g = sample(cut(
  seq(nrow(yelp_data)), 
  nrow(yelp_data)*cumsum(c(0,spec)),
  labels = names(spec)))
res = split(yelp_data, g)

# Our 3 working dataset for the machine learning part
yelp.train = res$train
yelp.validation = res$validation
yelp.test = res$test

BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))

# class imbalance check.
temp=table(yelp.train[,"ch_in"])
print(temp)

temp=table(yelp.validation[,"ch_in"])
print(temp)

temp=table(yelp.test[,"ch_in"])
print(temp)

# ----- Addressing class imbalance ------
############## SMOTE ##############


# The SMOTE (Synthetic Minority Over-sampling Technique) technique is typically
# applied on the training set only,
# before any further splitting of the data.
# The reason for this is to avoid introducing any synthetic or artificial samples
# into the validation or test sets, which could potentially bias the evaluation
# of the model's performance.

# # Install and load the DMwR2 package
# install.packages("DMwR2")
# library(DMwR2)
# install.packages("smotefamily")
# library(smotefamily)
# 
# 
# # Apply SMOTE to oversample the minority class (label = "1")
# train_set_smote <- SMOTE("ch_in"~., yelp.train)
# yelp.train_smote <- smotefamily::smote(yelp.train[, -ncol(yelp.train)], yelp.train$ch_in)
# 
# 
# # Check the class distribution after applying SMOTE
# table(train_set_smote$label)
# 
# class_distribution <- prop.table(table(yelp.train_smote$ch_in)) * 100
# class_distribution
# 
# 
# if(1){
#   yelp.train_smote=SMOTE(yelp.train[,-c(1,2)],yelp.train[,2])$data
#   names(yelp.train_smote)[ncol(yelp.train_smote)]="ch_in_string"
#   x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
#   x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
#   x.traindum=x.traindum_smote
#   rm(x.traindum_smote)
# }
# temp=table(x.traindum[,"ch_in_string"])
# print(temp)

######### NORMALIZATION ########

# Identify the variables to normalize
variables_to_normalize <- c("cum_n_tips", "TOBS", "PRCP", "business_long", "male", "female", "n_photo", "review_count.x", "stars",
                            "cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip")

# Create a subset of the dataset with the selected variables
subset_data <- yelp.train[, variables_to_normalize]

# Normalize the selected variables
normalized_data <- as.data.frame(scale(subset_data))

# Replace the normalized columns in the original dataset
yelp.train[, variables_to_normalize] <- normalized_data

# View the normalized train set
View(yelp.train)


#----------

probthres=mean(yelp.train$ch_in)


# the analyses
######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = yelp.train, family = "binomial") # estimating the probability of "checkin"

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







# 4) Variables importance, after the ML part





