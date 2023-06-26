# Libraries
library(visdat)
library(skimr)
library(DataExplorer)
library(corrplot)

# Loading the dataset
df<-read.csv("/Users/manuelscionti/Desktop/Yelp/df_complete.csv")
View(df)


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
View(working_df) #dropping unseful variables

# 1) Cast all the variables with their right datatype
working_df$ch_in <- factor(working_df$ch_in, levels = c(1, 0), labels = c(1, 0))
working_df$business_park <- factor(working_df$business_park, levels = c('TRUE', 'FALSE'), labels = c(1, 0))
working_df$business_price <- factor(working_df$business_price)
working_df$business_open <- factor(working_df$business_open, levels = c(1, 0), labels = c(1, 0))
working_df$business_cat <- factor(working_df$business_cat)
working_df$Quarter <- factor(working_df$Quarter)
working_df$weekend <- factor(working_df$weekend)
working_df$wifi_dummy <- factor(working_df$wifi_dummy)
working_df$tv_dummy <- factor(working_df$tv_dummy)
working_df$bikeparking_dummy <- factor(working_df$bikeparking_dummy)
working_df$goodforgroups_dummy <- factor(working_df$goodforgroups_dummy)
working_df$outdoorseating_dummy <- factor(working_df$outdoorseating_dummy)
working_df$creditcardpayment_dummy <- factor(working_df$creditcardpayment_dummy)
working_df$noise_level <- factor(working_df$noise_level)
working_df$alcohol_dummy <- factor(working_df$alcohol_dummy)

str(working_df)

# 2) Exploratory analysis

#vis_dat(working_df,warn_large_data=F)
#vis_miss(working_df,warn_large_data=F) #NaN check 
#skim(working_df)
#plot_bar(working_df)
#plot_histogram(working_df)

plot_boxplot(working_df$n_photo, by=working_df$ch_in)


# Correlation study
# ----> Subset numeric variables
correlation_table<-cor(working_df)
corrplot(correlation_table, method="circle", type="upper")






# Feature Engeneering?

# ----- Machine Learning -------
# 3) Check minority output-class (SMOTE if necessery - check literature which is better)
# Check all the ML best practices 
# Chooses some models
# combine smartly with R code of other course
# si può runnare l'agoritmo su tutte le variabili, poi le dividiamo per gruppi
# per capire quale subset è più importante e magari si prendono quelle più importanti 
# per ogni gruppo (TO CHECK)

# 4) Variables importance, after the ML part





