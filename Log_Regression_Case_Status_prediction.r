  #####################################################################################
  #                    MSIS-5223-PROGRAMMING FOR DATA SCIENCE
  #   ANALYSIS AND PREDICTION OF LABOUR CONDITION APPLICATION FOR H1-B IMMIGRATION
  #####################################################################################
  
###Install Library Packages
install.packages('psych')
install.packages('Hmisc')
install.packages("ggplot2")
install.packages("dummies")
install.packages("foreign")
install.packages("splitstackshape")
install.packages("tree")
install.packages("ROCR")

###Use the Libraries imported
library(psych)
library(Hmisc)
library(ggplot2)
library(dummies)
library(foreign)
library(nnet)
library(splitstackshape)
library(rpart)
library(ROCR)

###Check for the presence of files prior to attempting to open it
f1_check=file.exists("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY15.csv")
f2_check=file.exists("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY16.csv")
f3_check=file.exists("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY17.csv")


#### Open the files and data of each financial year to a data frame
if (f1_check == TRUE & f2_check== TRUE & f3_check == TRUE)
{
  h1bfy15=read.csv("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY15.csv", header=T, sep=",")
  h1bfy16=read.csv("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY16.csv", header=T, sep=",")
  h1bfy17=read.csv("/Users/RRC/MSIS5223/H1b_dataset/H-1B_Disclosure_Data_FY17.csv", header=T, sep=",")
  
}else
{
  print("Check the presence of files in the directory")
}

###Consolidate all the data files and append it into single data frame
final_consolidated_H1B = rbind(h1bfy15,h1bfy16,h1bfy17)

###Total Number of Original Consolidated columns and rows 
nrow(final_consolidated_H1B)
ncol(final_consolidated_H1B)
head(h1bfy15$CASE_SUBMITTED)

### Filtering data as per condition Visa_Class=H1B, Wage_unit_pay=year and Wilful_Violator=N 
h1b_analyzed=final_consolidated_H1B[final_consolidated_H1B$VISA_CLASS=='H-1B' & final_consolidated_H1B$WAGE_UNIT_OF_PAY=='Year' & final_consolidated_H1B$WILLFUL.VIOLATOR=='N',]

###Total Number of columns and rows post filtering 
nrow(h1b_analyzed)
ncol(h1b_analyzed)

###Converting the data into required datatypes and also creating derived varables 

h1b_analyzed$CASE_NUMBER=as.character(h1b_analyzed$CASE_NUMBER)
#h1b_analyzed$CASE_SUBMITTED=as.Date(h1b_analyzed$CASE_SUBMITTED)
h1b_analyzed$CASE_SUBMITTED=as.Date(strptime(h1b_analyzed$CASE_SUBMITTED, "%m/%d/%Y"))
h1b_analyzed$DECISION_DATE=as.Date(h1b_analyzed$DECISION_DATE)
h1b_analyzed$EMPLOYMENT_START_DATE <- as.Date(strptime(h1b_analyzed$EMPLOYMENT_START_DATE, "%m/%d/%Y"))
h1b_analyzed$EMPLOYMENT_END_DATE <- as.Date(strptime(h1b_analyzed$EMPLOYMENT_END_DATE , "%m/%d/%Y"))
h1b_analyzed$EMPLOYER_NAME=as.character(h1b_analyzed$EMPLOYER_NAME)
h1b_analyzed$EMPLOYER_ADDRESS=as.character(h1b_analyzed$EMPLOYER_ADDRESS)
h1b_analyzed$EMPLOYER_CITY=as.character(h1b_analyzed$EMPLOYER_CITY)
h1b_analyzed$EMPLOYER_STATE=as.character(h1b_analyzed$EMPLOYER_STATE)
h1b_analyzed$EMPLOYER_CITY=as.character(h1b_analyzed$EMPLOYER_CITY)
h1b_analyzed$EMPLOYER_COUNTRY=as.character(h1b_analyzed$EMPLOYER_COUNTRY)
h1b_analyzed$EMPLOYER_PROVINCE=as.character(h1b_analyzed$EMPLOYER_PROVINCE)
h1b_analyzed$AGENT_ATTORNEY_NAME=as.character(h1b_analyzed$AGENT_ATTORNEY_NAME)
h1b_analyzed$AGENT_ATTORNEY_CITY=as.character(h1b_analyzed$AGENT_ATTORNEY_CITY)
h1b_analyzed$AGENT_ATTORNEY_STATE=as.character(h1b_analyzed$AGENT_ATTORNEY_STATE)
h1b_analyzed$PW_WAGE_SOURCE_OTHER=as.character(h1b_analyzed$PW_WAGE_SOURCE_OTHER)
h1b_analyzed$WAGE_RATE_OF_PAY_TO=as.numeric(as.character(h1b_analyzed$WAGE_RATE_OF_PAY_TO))
h1b_analyzed$WORKSITE_CITY=as.character(h1b_analyzed$WORKSITE_CITY)
h1b_analyzed$WORKSITE_COUNTY=as.character(h1b_analyzed$WORKSITE_COUNTY)
h1b_analyzed$WORKSITE_STATE=as.character(h1b_analyzed$WORKSITE_STATE)
h1b_analyzed$WORKSITE_POSTAL_CODE=as.character(h1b_analyzed$WORKSITE_POSTAL_CODE)


head(h1b_analyzed)
###Creating  the derived column "Days"  as the difference between the employment start and end days
h1b_analyzed$Days <- h1b_analyzed$EMPLOYMENT_END_DATE - h1b_analyzed$EMPLOYMENT_START_DATE
h1b_analyzed$Days <- h1b_analyzed$EMPLOYMENT_END_DATE - h1b_analyzed$EMPLOYMENT_START_DATE

###See the Structure to confirm the changess made
str(h1b_analyzed)
head(h1b_analyzed)

###Export the ORIGINAL DATA FILE TO HAVE IT AS BACKUP IN CASE OF FAILURE
write.csv(h1b_analyzed,file="/Users/RRC/Downloads/H1b_dataset/H-1B_source_file.csv")

str(h1b_analyzed)
###Get the subset of columns without altering original data file for analysis purpose
###h1b_filtered=h1b_analyzed[,c('CASE_NUMBER','CASE_STATUS','CASE_SUBMITTED','EMPLOYMENT_START_DATE','EMPLOYMENT_END_DATE','EMPLOYER_NAME','JOB_TITLE','SOC_CODE','SOC_NAME','FULL_TIME_POSITION','PREVAILING_WAGE','WAGE_RATE_OF_PAY_FROM','WAGE_RATE_OF_PAY_TO','H.1B_DEPENDENT')]

h1b_filtered=h1b_analyzed[,c('CASE_NUMBER','CASE_STATUS','CASE_SUBMITTED','EMPLOYMENT_START_DATE','EMPLOYMENT_END_DATE','EMPLOYER_NAME','EMPLOYER_STATE','JOB_TITLE','SOC_CODE','SOC_NAME','PREVAILING_WAGE','WAGE_RATE_OF_PAY_FROM','WAGE_RATE_OF_PAY_TO','H.1B_DEPENDENT','Days')]
nrow(h1b_filtered)
str(h1b_filtered)

#Before Running the Descriptive Stats Check for the Null Values  for Employer Names
sum(is.na(h1b_filtered$EMPLOYER_NAME))

####################################
######DESCRIPTIVE STATISTICS########
####################################
##1##
###TOP 10 EMPLOYERS FILING LCA PETITIONS###

number_of_applications=as.data.frame(table(h1b_filtered$EMPLOYER_NAME))
colnames(number_of_applications)=c("EMPLOYER_NAME","APPLICATION_COUNT")
number_of_applications$EMPLOYER_NAME = as.character(number_of_applications$EMPLOYER_NAME)
attach(number_of_applications)
top_10_applications=number_of_applications[order(-APPLICATION_COUNT), ]
top_10_applications=top_10_applications[1:10,]
top_10_applications$EMPLOYER_NAME=factor(top_10_applications$EMPLOYER_NAME, levels = top_10_applications$EMPLOYER_NAME[order(top_10_applications$APPLICATION_COUNT)])

plot=ggplot(top_10_applications, aes(x=EMPLOYER_NAME,y=APPLICATION_COUNT)) + geom_bar(stat='identity', aes(fill = APPLICATION_COUNT)) + theme_bw() + ggtitle("TOP 10 EMPLOYERS FILING LCA PETITIONS") + theme (plot.title = element_text(hjust = 0.5))
plot+coord_flip()

##2##
###TOP 10 EMPLOYERS - CASE STATUS###

case_status_count=as.data.frame(table(h1b_filtered$EMPLOYER_NAME,h1b_filtered$CASE_STATUS))
colnames(case_status_count)=c("EMPLOYER_NAME","CASE_STATUS","CASE_COUNT")
attach(top_10_applications)
comparison_by_status=merge(top_10_applications, case_status_count, by = "EMPLOYER_NAME")

###Generate the Line Plot using above table###
ggplot(data=comparison_by_status, aes(x=CASE_STATUS, y=CASE_COUNT, group=EMPLOYER_NAME)) +
geom_line(aes(color=EMPLOYER_NAME))+
geom_point(aes(color=EMPLOYER_NAME)) + ggtitle("CASE STATUS OF TOP 10 EMPLOYERS") + theme (plot.title = element_text(hjust = 0.5))


##3##
###TOP 10 JOBS IN THE MARKET###
top_10_jobs=as.data.frame(table(h1b_filtered$JOB_TITLE))
colnames(top_10_jobs)=c("JOB_TITLE","JOB_APPLICATION_COUNT")
top_10_jobs$JOB_TITLE = as.character(top_10_jobs$JOB_TITLE)
attach(top_10_jobs)
top_10_jobs=top_10_jobs[order(-JOB_APPLICATION_COUNT), ]
top_10_jobs=top_10_jobs[1:10,]
top_10_jobs$JOB_TITLE=factor(top_10_jobs$JOB_TITLE, levels = top_10_jobs$JOB_TITLE[order(top_10_jobs$JOB_APPLICATION_COUNT)])

plot=ggplot(top_10_jobs, aes(x=JOB_TITLE,y=JOB_APPLICATION_COUNT)) + geom_bar(stat='identity', aes(fill = JOB_APPLICATION_COUNT)) + theme_bw() + ggtitle("TOP 10 JOBS FILED FOR LCA PETITIONS") + theme (plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
plot+coord_flip()


#############################################
#DATA CLEANSING
############################################

####Remove 0's and Spaces and then coerce them to 'NA' as they are undefined 

h1b_filtered$PREVAILING_WAGE[h1b_filtered$PREVAILING_WAGE == '' | h1b_filtered$PREVAILING_WAGE==' '] <- NA
h1b_filtered$WAGE_RATE_OF_PAY_FROM[h1b_filtered$WAGE_RATE_OF_PAY_FROM== '' | h1b_filtered$WAGE_RATE_OF_PAY_FROM==' '] <- NA
h1b_filtered$WAGE_RATE_OF_PAY_TO[h1b_filtered$WAGE_RATE_OF_PAY_TO== '' | h1b_filtered$WAGE_RATE_OF_PAY_TO==' '|h1b_filtered$WAGE_RATE_OF_PAY_TO==0] <- NA


### Ater coercing above values to NA's, remove those values from dataset as they are undefined
##Omit NA
h1b_filtered=na.omit(h1b_filtered)

##Check for the row and column count
nrow(h1b_filtered)
ncol(h1b_filtered)

###Additional Filtering of the Dataset to eliminate wrong or invalid data entries
h1b_filtered=h1b_filtered[h1b_filtered$PREVAILING_WAGE>10000,]
nrow(h1b_filtered)
#  this elimiated around 90 records

h1b_filtered=h1b_filtered[h1b_filtered$WAGE_RATE_OF_PAY_FROM>10000,]
nrow(h1b_filtered)
# eliminates just 1 record 

tail(h1b_filtered)
h1b_filtered=h1b_filtered[h1b_filtered$WAGE_RATE_OF_PAY_TO>10000,]
nrow(h1b_filtered)
#No Records have been eliminated[Thats really good!]

nrow(h1b_filtered)
h1b_filtered=h1b_filtered[h1b_filtered$PREVAILING_WAGE<250000,]
nrow(h1b_filtered)
#this eliminated 34 records

h1b_filtered=h1b_filtered[h1b_filtered$WAGE_RATE_OF_PAY_FROM<250000,]
nrow(h1b_filtered)
# eliminates just 354 records


h1b_filtered=h1b_filtered[h1b_filtered$WAGE_RATE_OF_PAY_TO<250000,]
nrow(h1b_filtered)
# eliminates 1049 records 


####Now to have a balanced data for our Analysis,
####lets do the sampling on each case status 

cert=h1b_filtered[h1b_filtered$CASE_STATUS=="CERTIFIED",]
nrow(cert)
den=h1b_filtered[h1b_filtered$CASE_STATUS=="DENIED",]
nrow(den)
with=h1b_filtered[h1b_filtered$CASE_STATUS=="WITHDRAWN",]
nrow(with)
certwith=h1b_filtered[h1b_filtered$CASE_STATUS=="CERTIFIED-WITHDRAWN",]
nrow(certwith)

###Sampling the CASE-STATYS-CERTIFIED to get the balance between the case status

###CASE-STATUS = CERTIFIED
set.seed(1)
index_sample_20=sample(1:nrow(cert),0.20*nrow(cert))
sample_20=cert[index_sample_20,]
nrow(sample_20)
str(sample_20)
head(sample_20)


###Conolidate the balanced data into single data frame[ The data would be close to 1L]
sample_consol=rbind(sample_20,den,with,certwith)
str(sample_consol)
nrow(sample_consol)
ncol(sample_consol)
head(sample_consol)

###Convert the number of days into numeric value (ex : 1095 days to 1095)
sample_consol$Days=as.numeric(sample_consol$Days)


#Write The final sampled file into CSV
write.csv(sample_consol,file="/Users/RRC/Downloads/H1b_dataset/H1-B_Sample_data.csv")

### OUTLIERS AND NORMALITY ANALYSIS - SAMPLE DATA SET TAKEN FOR ANALYSIS
par(mfrow=c(1,1))
boxplot(h1b_analyzed$PREVAILING_WAGE,xlab="Prevailing Wage",main="Dataset without outlier assessment")
boxplot(sample_consol$PREVAILING_WAGE,xlab="Prevailing Wage",main="Dataset outlier assessed & accepted")
hist(sample_consol$PREVAILING_WAGE,xlab="Prevailing Wage",main="Distribution after removing invalid data")

par(mfrow=c(1,2))
boxplot(h1b_analyzed$WAGE_RATE_OF_PAY_FROM,xlab="WAGE_RATE_OF_PAY_FROM",main="Dataset without outlier assessment")
boxplot(sample_consol$WAGE_RATE_OF_PAY_FROM,xlab="WAGE_RATE_OF_PAY_FROM",main="Dataset outlier assessed & accepted")
hist(sample_consol$WAGE_RATE_OF_PAY_FROM,xlab="WAGE_RATE_OF_PAY_FROM",main="Distribution after removing invalid data")

par(mfrow=c(1,1))
boxplot(h1b_analyzed$WAGE_RATE_OF_PAY_TO,xlab="WAGE_RATE_OF_PAY_TO",main="Dataset without outlier assessment")
boxplot(sample_consol$WAGE_RATE_OF_PAY_TO,xlab="WAGE_RATE_OF_PAY_TO",main="Dataset outlier assessed & accepted")
hist(sample_consol$WAGE_RATE_OF_PAY_TO,xlab="WAGE_RATE_OF_PAY_TO",main="Distribution after removing invalid data")

# Descriptive Statistics : Mean, median etc ...
summary(sample_consol)



##Identify the numeric variables that can be used as predictor variables

corr_columns=c("PREVAILING_WAGE", "WAGE_RATE_OF_PAY_FROM", "WAGE_RATE_OF_PAY_TO", "Days")
corr_subset <- sample_consol[corr_columns]
cor(corr_subset)


#Create Dummy Variables for H1B Dependent
unique(sample_consol$H.1B_DEPENDENT)
h1_b_dep = dummy(sample_consol$H.1B_DEPENDENT, sep = '_')
colnames(h1_b_dep)
h1_b_dep = as.data.frame(h1_b_dep)
sample_consol = data.frame(sample_consol, h1_b_dep)
names(sample_consol)


###Perform Logistic Regression using the above variables but only for CASE STATUS 'CERTIFIED' AND 'DENIED'

###Consolidate the data for Certified and Denied
data_consolidated=rbind(cert,den)

###Convert the number of days into numeric value (ex : 1095 days to 1095)
data_consolidated$Days=as.numeric(data_consolidated$Days)

###Create Dummy Variables for H1B Dependent
unique(data_consolidated$H.1B_DEPENDENT)
h1_b_dep = dummy(data_consolidated$H.1B_DEPENDENT, sep = '_')
colnames(h1_b_dep)
h1_b_dep = as.data.frame(h1_b_dep)
data_consolidated = data.frame(data_consolidated, h1_b_dep)
names(data_consolidated)


###Obtian the test data from the consolidated data using Stratified Sampling 
###to avoid biased data


##############################################################################
#####OBTAIN THE TRAIN DATA AND THEN PERFORM THE LOGISTIC REGRESSION
#####USE STRATIFIED SAMPLING FOR UNBIASED DATA
##############################################################################

###Stratified Data for Train Sampling 
set.seed(1)
train_data <- stratified(data_consolidated, c("CASE_STATUS"),replace = FALSE, 4000)
train_data$CASE_STATUS <- factor(train_data$CASE_STATUS)
unique(train_data$CASE_STATUS)
head(train_data)
nrow(train_data)

###################################################
####TRAIN_DATA STRATIFIED SAMPLING RESULT
#################################################@
##TOTAL ROWS
nrow(train_data)
##CERTIFIED
nrow(train_data[train_data$CASE_STATUS == "CERTIFIED",])
##DENIED
nrow(train_data[train_data$CASE_STATUS == "DENIED",])


#####################################################################################
#             MODEL - 1a       LOGISTIC REGRESSION - CASE-STATUS PREDICTION - TRAIN DATA
#####################################################################################
case_status_prediction_train=glm(CASE_STATUS~PREVAILING_WAGE + Days + H.1B_DEPENDENT_Y ,family=binomial(link='logit'), data = train_data)
summary(case_status_prediction_train)



##############################################################################
#####OBTAIN THE TEST DATA AND THEN PERFORM THE LOGISTIC REGRESSION
#####USE STRATIFIED SAMPLING FOR UNBIASED DATA
##############################################################################

###Stratified Data for Test Sampling 
set.seed(1)
test_data <- stratified(data_consolidated, c("CASE_STATUS"),replace = FALSE, 1000)
test_data$CASE_STATUS <- factor(test_data$CASE_STATUS)
head(test_data)
nrow(test_data)

###################################################
####TEST_DATA STRATIFIED SAMPLING RESULT
#################################################@
##TOTAL ROWS
nrow(test_data)
##CERTIFIED
nrow(test_data[test_data$CASE_STATUS == "CERTIFIED",])
##DENIED
nrow(test_data[test_data$CASE_STATUS == "DENIED",])


#####################################################################################
#             MODEL - 1b       LOGISTIC REGRESSION - CASE-STATUS PREDICTION - TEST DATA
#####################################################################################

###TEST - DATA
###LOGISTIC REGRESSION PREDCITING THE CASE STATUS
case_status_prediction_test=glm(CASE_STATUS~PREVAILING_WAGE + Days + H.1B_DEPENDENT_Y ,family=binomial(link='logit'), data = test_data)
summary(case_status_prediction_test)

###After Building The model with train Data Check it with Test Data for Accuracy
predict_logit=predict(case_status_prediction_train,newdata = test_data,type='response')
summary(predict_logit)

#Cut-off value = 0.5
pred_cut_off <- ifelse(predict_logit > 0.5, 'CERTIFIED','DENIED') #Setting cut-off t  o be at 0.5
table(test_data$CASE_STATUS,pred_cut_off)

pred <- prediction(pred_cut_off,test_data$CASE_STATUS)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#Printing AUC Value
perf1 <- performance(pred, "auc")
print(perf1.values[[1]])

install.packages("ROC")
library(ROCR)
#Plotting the ROC-curve
roc.curve(test_data$CASE_STATUS, pred_cut_off,col="red", main="The ROC-curve for Model with cut-off=0.5")
text(0.6,0.2,paste("AUC=0.52"))
confusionMatrix(pred, test_data$CASE_STATUS)
unique(Lend_pred)


################################################################################################
###                          MODEL - 2a  - CLASSICFICATION TREE - CASE STATUS PRODUCTION - TRAIN DATA
###############################################################################################

classifier=rpart(formula=CASE_STATUS ~ PREVAILING_WAGE + Days + H.1B_DEPENDENT_Y, data=train_data, method="class")
plot(classifier, uniform=TRUE, branch=0.6, margin=0.05)
text(classifier, all=TRUE, use.n=TRUE)
title("CLASSIFICATION TREE - CASE STATUS PREDICTION")

############################################################################################################################
###                          MODEL - 2b  - CLASSICFICATION TREE - CASE STATUS PRODUCTION USING CONFUSION MATRIX - TEST DATA
############################################################################################################################

###CONFUSION MATRIX FOR THE TEST DATA AND SEE THE ACCURACY
case_status_check <- predict(classifier, test_data, type="class")
table(test_data$CASE_STATUS, case_status_check)

