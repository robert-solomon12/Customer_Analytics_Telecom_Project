# TELECOM_CUSTOMER_ANALYTICS Master Project (Data Management - EDA) by Robert Solomon


#######################################################################################################################################################################################
############################                                                                                                                               ############################
#################################################            Installing and loading required Libraries          #######################################################################
############################                                                                                                                               ############################        
#######################################################################################################################################################################################


# Uncomment and run following codes to install required packages:
# install.packages("reshape2")
# install.packages("caret")
# install.packages("car")
# install.packages("plyr")
# install.packages("openxlsx")
# install.packages("sqldf")
# install.packages("ROCR")
# install.packages("e1071")
# install.packages("pROC")
# install.packages("rpart")
# install.packages("randomForest")

# Loading required package libraries:
library(reshape2)
library(caret)
library(car)
library(plyr)
library(openxlsx)
library(sqldf)
library(e1071)
library(ROCR)
library(pROC)
library(rpart)
library(randomForest)

# Setting a seed for reproducibility
set.seed(123)


#######################################################################################################################################################################################
############################                                                                                                                               ############################
#################################################           Background & Objective           ##########################################################################################
############################                                                                                                                               ############################        
#######################################################################################################################################################################################

# The background of this Dataset is to analyzing telecom customer churn data is crucial for providers to understand factors influencing customer departure. 
# This data helps identify patterns in service quality, pricing, and customer satisfaction, allowing companies to proactively address issues, improve customer retention strategies, and enhance overall service delivery. 
# Data analysis in this context enables telecom companies to stay competitive by adapting to evolving customer preferences and mitigating potential churn risks.


# The objective are the following:
#
# 1. To analyze "Customer Churn" and understand the factors associated with it
# 2. Develop Churn Prediction Model 
# 3. Implement Machine Learning Algorithms and select the best method for Churn Prediction


#############################################################################################################################################################
############################                                                                                                     ############################
##################################################################         Phase 1         ##################################################################
############################                                                                                                     ############################        
#############################################################################################################################################################


# Arising Questions:
# Q1.	Understand all 4 data sets:  
#   (understand variables, data dimension, Check duplicate records if any etc.)
# Q2.	Merge 4 data sets and create a master data
# Q3.	Perform data cleaning wherever necessary
# Q4.	Check if there are outliers present in the data
# Q5.	Check each numerical variable for possible recoding into categorical variable
# Q6.	 Find overall “CHURN RATE” in the data



# Importing the required datasets individually:

customerDem_data<-read.csv("PGD - DataScience (Level 9 2023)//dsi-woolf-msc//modules//Module_10_Data_Science_In_Practice_Project//Assignments//PROJECT_TELECOM_CUSTOMER_ANALYTICS//DATASETS//Customer_Demographics.csv",header=T)
paymentDetails_data<-read.csv("PGD - DataScience (Level 9 2023)//dsi-woolf-msc//modules//Module_10_Data_Science_In_Practice_Project//Assignments//PROJECT_TELECOM_CUSTOMER_ANALYTICS//DATASETS//Payment_Details.csv",header=T)
serviceAvailed_data<-read.csv("PGD - DataScience (Level 9 2023)//dsi-woolf-msc//modules//Module_10_Data_Science_In_Practice_Project//Assignments//PROJECT_TELECOM_CUSTOMER_ANALYTICS//DATASETS//Services_Availed.csv",header=T)
churn_data<-read.csv("PGD - DataScience (Level 9 2023)//dsi-woolf-msc//modules//Module_10_Data_Science_In_Practice_Project//Assignments//PROJECT_TELECOM_CUSTOMER_ANALYTICS//DATASETS//Churn_Status.csv",header=T)



# 1. Exploring and understanding them (e.g. data dimension, Check duplicate records if any etc.)

# a) Customer Demographics:
head(customerDem_data)
data1 = customerDem_data

# b) Payment Details:
head(paymentDetails_data)
data2 = paymentDetails_data

# c) Availed Services: 
head(serviceAvailed_data)
data3 = serviceAvailed_data

# d) Churned Customers:
head(churn_data)
data4 = churn_data




# Constructing the dimensions for all datasets:
customerDemographicsDim <- dim(customerDem_data)
paymentDetailsDim <- dim(paymentDetails_data)
serviceAvailedDim <- dim(serviceAvailed_data)
churnDim <- dim(churn_data)


# Printing out the dimensions (number of rows and columns) of all Datasets (Payment Details):

# a)
cat("Number of rows in Customer Demographics Dataset:", customerDemographicsDim[1], "\n")
cat("Number of columns in Customer Demographics Dataset:", customerDemographicsDim[2], "\n")

# b)
cat("Number of rows in Payment Details Dataset:", paymentDetailsDim[1], "\n")
cat("Number of columns in Payment Details Dataset:", paymentDetailsDim[2], "\n")

# c)
cat("Number of rows in Service Availed Dataset:", serviceAvailedDim[1], "\n")
cat("Number of columns in Service Availed Dataset:", serviceAvailedDim[2], "\n")

# d)
cat("Number of rows in Churned Dataset:", churnDim[1], "\n")
cat("Number of columns in Churned Dataset:", churnDim[2], "\n")





#  6 variables are present in the "Customer_Demographics" dataset:
# 1. CID (nominal scale, alpha-numeric, 211290 unique values, no duplicate values) - leave as it is
# 2. gender (categorical, nominal scale, alphabetic: 'Male/Female') - needs to be converted to numeric: 0/1
# 3. SeniorCitizen (categorical, nominal scale, numeric: 0/1) - leave as it is
# 4. Partner (categorical, nominal scale, alphabetic: 'No/Yes') - needs to be converted  to numeric: 0/1
# 5. Dependents (categorical, nominal scale, alphabetic: 'No/Yes') - needs to be converted to numeric: 0/1
# 6. tenure (Ratio scale, numeric, integer, Min 0 Max 72) - leave as it is, or convert into the ordinal scale (ranking)"

# Checking for duplicate records in Customer Demographics Dataset:
duplicate_rec_customerDem_data <- customerDem_data[duplicated(customerDem_data), ]

# Displaying the duplicate records (if present in Customer Demographics Dataset) via 'if' condition:
if (nrow(duplicate_rec_customerDem_data) > 0) {
  cat("Duplicate records found in Customer Demographics Dataset:\n")
  print(duplicate_rec_customerDem_data)
} else {
  cat("No duplicate records found in Customer Demographics Dataset.\n")
}



#  5 variables are present in the "Payment_Details" dataset:
# 1. CID (nominal scale, alpha-numeric, 211290 unique values, no duplicate values) - leave as it is
# 2. Contract (categorical, nominal scale, alphabetic: 'Month-to-month/One year/Two year') - needs to be converted to dummy variables
# 3. PaperlessBilling (categorical, nominal scale, alphabetic: 'No/Yes') - needs to be converted to numeric: 0/1
# 4. PaymentMethod (categorical, nominal scale, alphabetic: 'Mailed check/Credit card (automatic)/Bank transfer (automatic)/Electronic check/Mailed check') - needs to be converted to dummy variables
# 5. MonthlyCharges (Ratio scale, numeric, integer, Min 20.44 Max 120.94) - leave as it is, or convert into the ordinal scale (ranking)"

# Checking for duplicate records in Payment Details Dataset:
duplicate_rec_paymentDetails_data <- paymentDetails_data[duplicated(paymentDetails_data), ]

# Displaying the duplicate records (if present in Payment Details) via 'if' condition:
if (nrow(duplicate_rec_paymentDetails_data) > 0) {
  cat("Duplicate records found in Payment Details Dataset:\n")
  print(duplicate_rec_paymentDetails_data)
} else {
  cat("No duplicate records found in Payment Details Dataset.\n")
}



#  12 variables are present in the "Services_Availed" dataset:  
#     1. CID (nominal scale, alpha-numeric, 211290 unique values, no duplicate values) - variable can be left unchanged, can't be converted or used a dummy variable
#     2. PhoneService (categorical, nominal scale, alphabetic: 'No/Yes') - needs to be converted to numeric: 0/1
#     3. MultipleLines (categorical, nominal scale, alphabetic: 'No/Yes/No phone service') - needs to be converted to dummy variables
#     4. InternetService (categorical, nominal scale, alphabetic: 'DSL/Fiber optic/No') - needs to be converted to dummy variables
#     5. OnlineSecurity (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     6. OnlineBackup (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     7. DeviceProtection (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     8. TechSupport (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     9. StreamingTV (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     10. StreamingMovies (categorical, nominal scale, alphabetic: 'No/Yes/No internet service') - needs to be converted to dummy variables
#     11. numAdminTickets (Ratio scale, numeric, integer, Min 0 Max 5) - variable can be left unchanged or convert into the ordinal scale (ranking)
#     12. numTechTickets (Ratio scale, numeric, integer, Min 0 Max 9) - variable can be left unchanged or convert into the ordinal scale (ranking)" 

# Checking for duplicate records in Availed Services Dataset:
duplicate_rec_serviceAvailed_data <- serviceAvailed_data[duplicated(serviceAvailed_data), ]

# Displaying the duplicate records (if present) via 'if' condition:
if (nrow(duplicate_rec_serviceAvailed_data) > 0) {
  cat("Duplicate records found:\n")
  print(duplicate_rec_serviceAvailed_data)
} else {
  cat("No duplicate records found.\n")
}




#  2 variables are present in the "Churn_Status" dataset:  
#     1. CID (nominal scale, alpha-numeric, 211290 unique values, no duplicate values) - variable can be left unchanged, can't be converted or used a dummy variable
#     2. Churn (categorical, nominal scale, alphabetic: 'No/Yes') - needs to be converted to numeric: 0/1

# Checking for duplicate records in  Churned Dataset:
duplicate_rec_churn_data <- churn_data[duplicated(churn_data), ]

# Displaying the duplicate records (if present) via 'if' condition:
if (nrow(duplicate_rec_churn_data ) > 0) {
  cat("Duplicate records found in  Churned Dataset:\n")
  print(duplicate_rec_churn_data )
} else {
  cat("No duplicate records found in  Churned Dataset.\n")
}



# ================================================================================================================================================================================================#
# ================================================================================================================================================================================================#
# ================================================================================================================================================================================================#
# ================================================================================================================================================================================================#




# 2. Merging all 4 individual Datasets with an outer join by 'CID'. and creating a master dataframe:
master_df <- merge(merge(merge(data1, data2, by = "CID", all = TRUE),
                         data3, by = "CID", all = TRUE),
                   data4, by = "CID", all = TRUE)


# Printing the merged dataset (master_df)
print(master_df)


# Specifying the file name to save the Excel file:
excel_file <- "Customer_Analytics_Telecom_Master.xlsx"


# Writing the data frame to an Excel file:
write.xlsx(master_df, file = excel_file,
           sheetName = "Sheet1", rowNames = TRUE)

# Validating the merged file creation:
cat("Excel file has been created at", excel_file, "\n")


# 3.	Performing data cleaning wherever necessary:

# Removing leading and trailing whitespaces from all character columns
master_df[, sapply(master_df, is.character)] <- lapply(master_df[, sapply(master_df, is.character)], function(x) trimws(x))

# Converting 'SeniorCitizen' to a factor
master_df$SeniorCitizen <- factor(master_df$SeniorCitizen)

# Converting 'tenure' to numeric
master_df$tenure <- as.numeric(master_df$tenure)

# Converting 'numAdminTickets' and 'numTechTickets' to numeric
master_df$numAdminTickets <- as.numeric(master_df$numAdminTickets)
master_df$numTechTickets <- as.numeric(master_df$numTechTickets)

# Checking for missing values
missing_values <- sum(is.na(master_df))
cat("Number of missing values in the dataset:", missing_values, "\n")

# Removing any rows with missing values:
master_df <- master_df[complete.cases(master_df), ]

# Checking for duplicate rows
duplicate_rows <- sum(duplicated(master_df))
cat("Number of duplicate rows in the dataset:", duplicate_rows, "\n")

# Printing a summary of the dataset
summary(master_df)



# 4. Checking for outliers in the present in the data:

# Selecting relevant numerical columns
numeric_columns <- c('tenure', 'MonthlyCharges', 'numAdminTickets', 'numTechTickets')

# Creating box plots for each numerical column
par(mfrow = c(2, 2))  # Set up a 2x2 grid for plots
for (col in numeric_columns) {
  boxplot(master_df[[col]], main = paste("Boxplot of", col), col = "skyblue", border = "black")
}

# Reset the plotting layout
par(mfrow = c(1, 1))



# 5.	Check each numerical variable for possible recoding into categorical variables:

# Selecting relevant numerical columns
numeric_columns <- c('tenure', 'MonthlyCharges', 'numAdminTickets', 'numTechTickets')

# Explore the distribution of numerical variables
summary(master_df[, numeric_columns])

# Function to create categorical variables based on quantiles or custom criteria
create_categorical <- function(variable, categories) {
  cut(master_df[[variable]], breaks = categories, labels = paste(variable, categories[-length(categories)], sep = "_"), include.lowest = TRUE)
}

# Recoding 'tenure' into categories
tenure_categories <- c(0, 12, 24, 36, 48, 60, Inf)
master_df$tenure_category <- create_categorical('tenure', tenure_categories)

# Recoding 'MonthlyCharges' into categories
monthly_charges_categories <- c(0, 30, 60, 90, 120, Inf)
master_df$monthly_charges_category <- create_categorical('MonthlyCharges', monthly_charges_categories)

# Recoding 'numAdminTickets' into categories
admin_tickets_categories <- c(0, 1, 2, 3, Inf)
master_df$num_admin_tickets_category <- create_categorical('numAdminTickets', admin_tickets_categories)

# Recoding 'numTechTickets' into categories
tech_tickets_categories <- c(0, 1, 2, 3, Inf)
master_df$num_tech_tickets_category <- create_categorical('numTechTickets', tech_tickets_categories)

# Display the first few rows of the modified dataset
head(master_df)


print(master_df)


# 6.	 Finding overall “CHURN RATE” in the data:

# Calculating overall churn rate by taking the mean of a logical vector indicating whether each customer has churned:
overall_churn_rate <- mean(master_df$Churn == "Yes")
# Displaying the result of overall churn rate (26.54%)
cat("Overall Churn Rate:", round(overall_churn_rate * 100, 2), "%\n")



str(master_df)


######################################
#                                 ####
#                                 ####
# Further data cleaning carried out:##
#                                 ####
#                                 ####
######################################


# Checking for missing values
sum(is.na(master_df$gender))
sum(is.na(master_df$SeniorCitizen))
sum(is.na(master_df$Partner))
sum(is.na(master_df$Dependents))
sum(is.na(master_df$tenure))
sum(is.na(master_df$Contract))
sum(is.na(master_df$PaperlessBilling))
sum(is.na(master_df$PaymentMethod))
sum(is.na(master_df$MonthlyCharges))
sum(is.na(master_df$PhoneService))

sum(is.na(master_df$MultipleLines))
sum(is.na(master_df$InternetService))
sum(is.na(master_df$OnlineSecurity))
sum(is.na(master_df$OnlineBackup))
sum(is.na(master_df$DeviceProtection))
sum(is.na(master_df$TechSupport))
sum(is.na(master_df$StreamingTV))
sum(is.na(master_df$StreamingMovies))
sum(is.na(master_df$numAdminTickets))
sum(is.na(master_df$numTechTickets))

sum(is.na(master_df$Churn))


# Checking min/max of Churn
min(master_df$Churn); max(master_df$Churn)

# Converting non-numeric values to binary
master_df$gender <- as.factor(master_df$gender)
master_df$SeniorCitizen <- as.factor(mapvalues(master_df$SeniorCitizen, from=c("0","1"), to=c("No","Yes")))
master_df$OnlineSecurity <- as.factor(mapvalues(master_df$OnlineSecurity, from=c("No internet service"), to=c("No")))
master_df$OnlineBackup <- as.factor(mapvalues(master_df$OnlineBackup, from=c("No internet service"), to=c("No")))
master_df$StreamingTV <- as.factor(mapvalues(master_df$StreamingTV, from=c("No internet service"), to=c("No")))
master_df$StreamingMovies <- as.factor(mapvalues(master_df$StreamingMovies, from=c("No internet service"), to=c("No")))
master_df$DeviceProtection <- as.factor(mapvalues(master_df$DeviceProtection, from=c("No internet service"), to=c("No")))
master_df$TechSupport <- as.factor(mapvalues(master_df$TechSupport, from=c("No internet service"), to=c("No")))
master_df$MultipleLines <- as.factor(mapvalues(master_df$MultipleLines, from=c("No phone service"), to=c("No")))
master_df$Partner <- as.factor(master_df$Partner)
master_df$Dependents <- as.factor(master_df$Dependents)
master_df$Contract <- as.factor(master_df$Contract)
master_df$PaperlessBilling <- as.factor(master_df$PaperlessBilling)
master_df$PaymentMethod <- as.factor(master_df$PaymentMethod)
master_df$PhoneService <- as.factor(master_df$PhoneService)
master_df$InternetService <- as.factor(master_df$InternetService)
master_df$DeviceProtection <- as.factor(master_df$DeviceProtection)
master_df$Churn<-ifelse(master_df$Churn=="No",0,1)





#############################################################################################################################################################
############################                                                                                                     ############################
##################################################################         Phase 2         ##################################################################
############################                                                                                                     ############################        
#############################################################################################################################################################
#
# Arising Questions:

# Q1.	Generate tables/graphs for customer profiling (Example: Count and percentage table for Gender, Senior Citizen, partner etc.)
# Q2.	Analyse churn rate (% of Churn=”Yes”) for different segments in the data
#_____________________________________________________
#|Gender |	N	| N_Churn	 | Churn Rate=(N_Churn/N)*100 |
#|_______|____|__________|____________________________|
#|Male   |		|          |                            |
#|_______|____|__________|____________________________|
#|Female |	  |	         |                            |
#|_______|____|__________|____________________________|
# 
# Q3.	For numeric variables, box-whisker plot by Churn Status can be useful
# 
# The objective of phase 2 is to relate Churn Status with other variables in the data.
# These insights will be useful prior to modeling in which you will take all variables together and model Churn Status in phase 3




# A1. Generate tables/graphs for customer profiling (Example: Count and percentage table for Gender, Senior Citizen, partner etc.)

aggregate_Gender <- sqldf("SELECT gender as category,
                                  COUNT(gender) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(Gender)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_SeniorCitizen <- sqldf("SELECT SeniorCitizen as category,
                                  COUNT(SeniorCitizen) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(SeniorCitizen)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_Tenure <- sqldf("SELECT tenure as category,
                                  COUNT(tenure) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(tenure)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_Partner <- sqldf("SELECT Partner as category,
                                  COUNT(Partner) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(Partner)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_Dependents <- sqldf("SELECT Dependents as category,
                                  COUNT(Dependents) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(Dependents)),2) AS Churn_Percentage
                           FROM master_df")

aggregate_Contract <- sqldf("SELECT Contract as category,
                                  COUNT(Contract) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(Contract)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_PhoneService <- sqldf("SELECT PhoneService as category,
                                  COUNT(PhoneService) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(PhoneService)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_MultipleLines <- sqldf("SELECT MultipleLines as category,
                                  COUNT(MultipleLines) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(MultipleLines)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_InternetService <- sqldf("SELECT InternetService as category,
                                  COUNT(InternetService) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(InternetService)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_OnlineSecurity <- sqldf("SELECT OnlineSecurity as category,
                                  COUNT(OnlineSecurity) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(OnlineSecurity)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_OnlineBackup <- sqldf("SELECT OnlineBackup as category,
                                  COUNT(OnlineBackup) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(OnlineBackup)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_DeviceProtection <- sqldf("SELECT DeviceProtection as category,
                                  COUNT(DeviceProtection) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(DeviceProtection)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_TechSupport <- sqldf("SELECT TechSupport as category,
                                  COUNT(TechSupport) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  (SUM(Churn == '1') * 100.0 / COUNT(TechSupport)) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_StreamingTV <- sqldf("SELECT StreamingTV as category,
                                  COUNT(StreamingTV) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(StreamingTV)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_StreamingMovies <- sqldf("SELECT StreamingMovies as category,
                                  COUNT(StreamingMovies) as Count,
                                  SUM(Churn == '1') as Churn_Yes,
                                  round((SUM(Churn == '1') * 100.0 / COUNT(StreamingMovies)),2) AS Churn_Percentage
                           FROM master_df
                           GROUP BY category")

aggregate_Gender
aggregate_SeniorCitizen
aggregate_Tenure
aggregate_Partner
aggregate_Dependents
aggregate_Contract
aggregate_PhoneService
aggregate_MultipleLines
aggregate_InternetService
aggregate_OnlineSecurity
aggregate_OnlineBackup
aggregate_DeviceProtection
aggregate_TechSupport
aggregate_StreamingTV
aggregate_StreamingMovies




# Building a cross-table for each independent variable to examine the relationship against the dependent variable (Churn) 
# and each independent variable using the table function.

cross_tab_gender <- table(master_df$Churn, master_df$gender)
cross_tab_SeniorCitizen <- table(master_df$Churn, master_df$SeniorCitizen)
cross_tab_Partner <- table(master_df$Churn, master_df$Partner)
cross_tab_Dependents <- table(master_df$Churn, master_df$Dependents)
cross_tab_Tenure <- table(master_df$Churn, master_df$tenure)
cross_tab_Contract <- table(master_df$Churn, master_df$Contract)
cross_tab_PaperlessBilling <- table(master_df$Churn, master_df$PaperlessBilling)
cross_tab_PaymentMethod <- table(master_df$Churn, master_df$PaymentMethod)
cross_tab_MonthlyCharges <- table(master_df$Churn, master_df$MonthlyCharges)
cross_tab_InternetService <- table(master_df$Churn, master_df$InternetService)
cross_tab_OnlineSecurity <- table(master_df$Churn, master_df$OnlineSecurity)
cross_tab_OnlineBackup <- table(master_df$Churn, master_df$OnlineBackup)
cross_tab_DeviceProtection <- table(master_df$Churn, master_df$DeviceProtection)
cross_tab_TechSupport <- table(master_df$Churn, master_df$TechSupport)
cross_tab_StreamingTV <- table(master_df$Churn, master_df$StreamingTV)
cross_tab_StreamingMovies <- table(master_df$Churn, master_df$StreamingMovies)



# Listing variables to create crosstables:
variables <- c("gender", "SeniorCitizen", "Partner", "Dependents", "tenure", 
               "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", 
               "InternetService", "OnlineSecurity", "OnlineBackup", 
               "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")

# Iterating over each variable and create crosstable:
for (variable in variables) {
  cross_tab <- table(master_df$Churn, master_df[[variable]])
  print(paste("Crosstab for", variable, ":"))
  print(cross_tab)
}




##Data Visualzation of Customers who are not Senior Citizens and have no dependants are most likely to churn.
cols <- c('gender','SeniorCitizen','Partner','Dependents')
numerical <- cols
par(mfrow = c(1, length(cols)), mar = c(7, 3, 2, 1))
for (i in 1:length(cols)) {
  barplot(table(master_df[[cols[i]]]), main = cols[i], 
          col = "skyblue",
          xlab = "Churned Users",
          ylab = "Counts"
          )
  dev.off() # Resetting the plot
}



#Customers who have a phone service most likely to churn.
# Plotting 'tenure' first:
barplot(table(master_df$tenure), main = 'tenure',
        col = "purple",
        xlab = "Churned Users",
        ylab = "Counts")

# Plotting 'PhoneService', 'MultipleLines', and 'InternetService'
cols <- c('PhoneService','MultipleLines','InternetService')
par(mfrow = c(1, length(cols)), mar = c(7, 3, 2, 1))
for (i in 1:length(cols)) {
  barplot(table(master_df[[cols[i]]]), main = cols[i],
          col = "purple",
          xlab = "Churned Users",
          ylab = "Counts")
}
#dev.off() # Resetting the plot


#Customers with no Online Security, Online Backup, Device Protection and Tech Support most likely to churn.
cols <- c('OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport')
numerical <- cols
par(mfrow = c(1, length(cols)), mar = c(7, 3, 2, 1))
for (i in 1:length(cols)) {
  barplot(table(master_df[[cols[i]]]), main = cols[i], 
          col = "lightgreen",
          xlab = "Churned Users",
          ylab = "Counts")
}


 
# # 3.	For numeric variables, box-whisker plot by Churn Status can be useful
# 
par(mfrow = c(1, 1)) # resetting the box plot canvas to single plot (1 row, 1 column) before plotting each variable, ensuring that each variable is plotted separately in its own plot.

# ## Plotting the numeric variable results by Churn Status:
boxplot(master_df$MonthlyCharges, data= master_df, main="Fig.No.1 : BOX PLOT (Total
Monthly Charges)", ylab= "MonthlyCharges", col= "cadetblue3")

# Maybe I can add more plots for numerical variable results (will come back to this)



#############################################################################################################################################################
############################                                                                                                     ############################
##################################################################         Phase 3         ##################################################################
############################                                                                                                     ############################        
#############################################################################################################################################################
#
# Arising Questions:

# Q1.	Create data partition into train and test data sets ( 80/20 or 70/30)
# Q2.	Run Binary Logistic Regression with “Churn” as dependent variables and all others as independent variables ( demographics, services, payment related) on train data.
# Q3.	Check which variables are significant (revise the model if needed)
# Q4.	Relate results to EDA
# Q5.	Check multicollinearity
# Q6.	Obtain ROC curve and AUC for train data
# Q7.	Obtain classification table and accuracy (%)
# Q8.	Obtain threshold to balance sensitivity and specificity
#
# Go to step 9 only if you are satisfied with model on train data
# 9.	Obtain ROC curve and AUC for test data( compare with step 6). Use model object from step 2
# 10.	Use above threshold( step 8) to obtain sensitivity and specificity for test data(compare with step 7)
# 11.	Finalize the model by considering different versions of models ( original variables/recoded variables/mixed).




str(master_df)


# # Fitting logistic regression model to compare against train dataset
lgModel_OnMaster_df<- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets, family = 'binomial', data = master_df)
# Summary of the model
summary(lgModel_OnMaster_df)


# Check for missing values in the target variable
sum(is.na(master_df$Churn))



# A1.	Creating data partition into train and test data sets ( 80/20 or 70/30)

# Checking structure of dataframe (master)
str(master_df)

# Checking for unique values
unique_values <- unique(master_df$Churn)
print(unique_values)


# Setting a seed for reproducibility
set.seed(123)

index<-createDataPartition(master_df$Churn,p=0.8,list=FALSE)  # 80/20 split

train_data<-master_df[index,]
test_data<-master_df[-index,]

# Printing the dimensions of train and test datasets
cat("Dimensions of train dataset:", dim(train_data), "\n")
cat("Dimensions of test dataset:", dim(test_data), "\n")


# ===============================================================================================
# ===============================================================================================


# Checking the structure of the training data
str(train_data)

# Checking the structure of the master df (untrained data before splitting)
str(master_df)


# A2.	Run Binary Logistic Regression with “Churn” as dependent variables and all others as independent variables ( e.g. demographics, services, payment related) on train data.

# Fitting Binary Logistic Regression model
lgModel_OnTrainData2<- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets, family = 'binomial', data = train_data)

# Summary of the model0
summary(lgModel_OnTrainData2)

# RMSE_train2<-sqrt(mean(train_data$resi**2))
# RMSE_train2


# ===============================================================================================
# ===============================================================================================

# A3.	Check which variables are significant (revise the model if needed)
# 
# Check variable significance
# summary(model)

# Extract p-values
p_values <- summary(lgModel_OnTrainData2)$coefficients[, 4]
p_values


# Variables with p-values less than 0.05 are considered significant
significant_train_vars <- names(p_values[p_values < 0.05])

# Display significant variables
significant_train_vars


# ===============================================================================================
# ===============================================================================================


# str(master_df) # re-checking structure of data

# # A4.	Relate results to EDA
# #### Show the variables being relatable to EDA on the Churn Rate..
# 
# Calculate churn rates for each contract type
contract_churn_rates <- aggregate(Churn ~ Contract, data = master_df, FUN = function(x) mean(x == 1))

# Plot churn rates by contract type
barplot(contract_churn_rates$Churn, names.arg = contract_churn_rates$Contract,
        main = "Churn Rates by Contract Type",
        xlab = "Contract Type", ylab = "Churn Rate", col = "skyblue")


# Calculating churn rates for each gender
gender_churn_rates <- aggregate(Churn ~ gender, data = master_df, FUN = function(x) mean(x == 1))

# Plotting churn rates by gender
barplot(gender_churn_rates$Churn, names.arg = gender_churn_rates$gender,
        main = "Churn Rates by Gender Type",
        xlab = "Gender Type", ylab = "Churn Rate", col = "skyblue")

# Calculating churn rates for each Payment Method
paymentMethod_churn_rates <- aggregate(Churn ~ PaymentMethod, data = master_df, FUN = function(x) mean(x == 1))

# Plotting churn rates by Payment Method
barplot(paymentMethod_churn_rates$Churn, names.arg = paymentMethod_churn_rates$PaymentMethod,
        main = "Churn Rates by Payment method Type",
        xlab = "Payment Method", ylab = "Churn Rate", col = "skyblue")




# A5.	Check multicollinearity

# Revising the BLR summary:
lgModel_OnTrainData3<- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets, family = 'binomial', data = train_data)

# Printing the summary of the model
summary(lgModel_OnTrainData3)

# Checking for multicollinearity using VIF on trained model:

vif_values <- car::vif(lgModel_OnTrainData3)

# Printing VIF values
print(vif_values)

# Looking at the Generalized Variance Inflation Factor column, the following variables indicate high vif hence multicollinearity exists:
# MonthlyCharges, PhoneService, StreamingTV, StreamingMovies are more than 5

# Therefore the above variables need to be removed and model needs to be revised.


# Revising the model with the removed variables that were high vif
revised_lgModel_OnTrainData <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets, family = 'binomial', data = train_data)

# Printing the summary of the revised model
summary(revised_lgModel_OnTrainData)


# Re-checking revised model again for multicollinearity using VIF:
vif_values2 <- car::vif(revised_lgModel_OnTrainData)

# Printing VIF values
print(vif_values2) # Upon revision of the model, we can conclude that multicollinearity is no longer present.


# ===============================================================================================
# ===============================================================================================


# A6.	Obtaining ROC curve and AUC for trained data

# Making Predictions probabilities of Churn by fitting the trained model (revised model after multicollinearity)
train_data$predprob<-fitted(revised_lgModel_OnTrainData)
predtrain<-prediction(train_data$predprob,train_data$Churn)

# Calculating performance measures of trained data
roc_perftrain<-performance(predtrain,"tpr","fpr")

# Calculating AUC and converting it to numeric type to add to graph
auc_value <- as.numeric(performance(predtrain, "auc")@y.values)

# printing auc value
print(auc_value)

# Plotting ROC curve
plot(roc_perftrain, main = "ROC Curve for Training Data\n w/Binary Logistical Regression Model",
     col = "blue", lwd = 2, print.auc = TRUE)

abline(0,1)

# Add AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc_value, 3)), adj = c(0.5, 0.5), col = "blue", font = 2)



# A7.	Obtain classification table and accuracy (%)

# Obtaining the classification table and accuracy percentage for the train data:

# Predicting the Churn using the trained model

# Making predictions on the training data
train_data$predicted <- ifelse(train_data$predprob > 0.5, 1, 0)  # Convert probabilities to binary predictions

# Converting predicted and actual churn variables to factors with the same levels
train_data$predicted <- factor(train_data$predicted, levels = c(0, 1))
train_data$Churn <- factor(train_data$Churn, levels = c(0, 1))

# Creating confusion matrix
conf_matrix <- confusionMatrix(train_data$predicted, train_data$Churn)

# Displaying the confusion matrix and accuracy flr train_data of BLR
print(conf_matrix)

# Extracting accuracy from the confusion matrix
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy for train_data (BLR):", round(accuracy * 100, 2), "%"))




# ===============================================================================================
# ===============================================================================================

# A8.	Obtain threshold to balance sensitivity and specificity

# Obtaining threshold to balance sensitivity and specificity on trained data:

sstrain <- performance(predtrain, "sens", "spec")

# Finding optimum threshold for classification by maximizing sensitivity + specificity
best_threshold_train <- sstrain@alpha.values[[1]][which.max(sstrain@x.values[[1]]+sstrain@y.values[[1]])]
print(paste("Best Threshold for training data found is:", round(best_threshold_train * 100, 2), "%"))


# Go to step 9 only if you are satisfied with model on train data
# 
# A9.	Obtain ROC curve and AUC for test data( compare with step 6)

# Re-checking the dimensions for test data
cat("Dimensions of test dataset:", dim(test_data), "\n")

# Creating a prediction object and refitting the trained model to test data
test_data$predprob<-predict(lgModel_OnTrainData2, newdata = test_data, type = "response")
predtest<-prediction(test_data$predprob,test_data$Churn)

# Calculating performance measures of test data
perf_log_test <-performance(predtest,"tpr","fpr")

# Calculating AUC and converting it to numeric type to add to graph
auc1_value_blr_test <- as.numeric(performance(predtest, "auc")@y.values)

# Printing AUC value
print(auc1_value_blr_test)

# Plotting ROC curve for test data
plot(perf_log_test, main = "ROC Curve for Testing Data\n w/Binary Logistical Regression Model",
     col = "blue", lwd = 2, print.auc = TRUE)
abline(0,1)

# Adding AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc1_value_blr_test, 3)), adj = c(0.5, 0.5), col = "red", font = 2)


# ===============================================================================================
# ===============================================================================================

# A10.	Use above threshold to obtain sensitivity and specificity for testing data (compare with step 7)
sstest <- performance(predtest, "sens", "spec")

# Finding optimum threshold for classification by maximizing sensitivity + specificity
best_threshold_test <- sstrain@alpha.values[[1]][which.max(sstest@x.values[[1]]+sstest@y.values[[1]])]
print(paste("Best Threshold for testing data found is:", round(best_threshold_test * 100, 2), "%"))



# A11. Finalize the model by considering different versions of models ( original variables/recoded variables/mixed).
# 
# Let us say you have 3 versions: Select model with highest AUC for test data





#############################################################################################################################################################
############################                                                                                                     ############################
##################################################################         Phase 4         ##################################################################
############################                                                                                                     ############################        
#############################################################################################################################################################

# Arising Questions:
# Q1.	Create data partition into train and test data sets ( 80/20)
# (already done for Logistic Regression)
# Q2.	Apply Naïve Bayes Method on train data 
# Q3.	Obtain ROC curve and AUC for train data
# Q4.	Obtain Confusion Matrix for train data
# Q5.	Obtain ROC curve and AUC for test data
# Q6.	Obtain Confusion Matrix for test data
# Q7.	Repeat steps 3 to 6 for Decision Tree
# Q8.	Repeat steps 3 to 6 for Random Forest Method 
# Q9.	Compare AUC for test data in case of 4 methods and finalize the method


# A1.	Creating data partition into train and test data sets ( 80/20) (already done for Logistic Regression above 80%)

# Checking object structure (master dataframe)
str(master_df)

# Converting dependent variable 'Churn' into a factor
master_df$Churn<-as.factor(master_df$Churn)

# As it's already built above for BLR Model, I'm only going to verify the dimension by printing it out:
cat("Dimensions of train dataset is:", dim(train_data), "\n")
cat("Dimensions of test dataset is:", dim(test_data), "\n")


str(master_df)

# A2.	Fitting Naïve Bayes Method on training data 
NB_ModelChurn <-naiveBayes(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets, data=train_data, family = binomial)
NB_ModelChurn 



# A3.	Obtaining ROC curve and AUC for train data 

# Making Predictions probabilities of Churn by fitting the NB Model above to train data
prednb<-predict(NB_ModelChurn,train_data,type='raw')
pred<-prediction(prednb[,2],train_data$Churn)
perf<-performance(pred,"tpr","fpr")
plot(perf, main = "ROC Curve for Training Data\n w/Naive Bayes Method",
     col = "blue", lwd = 2, print.auc = TRUE)
abline(0,1)


# Area under ROC Curve in R (AUC)
auc2_train<-performance(pred,"auc")
auc2_train <- as.numeric(auc2_train@y.values[[1]])  # Extracting the AUC value from the performance object
class(auc2_train) # Checking object type ensuring that it's numeric
print(auc2_train) #printing AUC value

# Adding AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc2_train, 3)), adj = c(0.5, 0.5), col = "orange", font = 2)



# A4.	Obtaining Confusion Matrix for train data

# Generating a Confusion Matrix for train data w/Naive Bayes Model
prednb<-predict(NB_ModelChurn,train_data,type='raw')
train_data$predY<-ifelse(prednb[,2]>0.25,1,0)
confusionMatrix(factor(train_data$predY),factor(train_data$Churn))



# A5.	Obtaining ROC curve and AUC for test data

# Making Predictions probabilities of Churn by fitting the NB Model to test data
prednb<-predict(NB_ModelChurn,test_data,type='raw')
pred<-prediction(prednb[,2],test_data$Churn)
perf_nb_test <-performance(pred,"tpr","fpr")
plot(perf_nb_test, main = "ROC Curve for Testing Data\n w/Naive Bayes Method",
     col = "blue", lwd = 2, print.auc = TRUE)
abline(0,1)


# Area under ROC Curve in R (AUC)
auc2_test <-performance(pred,"auc")
auc2_nb_value_test <- as.numeric(auc2_test@y.values[[1]])  # Extracting the AUC value from the performance object
class(auc2_test) # Checking object type ensuring that it's numeric
print(auc2_test) #printing AUC value

# Adding AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc2_nb_value_test, 3)), adj = c(0.5, 0.5), col = "red", font = 2)


# # A6.	Obtaining Confusion Matrix for test data 

# Generating a Confusion Matrix for test data w/Naive Bayes Model
prednb<-predict(NB_ModelChurn,test_data,type='raw')
test_data$predY<-ifelse(prednb[,2]>0.25,1,0)
confusionMatrix(factor(test_data$predY),factor(test_data$Churn))








# # A7.	Repeating steps 3 to 6 for Decision Tree

# a) Re-calling dimensions to confirm:
cat("Dimensions of train dataset is:", dim(train_data), "\n")
cat("Dimensions of test dataset is:", dim(test_data), "\n")


# Fitting Decision Tree Algorithm on training data 
DCT_ModelChurn<-rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets,
      data=train_data, method = "class")
# printing the summary of Decision Tree
summary(DCT_ModelChurn)


# Making Predictions probabilities of Churn by fitting the Decision Tree Model to train data
pred_DCT<-predict(DCT_ModelChurn ,train_data,type='prob')
pred<-prediction(pred_DCT[,2],train_data$Churn)

# Creating performance object
perf<-performance(pred,"tpr","fpr")
plot(perf, main = "ROC Curve for Training Data\n w/Decision Tree Algorithm",
     col = "blue", lwd = 2, print.auc = TRUE)
abline(0,1)

# Calculating Area under ROC Curve in R (AUC) for Training Data
auc3_train<-performance(pred,"auc")
auc3_value_train <- as.numeric(auc3_train@y.values[[1]])  # Extracting the AUC value from the performance object
class(auc3_value_train) # Checking object type ensuring that it's numeric
print(auc3_value_train) #printing AUC value

# Adding AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc3_value_train, 3)), adj = c(0.5, 0.5), col = "purple", font = 2)


# b) Generating a Confusion Matrix for train data w/Decision Tree Model 
pred_DCT<-predict(DCT_ModelChurn,train_data,type='prob')
train_data$predY<-ifelse(pred_DCT[,2]>0.25,1,0)
confusionMatrix(factor(train_data$predY),factor(train_data$Churn))




# c) Making Predictions probabilities of Churn by fitting the Decision Tree Model to test data
pred_DCT <-predict(DCT_ModelChurn ,test_data,type='prob')
pred<-prediction(pred_DCT[,2],test_data$Churn)

# Creating performance object
perf_tree_test <-performance(pred,"tpr","fpr")
plot(perf_tree_test, main = "ROC Curve for Testing Data\n w/Decision Tree Algorithm",
     col = "blue", lwd = 2, print.auc = TRUE)
abline(0,1)

# Calculating Area under ROC Curve in R (AUC)
auc3_test<-performance(pred,"auc")
auc3_value_dct_test <- as.numeric(auc3_test@y.values[[1]])  # Extracting the AUC value from the performance object and converting to numeric
class(auc3_value_dct_test) # Checking object type ensuring that it's numeric
print(auc3_value_dct_test) #printing AUC value

# Adding AUC value as text
text(0.5, 0.3, paste("AUC =", round(auc3_value_dct_test, 3)), adj = c(0.5, 0.5), col = "red", font = 2)


# d) Generating a Confusion Matrix for testing data w/Decision Tree Model 
pred_DCT<-predict(DCT_ModelChurn,test_data,type='prob')
test_data$predY<-ifelse(pred_DCT[,2]>0.25,1,0)
confusionMatrix(factor(test_data$predY),factor(test_data$Churn))







# # A8.	Repeating steps 3 to 6 for Random Forest Method 

# a) Re-calling dimensions to confirm:
cat("Dimensions of train dataset is:", dim(train_data), "\n")
cat("Dimensions of test dataset is:", dim(test_data), "\n")


# Converting variable 'Churn' to a factor with levels 0 and 1
train_data$Churn <- as.factor(train_data$Churn) #, levels = c("0", "1"))
test_data$Churn <- as.factor(test_data$Churn)  #,  levels = c("0", "1"))


# Re-checking structure of trained & testing dataset post conversion of dependant variable 'Churn' ensuring that it's a factor for sanity check:
str(train_data$Churn)
str(test_data$Churn)

# Fitting Random Forest Algorithm on training data for classification
RF_ModelChurn<-randomForest(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+numAdminTickets+numTechTickets,
                      data=train_data,
                      mtry=2, ntree=100, importance=TRUE,
                      cutoff=c(0.6,0.4))

# Printing the summary of Random Forest Algorithm
summary(RF_ModelChurn)


# Constructing Importance Matrix
RF_ModelChurn$importance

# Plotting Importance
varImpPlot(RF_ModelChurn, col="blue")



# Predicting probabilities of churn for train data
predicted_probs_train_rf <- predict(RF_ModelChurn, train_data, type = "prob")[, 2]

# Constructing the lengths of the response variables of predicted probabilities
length_response <- length(train_data$Churn)
length_probs_train_RF <- length(predicted_probs_train_rf)

# Checking if response lengths match with the predicted outcomes for training data:
if (length_response != length_probs_train_RF) {
  stop("Length of response variable does not match length of predicted probabilities (train_data) for Random Forest Classifier Model (testing data).")
}

# Creating a prediction object
pred <- prediction(predicted_probs_train_rf, train_data$Churn)

# Creating performance object
perf <- performance(pred, "tpr", "fpr")

# Plotting ROC curve
plot(perf, main = "ROC Curve for Training Data\n w/Random Forest Algorithm",
     col = "blue", lwd = 2, print.auc = TRUE)

# Calculating AUC
auc4_rf_train <- performance(pred, "auc")
auc4_rf_value_train <- as.numeric(auc4_rf_train@y.values) # Extracting the AUC value from the performance object and converting to numeric

class(auc4_rf_value_train) # Checking object type ensuring that it's numeric
print(auc4_rf_value_train) #Printing AUC value

# Adding AUC value to the plot
text(0.5, 0.3, paste("AUC =", round(auc4_rf_value_train, 3)), adj = c(0.5, 0.5), col = "orange", font = 2)



# b) Generating Confusion Matrix for training data w/Random Forest Algorithm 
pred_RF<-predict(RF_ModelChurn,train_data,type='prob')
train_data$predY<-ifelse(pred_RF[,2]>0.25,1,0)
confusionMatrix(factor(train_data$predY),factor(train_data$Churn))





# c) Predicting probabilities of churn for testing data for Random Forest Method
predicted_probs_test_rf <- predict(RF_ModelChurn, test_data, type = "prob")[, 2]

# Constructing the lengths of the response variables of predicted probabilities
length_response <- length(test_data$Churn)
length_probs_test_RF <- length(predicted_probs_test_rf)

# Checking if response lengths match with the predicted outcomes for testing data:
if (length_response != length_probs_test_RF) {
  stop("Length of response variable does not match length of predicted probabilities  (test_data) for Random Forest Classifier Model (testing data).")
}

# Checking level structure of variable 'Churn' of Test Data
str(test_data$Churn)

# Creating a prediction object and refitting the trained model to test data
pred <- prediction(predicted_probs_test_rf, test_data$Churn)

# Creating performance object
perf_rf_test <- performance(pred, "tpr", "fpr")

# Plotting ROC curve
plot(perf_rf_test, main = "ROC Curve for Testing Data\n w/Random Forest Algorithm",
     col = "blue", lwd = 2, print.auc = TRUE)

# Calculating AUC
auc4_rf_test <- performance(pred, "auc")
auc4_rf_value_test <- as.numeric(auc4_rf_test@y.values) # Extracting the AUC value from the performance object and converting to numeric

class(auc4_rf_value_test) # Checking object type ensuring that it's numeric
print(auc4_rf_value_test) # Printing AUC value

# Adding AUC value to the plot
text(0.5, 0.3, paste("AUC =", round(auc4_rf_value_test, 3)), adj = c(0.5, 0.5), col = "orange", font = 2)




# d) Generating a Confusion Matrix for testing data w/Random Forest Algorithm 
pred_RF<-predict(RF_ModelChurn,test_data,type='prob')
test_data$predY<-ifelse(pred_RF[,2]>0.25,1,0)
confusionMatrix(factor(test_data$predY),factor(test_data$Churn))





# A9.	Comparing AUC for test data in case of 4 methods and finalize the method

# Since I've already constructed the 4 models previously and formed the predictions and performances respectively, no need to do it again so I will recall them by reusing them to plot the curves in one graph. 

# Plotting ROC curves
plot(perf_log_test, col = "red", lwd = 2, main = "ROC Curves for Different Models")
plot(perf_nb_test, col = "blue", lwd = 2, add = TRUE)
plot(perf_tree_test, col = "green", lwd = 2, add = TRUE)
plot(perf_rf_test, col = "purple", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Binary Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest"),
       col = c("red", "blue", "green", "purple"), lwd = 2)

# Adding AUC values to the plot
text(0.5, 0.6, paste("Binary Logistic Regression AUC =", round(auc1_value_blr_test, 3)), col = "red")
text(0.5, 0.55, paste("Naive Bayes Classifier AUC =", round(auc2_nb_value_test, 3)), col = "blue")
text(0.5, 0.5, paste("Decision Tree Method AUC =", round(auc3_value_dct_test, 3)), col = "green")
text(0.5, 0.45, paste("Random Forest Method AUC =", round(auc4_rf_value_test, 3)), col = "purple")

# Printing AUC values for the Models
print(paste("Binary Logistic Regression AUC for Testing Data:", round(auc1_value_blr_test, 3)))
print(paste("Naive Bayes AUC for Testing Data:", round(auc2_nb_value_test, 3)))
print(paste("Decision Tree AUC for Testing Data:", round(auc3_value_dct_test, 3)))
print(paste("Random Forest AUC for Testing Data:", round(auc4_rf_value_test, 3)))

# Finalizing the method with the highest AUC
model_AUCs <- c(Logistic_Regression = auc1_value_blr_test, Naive_Bayes = auc2_nb_value_test, Decision_Tree = auc3_value_dct_test, Random_Forest = auc4_rf_value_test)
best_Model <- names(which.max(model_AUCs))
print(paste("The best model is:", best_Model, "with AUC =", round(max(model_AUCs), 3)))

