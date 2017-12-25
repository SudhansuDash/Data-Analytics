# Loading libraries
library(lubridate)
library(scales)
library(ggplot2)
library(corrplot)

#Laoding the Loan data file into laon_data_set data frame
loan_data_set <- read.csv("loan.csv")

# Checking for duplicated data in the loan_data_set, and no duplicate records exist
sum(duplicated(loan_data_set$id))

##########Data Cleaning_sud##################
# Creating the lona_data dataframe 
loan_data <- loan_data_set

# Eliminating the columns which contains data of more than 40% as NA and 0's
loan_data <- loan_data[,-which(colMeans(loan_data == 0 | is.na(loan_data)) > 0.4)]

#Dropping columns which have same data value in the complete dataset
#Dropping the application_type column as its value is INDIVIDUAL in the entire dataset
loan_data <- loan_data[,-41]
# Dropping the column policy code as its values is 1 in entire dataset
loan_data <- loan_data[,-40]
# Dropping the column next_pymnt_d as it contains 50 % blank values
loan_data <- loan_data[,-38]
# Dropping the column intial_list_status as its value is 'f' in complete dataset
loan_data <- loan_data[,-31]
# Dropping the column pymnt_plan as its value is 'n' in complete dataset
loan_data <- loan_data[,-18]
# Dropping the columns url and desc as they can be ignored for the analysis
loan_data <- loan_data[,-c(18:19)]


#Changing the datatype of emp_title to char
loan_data$emp_title <- as.character(loan_data$emp_title)
# Handlng the blank values in the emp_title and assigning them as NA
loan_data$emp_title[loan_data$emp_title ==  ""]<- NA

# Handling the title column with NA values and assigning it as 0
loan_data$title[is.na(loan_data$title)]<- 0

#As part of datacleaning, replacing the emp_length column with value '<1' as 0 
loan_data$emp_length <- gsub("< 1","0",loan_data$emp_length)
# Making the emp_length column as factor type
loan_data$emp_length <- as.factor(loan_data$emp_length)

# Converting the date columns in loan_data to the standard format in R
# Adding '01-' as date for every row in the issue_d and making it in YYYY_MM_DD format
loan_data$issue_d <- parse_date_time(paste("01-",loan_data$issue_d),orders = c("d-b-y"))
# Adding '01-' as date for every row in the earliest_cr_line and making it in YYYY_MM_DD format
loan_data$earliest_cr_line<- parse_date_time(paste("01-",loan_data$earliest_cr_line),orders = c("d-b-y"))
# Adding '01-' as date for every row in the last_credit_pull_d and making it in YYYY_MM_DD format
loan_data$last_credit_pull_d <- parse_date_time(paste("01-",loan_data$last_credit_pull_d),orders = c("d-b-y"))
# Adding '01-' as date for every row in the last_pymnt_d and making it in YYYY_MM_DD format
loan_data$last_pymnt_d<-parse_date_time(paste("01-",loan_data$last_pymnt_d),orders = c("d-b-y"))

#Changing the addr_state column from abbreviated format to complete name format
loan_data$addr_state <- state.name[match(loan_data$addr_state,state.abb)]

#Changing the columns int_rate and revol_util from factor level to numeric type
loan_data$int_rate <- as.numeric(loan_data$int_rate)
loan_data$revol_util <- as.numeric(loan_data$revol_util)

################### Data Analysis########################
## Copying the loan_data into cleaned _data_set dataframe and performing analysis on this 
cleaned_loan_data<- loan_data

# Plot made for Verification status to Loan Status
ggplot(cleaned_loan_data,aes(x = cleaned_loan_data$verification_status, fill = cleaned_loan_data$loan_status))+geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing the Loan Status with the Verification Stages")+xlab("Verification Status")+ylab("Loan Status")

# Plot for the three Loan status, comparing the Employee length to the number of loan taken
ggplot(cleaned_loan_data,aes(x = cleaned_loan_data$emp_length, fill = cleaned_loan_data$funded_amnt))+geom_histogram(stat = "count")+facet_wrap(~cleaned_loan_data$loan_status)+theme(axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle("Analysing the Loan Status with the Investement amount over a tenure of loan period")+xlab("Employee Length")+ylab("Count")
#Can be infered that loans taken by customers of emp_length of less than 1 year are more defaulted


#Plot analysing the Issue date with the funded amount over a period of time
ggplot(cleaned_loan_data,aes(cleaned_loan_data$issue_d, y = cleaned_loan_data$funded_amnt))+geom_smooth()+theme(axis.text.x = element_text(angle = 30,hjust = 0.8))+ggtitle("Analysing Funded amount lended by the comapany over a period of time")+xlab("Laon issueed Year")+ylab("Funded Amount by Comapny")

#Plot for Loan Status for two different Terms and can be infered that Charged off status is more observed for short term of 36 months
ggplot(cleaned_loan_data,aes(x = cleaned_loan_data$loan_status, fill = cleaned_loan_data$funded_amnt))+geom_bar(position = "dodge")+facet_wrap(~cleaned_loan_data$term)+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing the Loan Status with the Funded loan amount over the term")+xlab("Loan Status")+ylab("Count")

#Plot for analysing the Home Owner ship with the Loan Funded Amount for the various Loan Status
ggplot(cleaned_loan_data,aes(x = cleaned_loan_data$home_ownership, fill = cleaned_loan_data$funded_amnt))+geom_bar(position = "dodge")+facet_wrap(~cleaned_loan_data$loan_status)+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing the Home Ownership factor with the Funded Loan Amount")+xlab("Home Ownership Factors")+ylab("Count")

## creating the dataframe chargedoff_data which contains data of only charged Off Loan status customers
chargedoff_data <- subset(cleaned_loan_data,cleaned_loan_data$loan_status == "Charged Off")

#Plot for knowing the trend of Loan amonut taken by Defaulters
ggplot(chargedoff_data,aes(x = chargedoff_data$loan_amnt))+geom_histogram()+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing the Loan amount taken by Defaulters")+xlab("Loan Amount")+ylab("Count")

#Plot for analysing the Purpose mentioned by the defaulters and debit+consolidation is found to be the most specified reason
ggplot(chargedoff_data,aes(x = chargedoff_data$purpose))+geom_bar()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle("Analysing the Defaulter with purpose of Loan ")+xlab("Purpose for Loan taken")+ylab("Count")

#Plot  showing the home ownership status of the defaulters and can be infered that Rent and Mortgage status are more for defaulters
ggplot(chargedoff_data,aes(x = chargedoff_data$home_ownership))+geom_bar()+theme(axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle("HomeOwner ship of the defaulters ")+xlab("Defaulters Home Ownership ")+ylab("Count")


#Plot showing the the Home Ownership with the verifiaction status of the Defaulters loan approal process
ggplot(chargedoff_data,aes(x = chargedoff_data$home_ownership, fill = chargedoff_data$verification_status))+geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing the Home Ownership factor to verification status")+xlab("Home Ownership Factors")+ylab("Count")
# here Rent and No verification status can be infered as the risk factor


#Plot showing the annual income to the monthly installment pay of the defaulters
ggplot(chargedoff_data,aes(x = chargedoff_data$annual_inc, y = chargedoff_data$installment))+geom_smooth()+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Analysing Annual Income of defaulters to the every month installments they pay")+xlab("Annual Income of Defaulters")+ylab("Installments of Defaulters")
# Can be infered that defaulters with annual income less than 40000 and having high monthly installments to pay are more which is a risky factor and could be the reason for the default


#Box Plot for defaulters DTI to the Interest Rate and can be infered that the interest rate for 36 months is more comapred to that of 60 months tensure 
ggplot(chargedoff_data,aes(x = chargedoff_data$dti, y = chargedoff_data$int_rate))+geom_boxplot()+facet_wrap(~chargedoff_data$term)+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("DTI to the Interest Rate of the Defaulters for Different Term Period")+xlab("Defaulters DTI ")+ylab("Interest rate of Defaulters")

# Box plot for the defaulters loan grade to the funded amount
ggplot(chargedoff_data,aes(x = chargedoff_data$grade, y = chargedoff_data$funded_amnt_inv))+geom_boxplot()+theme(axis.text.x = element_text(angle = 30,hjust = 0.9))+ggtitle("Defaulters Loan Graded Status to the Funded Amount of Company")+xlab("Loan Grade Status")+ylab("Funded amount to defaulters")

## Correlation of Investment amount to the DTI of the defaulters
# Creating data frame Default_cor_df with the investent amount, interset rate, annual income and DTI of the defaulters
Default_cor_df <- data.frame(chargedoff_data$funded_amnt_inv,chargedoff_data$int_rate,chargedoff_data$annual_inc,chargedoff_data$dti)
# Correlation  of the variables in Default_cor_df
Dcor <- cor(Default_cor_df)
#Plot for the correlation matrix of the defaulters funded amount,interest rate, annual income and dti
# Customised correlation matrix for the defaulters data
corrplot(Dcor)
image(x=seq(nrow(Dcor)), y=seq(ncol(Dcor)), z=cor(Default_cor_df), axes=F, xlab="Investemnt/Interset rate/Income/DTI", ylab="Investemnt/Interset rate/Income/DTI")
text(expand.grid(x=seq(dim(Dcor)[1]), y=seq(dim(Dcor)[2])), labels=round(c(Dcor),2))
box()








