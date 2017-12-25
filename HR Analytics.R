

# HR Analytics Case Study

#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Co XYZ employs 4000 employees and has 15% attrition. It feels this level of Att is bad
# for 3 reasons a) Its projects get delayed, missed timelines b) Needs to maintain a big
# recruting division and c) New Emp have to be trained

## GOAL:

# To Model the probability of Attrition using Logit Regression
# and to find the factors affecting attrition

# Here data is gathered from 4 sources
# 1. Base file
# 2. Employee survey - EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance 
# 3. Managers Survey - PerformanceRating & JobInvolvement
# 4. Time keeping system - Gives the System date and time for "In" and "Out"

################################################################

### Data Understanding

# Load the required packages

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

# Loading 3 files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)

str(general_data)    # 4410 obs of 24 variables including the target variable
str(employee_survey) # 4410 obs of 4 variables
str(manager_survey) # 4410 obs of 3 variables

# Collate the data together in one single file
length(unique(general_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(employee_survey$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(manager_survey$EmployeeID)) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,employee_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey$EmployeeID) # Identical EmployeeID across these datasets

empdata<- merge(general_data,employee_survey, by="EmployeeID", all = F)
empdata<- merge(empdata,manager_survey, by="EmployeeID", all = F)

View(empdata) #master file

################################################################

#calculating derived metrics, average hours worked by a) converting outtime and intime 
#to date format inseconds, b) substracting them c) converting to Number d) calculating 
#avg of hrs worked e) appending it to the master file
# Loading the 2 files
intime<- read.csv("in_time.csv", stringsAsFactors = F)
outtime<- read.csv("out_time.csv", stringsAsFactors = F)

# adding the Employee id - column name to both files
colnames(intime)[1]<-"EmployeeID"
colnames(outtime)[1]<-"EmployeeID"

# checking in Emp id
length(unique(intime$EmployeeID)) # 4410, confirming EmployeeID is key 
length(unique(outtime$EmployeeID)) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,intime$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,outtime$EmployeeID) # Identical EmployeeID across these datasets

#Converting to date format, such that it can be stored in Seconds
for(i in 2:ncol(intime)) {
intime[,i]<-as.POSIXct(intime[,i], tz="", "%m/%d/%Y %H:%M")
}
str(intime) #the DF has changed to date format correctly
for(i in 2:ncol(outtime)) {
  outtime[,i]<-as.POSIXct(outtime[,i], tz="", "%Y-%m-%d %H:%M:%S")
}
str(outtime) #the DF has changed to date format correctly

#Creating a new df
date<-rnorm(4410)
date <-as.data.frame(date)
#Substracting outtime from intime to arrive at hours worked every day and storing in date df
for(i in 2:ncol(outtime)) {
  date[,c(i)]<-outtime[,i]-intime[,i]
}
date<-date[-1]
#Converting the new df to numeric (from atomic format)
for(i in 1:261) {
  date[,i]<-as.numeric(date[,i])
}
#Calculating No_of_Leaves and Off days over the period
for(i in 1:nrow(outtime)) {
  date[i,262] <- sum(is.na(date[i,]))
}
date$V262 # The data shows correctly
colnames(date)[262]<-"No_of_Leaves"

#Calculating average hours worked by each employee over 261 days

date[is.na(date)]<- 0
date$AvgHrsWorked <- rowMeans(date[1:262])

#appending the derived metrics to our master data set
date <- cbind (date, intime$EmployeeID)
date_final<- date[,262:264]
colnames(date_final)[3]<-"EmployeeID"

empdata<- merge(empdata,date_final, by="EmployeeID", all = F)

# Understanding the structure of the collated file
str(empdata) #4410 obs. of 31 variables;

empdata <- empdata [,c(-9,-16,-18)] #Removing employee count, over 18, and StandardHrs: No value to the model

# Missing values treatment
sum(is.na(empdata)) # 111 na values
empdata1 <- na.omit(empdata) #4300 obs of 31 variables
# Now one approach can be to impute the na values they are less than 10%. However all the NA values 
# are in Categorical variables, hence replacing the NA values with mode
# EnvironmentSatisfaction - 25 NA's, JobSatisfaction - 20NA's & WorkLifeBalance - 38 NA's

na.index<-which(is.na(empdata$EnvironmentSatisfaction))
empdata$EnvironmentSatisfaction[na.index]<-3

na.index<-which(is.na(empdata$JobSatisfaction))
empdata$JobSatisfaction[na.index]<-4

na.index<-which(is.na(empdata$WorkLifeBalance))
empdata$WorkLifeBalance[na.index]<-3

sum(is.na(empdata)) # 9 NA-TotalWorkingYears & 19 NA-NumCompaniesWorked (continuous variable)
empdata <- na.omit(empdata) #4382 obs of 28 variables

summary(empdata)

# Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(empdata, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(empdata, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=Education,fill=Attrition))+ geom_bar(),
          ggplot(empdata, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(empdata, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          ggplot(empdata, aes(x=EnvironmentSatisfaction ,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(empdata, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(empdata, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(),
          ggplot(empdata, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

#Amongst Business travellers - Frequent Travellers have higher attrition
#Sales dept has a higher attrition Vs other Depts
#Attrition is higher for people in Eduction bucket 1 and 2
#Females have a higher attrition
#MaritalStatus - Single have higher attrition
#Giving Stock Options - Does not seem to be help in reducing Attrition
#Employee with Environment Satisfacton 1 have highest attrition
#employees who gave Employee satisfaction scores of 1 in the JOb Satisfaction and Work life balance
#have higher attrition

# Boxplots for numeric variables:First row has the spread and 2nd row has spread relative to Attrition

plot_grid(ggplot(empdata, aes(x="", y=Age))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=DistanceFromHome))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=MonthlyIncome))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=Age))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=DistanceFromHome))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=MonthlyIncome))+ geom_boxplot(),
          align = "h")

plot_grid(ggplot(empdata, aes(x="", y=TotalWorkingYears))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=TrainingTimesLastYear))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=YearsAtCompany))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=TotalWorkingYears))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=TrainingTimesLastYear))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=YearsAtCompany))+ geom_boxplot(),
          align = "h")

plot_grid(ggplot(empdata, aes(x="", y=YearsSinceLastPromotion))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=YearsWithCurrManager))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=No_of_Leaves))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=YearsSinceLastPromotion))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=YearsWithCurrManager))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=No_of_Leaves))+ geom_boxplot(),
          align = "h")

plot_grid(ggplot(empdata, aes(x="", y=AvgHrsWorked))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=NumCompaniesWorked))+ geom_boxplot(),
          ggplot(empdata, aes(x="", y=PercentSalaryHike))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=AvgHrsWorked))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=NumCompaniesWorked))+ geom_boxplot(),
          ggplot(empdata, aes(x=Attrition, y=PercentSalaryHike))+ geom_boxplot(),
          align = "h")

# More relatively younger people are leaving the organisation
# The attriting people packed more tighty in the lower income groups
# People to tend to leave have less "Total work experience" and less "Years of work exp with the Co."
# More attrition in the Promoted with in "1 to 2 years bucket"
# More attrition in people having worked in less no of companies
# More attrition for people who are taking lesser leaves
# More attrition for people,working longer hours

# There are no outliers in Age, DistanceFromHome and No_of_Leaves
# There are heavy outliers in MonthlyIncome, TotalWorkingYears, YearsAtCompany and AvgHrsWorked
# These will need outlier treatment
#Since there will be high correlation between TotalWorkingYears & YearsAtCompany, treating
# only TotalWorkingYears for outliers, YearsAtCompany outliers will automatically get imputed
# Other variables have negligble outliers

## Outlier treatment

box <- boxplot.stats(empdata$MonthlyIncome)
out <- box$out

empdata1 <- empdata[ !empdata$MonthlyIncome %in% out, ]
empdata <- empdata1

box <- boxplot.stats(empdata$TotalWorkingYears)
out <- box$out

empdata1 <- empdata[ !empdata$TotalWorkingYears %in% out, ]
empdata <- empdata1

box <- boxplot.stats(empdata$AvgHrsWorked)
out <- box$out

empdata1 <- empdata[ !empdata$AvgHrsWorked %in% out, ]
empdata <- empdata1

# Correlation between numeric variables

library(GGally)
ggpairs(empdata[, c("Attrition", "Age", "DistanceFromHome", "MonthlyIncome", "TotalWorkingYears",
                    "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion",
                    "YearsWithCurrManager", "No_of_Leaves", "AvgHrsWorked", "PercentSalaryHike",
                    "NumCompaniesWorked")])

#Total working years and Age are correlated
#Total working years and With currrent manager are correlated
#Years since last promotion and Montly income are correlated 
#Years with current manager and Total working years are highly correlated

################################################################
### Data Preparation

# De-Duplication - not needed

################################################################
# Feature standardisation

# Normalising continuous features 
empdata$Age<- scale(empdata$Age)
empdata$DistanceFromHome<- scale(empdata$DistanceFromHome)
empdata$MonthlyIncome<- scale(empdata$MonthlyIncome)
empdata$TotalWorkingYears<- scale(empdata$TotalWorkingYears)
empdata$TrainingTimesLastYear<- scale(empdata$TrainingTimesLastYear)
empdata$YearsAtCompany<- scale(empdata$YearsAtCompany)
empdata$YearsSinceLastPromotion <- scale(empdata$YearsSinceLastPromotion)
empdata$YearsWithCurrManager<- scale(empdata$YearsWithCurrManager)
empdata$No_of_Leaves<- scale(empdata$No_of_Leaves)
empdata$AvgHrsWorked<- scale(empdata$AvgHrsWorked)
empdata$PercentSalaryHike<- scale(empdata$PercentSalaryHike) 
empdata$NumCompaniesWorked<- scale(empdata$NumCompaniesWorked) 

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
empdata$Attrition<- ifelse(empdata$Attrition=="Yes",1,0)

# creating a dataframe of categorical features
empdata_chr<- empdata[,c(4,5,7,8,9,10,11,12,16,22,23,24,25,26)]

# converting categorical attributes to factor
empdata_fact<- data.frame(sapply(empdata_chr, function(x) factor(x)))
str(empdata_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(empdata_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =empdata_fact))[,-1]))

# Final dataset
empdata_final<- cbind(empdata[,-c(4,5,7,8,9,10,11,12,16,22,23,24,25,26)],dummies) 
View(empdata_final) #3790 obs. of 58 variables

empdata_final<-empdata_final[,-1] #removing EmployeeID (Key)

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(empdata_final$Attrition, SplitRatio = 0.7)

train = empdata_final[indices,]

test = empdata_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 1836.7....57 coeff..nullDev 2380.5...resDev 1722.7

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)#AIC 1806.1....36 coeff..nullDev 2380.5...resDev 1732.1

# Removing multicollinearity through VIF check

vif(model_2) 
# Department.xSales & Department.xResearch...Development both have VIF>4
#Department.xResearch...Development has higher pValue

#Excluding Department.xResearch...Development
model_3 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Department.xSales + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x4 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_3) #AIC 1817.8....35 coeff..nullDev 2380.5...resDev 1745.8

vif(model_3)  
#WorkLifeBalance.x2 and WorkLifeBalance.x3 both VIF>3
#WorkLifeBalance.x2 higher pValue

#Excluding WorkLifeBalance.x2
model_4 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Department.xSales + Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x4 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)             

summary(model_4) #AIC 1829.7....34 coeff..nullDev 2380.5...resDev 1759.7

vif(model_4) 
# Now VIF Values are fairly low (all less than 2.5), only remove variables basis high pValue

#Excluding Department.xSales JobLevel.x4 
model_5 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x4 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_5) #AIC 1829.8....33 coeff..nullDev 2380.5...resDev 1761.8

#Excluding JobLevel.x4 
model_6 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_6) #AIC 1830.1....32 coeff..nullDev 2380.5...resDev 1764.1

#Excluding PerformanceRating 
model_7 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, 
              family = "binomial", data = train)

summary(model_7) #AIC 1830.6....31 coeff..nullDev 2380.5...resDev 1766.6

#Excluding WorkLifeBalance.x4 
model_8 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3, 
              family = "binomial", data = train)

summary(model_8) #AIC 1830.9....30 coeff..nullDev 2380.5...resDev 1768.9

#Excluding JobLevel.x5   
model_9 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3, 
              family = "binomial", data = train)

summary(model_9) #AIC 1831.9....29 coeff..nullDev 2380.5...resDev 1771.9

#Excluding EducationField.xMedical   
model_10 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                Education.x2 + Education.x5 + EducationField.xMarketing + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3, 
              family = "binomial", data = train)

summary(model_10) #AIC 1832.6....28 coeff..nullDev 2380.5...resDev 1774.6

#Excluding EducationField.xTechnical.Degree   
model_11 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 + Education.x5 + EducationField.xMarketing + 
                 EducationField.xOther + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_11) #AIC 1833.8....27 coeff..nullDev 2380.5...resDev 1777.8

#Excluding StockOptionLevel.x1   
model_12 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 + Education.x5 + EducationField.xMarketing + 
                 EducationField.xOther + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_12) #AIC 1834.9....26 coeff..nullDev 2380.5...resDev 1780.9

#Excluding EducationField.xMarketing   
model_13 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 + Education.x5 + 
                 EducationField.xOther + 
                 JobRole.xLaboratory.Technician + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_13) #AIC 1836.4....25 coeff..nullDev 2380.5...resDev 1784.4

#Excluding JobRole.xLaboratory.Technician   
model_14 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 + Education.x5 + 
                 EducationField.xOther + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_14) #AIC 1837.7....24 coeff..nullDev 2380.5...resDev 1787.7

#Excluding Education.x5   
model_15 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 +
                 EducationField.xOther + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_15) #AIC 1839.8....23 coeff..nullDev 2380.5...resDev 1791.8

#Excluding JobRole.xManufacturing.Director   
model_16 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 Education.x2 +
                 EducationField.xOther + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_16) #AIC 1842.6....22 coeff..nullDev 2380.5...resDev 1796.6

#Excluding Education.x2    
model_17 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_17) #AIC 1845.9....21 coeff..nullDev 2380.5...resDev 1801.9

#Excluding EducationField.xOther
model_18 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_18) #AIC 1850.5....20 coeff..nullDev 2380.5...resDev 1808.5

#Excluding JobRole.xResearch.Director
model_19 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, 
               family = "binomial", data = train)

summary(model_19) #AIC 1854....19 coeff..nullDev 2380.5...resDev 1814.0

#Excluding JobInvolvement.x3
model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 No_of_Leaves + AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3, 
               family = "binomial", data = train)

summary(model_20) #AIC 1858.7...18 coeff..nullDev 2380.5...resDev 1820.7

#Excluding No_of_Leaves
model_21 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3, 
               family = "binomial", data = train)

summary(model_21) #AIC 1863.5...17 coeff..nullDev 2380.5...resDev 1827.5

#Excluding Age
model_22 = glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3, 
               family = "binomial", data = train)

summary(model_22) #AIC 1868.9...16 coeff..nullDev 2380.5...resDev 1834.9

#Excluding MaritalStatus.xMarried
model_23 = glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3, 
               family = "binomial", data = train)

summary(model_23) #AIC 1875.2...15 coeff..nullDev 2380.5...resDev 1843.2

#Excluding TrainingTimesLastYear
model_24 = glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgHrsWorked + BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3, 
               family = "binomial", data = train)

summary(model_24) #AIC 1882...14 coeff..nullDev 2380.5...resDev 1852.0

#Stopping at model_24, since pValues of all variables are less than 0.0004, we have 14 
#coeff in the final model

########################################################################
# With 14 significant variables in the model

final_model<- model_24

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)

# Summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrn <- factor(ifelse(test$Attrition ==1,"Yes","No"))


table(test_actual_attrn,test_pred_attrn)


#######################################################################
# Let's use the probability cutoff of 40%.
test_pred_attrn <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

library(e1071)

test_conf <- confusionMatrix(test_pred_attrn, test_actual_attrn, positive = "Yes")
test_conf
#######################################################################

# Let's Choose the cutoff value. 

# Let's find out the optimal probabilility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrn, test_actual_attrn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=6),seq(0,1,length=6),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(.50,1,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#_________________________________________________________

# Let's choose a cutoff value of 0.191 for final model

test_cutoff_attrn <- factor(ifelse(test_pred >=0.191, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrn, test_actual_attrn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # Accuracy 0.7528584 

sens # Sencitivity 0.7234043

spec # Specificitivity 0.7586934

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrn <- ifelse(test_cutoff_attrn=="Yes",1,0)
test_actual_attrn <- ifelse(test_actual_attrn=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrn, test_actual_attrn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.4820976

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrn_decile = lift(test_actual_attrn, test_pred, groups = 10)
attrn_decile
#our gain Table
# A tibble: 10 × 6
#bucket total totalresp Cumresp      Gain  Cumlift
#<dbl> <int>     <dbl>   <dbl>     <dbl>    <dbl>
#1       1   114        71      71  37.76596 3.776596
#2       2   114        39     110  58.51064 2.925532
#3       3   114        24     134  71.27660 2.375887
#4       4   113        18     152  80.85106 2.021277
#5       5   114        10     162  86.17021 1.723404
#6       6   114        11     173  92.02128 1.533688
#7       7   113         3     176  93.61702 1.337386
#8       8   114         3     179  95.21277 1.190160
#9       9   114         3     182  96.80851 1.075650
#10     10   113         6     188 100.00000 1.000000