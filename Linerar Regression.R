library(MASS)
library(car)
library(tidyR)
library(stringr)

############################## Importing Data to R environemnt###################
Cardata<-read.csv("CarPrice_Assignment.csv")
View(Cardata)

############################### Data Cleaning ##############################################
#duplicate check
sum(duplicated(Cardata))

#NA Value check
sum(is.na(Cardata))

#displaying the structure of Cardata
str(Cardata)



############################# Dummy Variables creation #####################################

levels(Cardata$fueltype)<-c(1,0)
Cardata$fueltype<- as.numeric(levels(Cardata$fueltype))[Cardata$fueltype]
summary(factor(Cardata$fueltype))

levels(Cardata$aspiration)<-c(1,0)
Cardata$aspiration<- as.numeric(levels(Cardata$aspiration))[Cardata$aspiration]

levels(Cardata$doornumber)<-c(1,0)
Cardata$doornumber<- as.numeric(levels(Cardata$doornumber))[Cardata$doornumber]

levels(Cardata$enginelocation)<-c(1,0)
Cardata$enginelocation<- as.numeric(levels(Cardata$enginelocation))[Cardata$enginelocation]

summary(factor(Cardata$drivewheel))
temp_1<-data.frame(model.matrix(~drivewheel,data=Cardata))
temp_1<-temp_1[,-1]
Cardata_1<-cbind(Cardata[,!(names(Cardata) %in% c("drivewheel"))],temp_1)
str(Cardata_1)

temp_2<-data.frame(model.matrix(~enginetype,data=Cardata_1))
temp_2<-temp_2[,-1]
Cardata_1<-cbind(Cardata_1[,!(names(Cardata_1) %in% c("enginetype"))],temp_2)
str(Cardata_1)

temp_3<-data.frame(model.matrix(~fuelsystem,data=Cardata_1))
temp_3<-temp_3[,-1]
Cardata_1<-cbind(Cardata_1[,!(names(Cardata_1) %in% c("fuelsystem"))],temp_3)
str(Cardata_1)

temp_4<-data.frame(model.matrix(~carbody,data=Cardata_1))
temp_4<-temp_4[,-1]
Cardata_1<-cbind(Cardata_1[,!(names(Cardata_1) %in% c("carbody"))],temp_4)
str(Cardata_1)


factor(Cardata_1$cylindernumber)
wordToNum<-function(input){
  output <- 0
  if (input == "two") { output <- 2}
  else if (input == "three") {output <- 3}
  else if (input == "four") {output <- 4}
  else if (input == "five") {output <- 5}
  else if (input == "six") {output <- 6}
  else if (input == "eight") {output <- 8}
  else {output <- 12}
  return(output)
}

Cardata_1$cylindernumber<-as.numeric(as.character(unlist((lapply(Cardata_1$cylindernumber,wordToNum)))))
factor(Cardata_1$cylindernumber)

Cardata_1<-separate(Cardata_1,CarName,into = c("BrandName"),sep = " ",extra="drop")
View(Cardata_1)
summary(factor(Cardata_1$BrandName))

temp_5<-data.frame(model.matrix(~BrandName,data=Cardata_1))
temp_5<-temp_5[,-1]
Cardata_2<-cbind(Cardata_1[,!(names(Cardata_1) %in% c("BrandName"))],temp_5)
str(Cardata_2)

summary(factor(Cardata_1$symboling))
temp_6<-data.frame(model.matrix(~symboling,data=Cardata_1))
temp_6<-temp_6[,-1]
Cardata_1$symboling<-temp_6
str(Cardata_1)

Cardata_1<-Cardata_1[,!(names(Cardata_1) %in% c("BrandName","car_ID"))]


########################################## Data modelling ##################################################
#set the seed to 100 and lets run it
set.seed(100)

trainindices=sample(1:nrow(Cardata_1),0.7*nrow(Cardata_1))
train=Cardata_1[trainindices,]
test=Cardata_1[-trainindices,]

#Initial model creation
model_1 = lm(price~., data = trainingData)
summary(model_1)
step <- stepAIC(model_1, direction="both")
step
model_2 <- lm( price ~ symboling + aspiration + enginelocation + 
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + stroke + peakrpm + highwaympg + enginetypeohc + 
                enginetypeohcv + enginetyperotor + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystemspdi, data = trainingData)

summary(model_2)

vif(model_2)
model_3 <- lm(price ~ aspiration + enginelocation + carheight + enginesize + boreratio + stroke + peakrpm +enginetypeohc + 
                enginetypeohcv + enginetyperotor + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystemspdi, data = trainingData)
vif(model_3)
summary(model_3)

model_4 <- lm(price ~ aspiration + enginelocation + carheight + enginesize + boreratio + stroke + peakrpm +enginetypeohc + enginetypeohcv + enginetyperotor + carbodyhardtop +  fuelsystemspdi, data = trainingData)
vif(model_4)
summary(model_4)

model_5  <- lm(price ~ aspiration + enginelocation + carheight + enginesize + boreratio + stroke + peakrpm + enginetyperotor + carbodyhardtop +  fuelsystemspdi, data = trainingData)
summary(model_5)
vif(model_5)
model_6  <- lm(price ~ aspiration + enginelocation + carheight + enginesize + stroke + peakrpm + enginetyperotor + carbodyhardtop +  fuelsystemspdi, data = trainingData) 
vif(model_6)
summary(model_6)
Predict_1 <- predict(model_6,test[,-21])
test$test_Price <- Predict_1

r <- cor(test$price,test$test_Price)
rsquared <- cor(test$price,test$test_Price)^2

rsquared
## Conclusion !!
## Adjusted R Square is .85 and R square value comes out to be .90 