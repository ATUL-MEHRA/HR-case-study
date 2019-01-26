#Let us load the libraries

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(corrplot)


#Reading the data from different files
data_gen<-read.csv("general_data.csv",stringsAsFactors = F)
data_emp<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
data_mgr<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

#Data Cleaning.....

data_gen<-data_gen[,-8] #REmove employee count it is same and it is not really relevant
data_gen<-data_gen[,-15] #Remove above 18 variable as it is same 


#total of 28 NA's in general data .
sum(is.na(data_gen))
#19 in Num Companies worked
sum(is.na(data_gen$NumCompaniesWorked))
#and 9 in Total working years
sum(is.na(data_gen$TotalWorkingYears))

# Since No. of NA are less than 10%, we can remove these NA 

data_gen<-data_gen[complete.cases(data_gen),]
str(data_gen)

sum(is.na(data_emp))
# Total 83 NA
sum(is.na(data_emp$EnvironmentSatisfaction))
#25 NA
sum(is.na(data_emp$JobSatisfaction))
#20 NA
sum(is.na(data_emp$WorkLifeBalance))
#38 NA
# Since No. of NA are less than 10%, we can remove these NA
data_emp<-data_emp[complete.cases(data_emp),]

sum(is.na(data_mgr))
# 0 NA
sum(is.na(in_time))
# 109080 NA
sum(is.na(out_time))
# 109080 NA

# Need not to remove NA from in_time & out_time, since we can obtain difference between two without
# affecting Result. We would ignore NA while calculating Average working hours for each employee.

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


###EDA##############################################################################

plot_grid(ggplot(data_gen, aes(x=Gender,fill=Attrition))+ geom_bar(), 
          ggplot(data_gen, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# reveals strong contrast for attrition wrt Department, job role, job level.
# Strong possibility if the job level is less the attrition is more and in R&D department

plot_grid(ggplot(data_gen, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=factor(TotalWorkingYears),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

# reveals strong contrast for attrition wrt TotalWorkingYears,marital status, numberofcompanies
#More attrtion in lesser working years and in single marital status.

plot_grid(ggplot(data_gen, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(data_gen, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar()+bar_theme1,
                    align = "h") 

#reveals strong contrast for attrition wrt YearsAtCompany
#More attrition in people who has spent less years at company and with the current manager and promoted long back


# For Numerical Variable #

Age_plot <- ggplot(data_gen,aes(x=Attrition,y=Age,fill=Attrition))+geom_boxplot()
#People who are aged have lesser possibility of quitting.....

Distancehome_plot <- ggplot(data_gen,aes(x=Attrition,y=DistanceFromHome,fill=Attrition))+geom_boxplot()
#Not much impact of distance on the attrition

Education_plot <- ggplot(data_gen,aes(x=Attrition,y=Education,fill=Attrition))+geom_boxplot()
#Education does not seems to impact much

grid.arrange(Age_plot,Distancehome_plot,Education_plot)

Yearpromo_plot <- ggplot(data_gen,aes(x=Attrition,y=YearsSinceLastPromotion,fill=Attrition))+geom_boxplot()

Yearmanager_plot <- ggplot(data_gen,aes(x=Attrition,y=YearsWithCurrManager,fill=Attrition))+geom_boxplot()

grid.arrange(Yearpromo_plot,Yearmanager_plot)

#it looks like years with current manager helps reduce attrition.

################EDA############################################

#Derived Metrics....

# Obtaining Derived Matrics out of out_time & in_time.

work_Hours <- data.frame(sapply(2:262, function(i) difftime(time1 = out_time[,i], time2 = in_time[,i], units = "hours")))
work_Hours1 <- work_Hours


EmployeeID <- c(1:4410)


work_Hours1 <- sapply(work_Hours1, as.numeric)

typeof(work_Hours1)

Avg_work_Hour <- data.frame(sapply(1:4410, function(i) (mean(work_Hours1[i,], na.rm = T))))

Avg_work_Hour <- cbind(Avg_work_Hour,EmployeeID)

names(Avg_work_Hour)[1] <- "Average Hours"

Avg_work_Hour$`Average Hours` <- scale(Avg_work_Hour$`Average Hours`)

####Data preparation for model ###############################

# leveling to (1,0) for two level categories.

#for Gender
data_gen$Gender<-as.factor(data_gen$Gender) # with 2 levels
levels(data_gen$Gender)<-c(1,0)
data_gen$Gender <- as.numeric(levels(data_gen$Gender))[data_gen$Gender]

#For Attrition
data_gen$Attrition<-as.factor(data_gen$Attrition) # with 2 levels
levels(data_gen$Attrition)<-c(0,1)
data_gen$Attrition <- as.numeric(levels(data_gen$Attrition))[data_gen$Attrition]


data_gen$BusinessTravel<-as.factor(data_gen$BusinessTravel)

# Attrition rate

attr <- sum(data_gen$Attrition)/nrow(data_gen)

# 16% - Attrition rate

# Create the dummy variable for Business travel
dummy_businesstravel <- data.frame(model.matrix( ~BusinessTravel, data = data_gen))
dummy_businesstravel <- dummy_businesstravel[,-1]
data_gen<-cbind(data_gen[,-3],dummy_businesstravel)

data_gen$Department<-as.factor(data_gen$Department)
# Create the dummy variable for Department
dummy_Department <- data.frame(model.matrix( ~Department, data = data_gen))
dummy_Department <- dummy_Department[,-1]
data_gen<-cbind(data_gen[,-3],dummy_Department)


data_gen$EducationField<-as.factor(data_gen$EducationField)
# Create the dummy variable for Education Field
dummy_EducationField <- data.frame(model.matrix( ~EducationField, data = data_gen))
dummy_EducationField <- dummy_EducationField[,-1]
data_gen<-cbind(data_gen[,-5],dummy_EducationField)

data_gen$JobRole<-as.factor(data_gen$JobRole)
# Create the dummy variable for Job Role
dummy_JobRole <- data.frame(model.matrix( ~JobRole, data = data_gen))
dummy_JobRole <- dummy_JobRole[,-1]
data_gen<-cbind(data_gen[,-8],dummy_JobRole)


data_gen$MaritalStatus<-as.factor(data_gen$MaritalStatus)
# Create the dummy variable for Marital status
dummy_MaritalStatus <- data.frame(model.matrix( ~MaritalStatus, data = data_gen))
dummy_MaritalStatus <- dummy_MaritalStatus[,-1]
data_gen<-cbind(data_gen[,-8],dummy_MaritalStatus)

#Now all data in data_gen is numeric, before merging data_emp, let's count NA's and remove them
sum(is.na(data_emp))
sum(is.na(data_gen))

######## Outlier Treatment ###############

# ----- MonthlyIncome - Outlier Treatment ------#

# Lets see a quantile & boxplot of net monthly income #

Monthincome_Plot <- ggplot(data_gen) +
  geom_boxplot(aes(x=1,y=MonthlyIncome)) + scale_y_continuous(breaks=seq(0,200000,10000)) +
  labs(title="MonthlyIncome",x="MonthlyIncome",y="No of Employees")


quantile(data_gen$MonthlyIncome,seq(0,1,0.01))

Monthincome_Plotq <- ggplot() + geom_line(aes(x=c(0:100),y=quantile(data_gen$MonthlyIncome,seq(0,1,0.01)))) + 
  scale_y_continuous(breaks=seq(0,200000,10000)) +
  labs(title=" MonthlyIncome Quantile Rise",x="Quantile",y="MonthlyIncome")


# Lets cap the monthly income to 165000 as seen in boxplot and  quantile .

data_gen[which(data_gen$MonthlyIncome > 165000),'MonthlyIncome'] <- 165000

# --------- TotalWorkingYears - Outlier Treatment ----------#

# Boxplot
Totalworkyear_plot <- ggplot(data_gen) + 
  geom_boxplot(aes(x=1,y=TotalWorkingYears)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title="TotalWorkingYears",x="TotalWorkingYears",y="No of Employees")


# Quantile #

quantile(data_gen$TotalWorkingYears,seq(0,1,0.01))
Totalworkyear_plotq <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(data_gen$TotalWorkingYears,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,7)) +
  scale_y_continuous(breaks=seq(0,40,2)) + 
  labs(title="TotalWorkingYears quantile wise increase",x="quantile",y="TotalWorkingYears")


# cap the Totalworkingyears to 27 as seen in boxplot and  quantile .

data_gen[which(data_gen$TotalWorkingYears > 27),'TotalWorkingYears'] <- 27

# ----------YearsAtCompany - Outlier Treatment------------ #

# Lets check a net boxplot for this
Yearatcomp_plot <- ggplot(data_gen) + 
  geom_boxplot(aes(x=1,y=YearsAtCompany)) +
  scale_y_continuous(breaks=seq(0,40,5)) +
  labs(title="YearsAtCompany",x="YearsAtCompany",y="No of Employees")


# Lets see the quantile #
quantile(data_gen$YearsAtCompany,seq(0,1,0.01))
Yearatcomp_plotq <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(data_gen$YearsAtCompany,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,7)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="YearsAtCompany",x="quantile",y="YearsAtCompany")


# cap the YearsAtCompany to 17 as seen in boxplot and  quantile .

data_gen[which(data_gen$YearsAtCompany > 17),'YearsAtCompany'] <- 17

##length(which(data_gen$YearsAtCompany>17))

# --------- YearsSinceLastPromotion - outlier treatment ----------#

#  Boxplot for this
Yearslastpromo_plot <- ggplot(data_gen) + 
  geom_boxplot(aes(x=1,y=YearsSinceLastPromotion)) +
  scale_y_continuous(breaks=seq(0,15,1)) +
  labs(title="YearsSinceLastPromotion",x="YearsSinceLastPromotion",y="No of Employees")


#  Quantile #
quantile(data_gen$YearsSinceLastPromotion,seq(0,1,0.01))
Yearslastpromo_plotq<- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(data_gen$YearsSinceLastPromotion,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,7)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="YearsSinceLastPromotion quantile wise increase",x="quantile",y="YearsSinceLastPromotion")


# Cap the YearsSinceLastPromotion to 9 as seen in boxplot and  quantile .

data_gen[which(data_gen$YearsSinceLastPromotion > 9),'YearsSinceLastPromotion'] <- 9

# --------- YearsWithCurrManager - outlier treatment ----------#

#  Boxplot 
Yearcurrentmanager_plot <- ggplot(data_gen) + 
  geom_boxplot(aes(x=1,y=YearsWithCurrManager)) +
  scale_y_continuous(breaks=seq(0,20,1)) +
  labs(title="YearsWithCurrManager",x="YearsWithCurrManager",y="No of Employees")


#  Quantile #
quantile(data_gen$YearsWithCurrManager,seq(0,1,0.01))
Yearcurrentmanager_plotq <- ggplot() + 
  geom_line(aes(x=c(0:100),y=quantile(data_gen$YearsWithCurrManager,seq(0,1,0.01)))) +
  scale_x_continuous(breaks=seq(0,100,7)) +
  scale_y_continuous(breaks=seq(0,40,2)) +
  labs(title="YearsWithCurrentManager quantile wise increase",x="quantile",y="YearsWithCurrManager")


# cap the YearsWithCurrManager to 13 as seen in boxplot and  quantile .

data_gen[which(data_gen$YearsWithCurrManager > 13),'YearsSinceLastPromotion'] <- 13

master_data<-merge(data_emp,data_gen,by = "EmployeeID")
sum(is.na(master_data))


master_data<-merge(master_data,data_mgr,by = "EmployeeID")
sum(is.na(master_data))

master_data <- merge(master_data,Avg_work_Hour, by = "EmployeeID")

str(master_data)

plot_grid(ggplot(master_data, aes(x=EnvironmentSatisfaction,fill=factor(Attrition)))+ geom_bar(), 
          ggplot(master_data, aes(x=JobSatisfaction,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=WorkLifeBalance,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=JobInvolvement,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(master_data, aes(x=factor(PerformanceRating),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          
          align = "h") 

#Better performance rating, better work life balance better job involvement keeps employees not looking for change.

master_data<-master_data[,-1]

master_data<-master_data[,-13] #Remove working hours as it does not change at all for any data point

#Now all data in numeric and we need to scale the continuous ones.

master_data$Age<-scale(master_data$Age)

master_data[,1:3]<-sapply(master_data[,1:3],scale)
master_data[,6:7]<-sapply(master_data[,6:7],scale)
master_data[,9:18]<-sapply(master_data[,9:18],scale)
master_data[,38:39]<-sapply(master_data[,38:39],scale)


#####Building the model.....

# First let us split dataset in train & test data.

set.seed(100)

train_data_indices<-sample.split(master_data$Attrition,SplitRatio = 0.7)

train_data<-master_data[train_data_indices,]
test_data<-master_data[!train_data_indices,]

sum(is.na(train_data))
model_1<-glm(data = train_data,formula=Attrition~.,family = "binomial")

summary(model_1)

model_2<-stepAIC(model_1,direction = "both")
summary(model_2)
sort(vif(model_2))

#Remove Joblevel as it is lesser significant

model_3<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + JobRoleHuman.Resources + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_3)
sort(vif(model_3))

#Remove StockOptionLevel as  less significance

model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + JobRoleHuman.Resources + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_4)
sort(vif(model_4))

#Now let's remove  JobRoleHuman.Resources as it is less significant

model_5<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_5)
sort(vif(model_5))

#Let's remove insignificant MaritalStatusMarried

model_6<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive  + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_6)
sort(vif(model_6))

#Now, let's remove EducationFieldLife.Sciences  as it is high VIF and lesser significant as 
#compared to medical and both are correlated

cor(train_data$EducationFieldLife.Sciences,train_data$EducationFieldMedical)


model_7<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age + NumCompaniesWorked +  
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive  + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_7)
sort(vif(model_7))


#Based on less significance, removing EducationFieldMarketing

model_8<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age  + NumCompaniesWorked  + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely  + 
                EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree  + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive  + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_8)
sort(vif(model_8))

#Based on insignificance removing Medical, other and technical degree
model_9<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Age  + NumCompaniesWorked  + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely  + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
               JobRoleSales.Executive  + MaritalStatusSingle + 
               `Average Hours`, family = "binomial", data = train_data)
summary(model_9)
sort(vif(model_9))

cor (train_data$BusinessTravelTravel_Frequently,train_data$BusinessTravelTravel_Rarely)
#Based on the high CIF and higher correlation, removing the lesser significant BusinessTravelTravel_Rarely

model_10<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Age  + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravelTravel_Frequently + 
                JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive  + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_10)
sort(vif(model_10))



cor(train_data$YearsSinceLastPromotion,train_data$YearsWithCurrManager)

# Both are correlated with high CIF, removing the lesser significant one of the two which is YEars with curr manager

model_11<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Age  + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive  + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_11)
sort(vif(model_11))

# removing lesser significant JobRoleManager

model_12<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Age  + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                JobRoleSales.Executive  + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_12)
sort(vif(model_12))


#Removing the less significant one, JobRoleSalesExecutive

model_13<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Age  + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + JobRoleResearch.Director + 
                MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_13)
sort(vif(model_13))

#Removing the less significant one, JobRoleResearchdirector

model_14<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Age  + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_14)
sort(vif(model_14))

cor(train_data$Age,train_data$TotalWorkingYears)

#Based on high correlation and high CIF, removing lesser significant one, ie., Age

model_15<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + NumCompaniesWorked  + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_15)
sort(vif(model_15))




#Removing the less significant one, Training times last year

model_16<-glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + NumCompaniesWorked  + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + MaritalStatusSingle + 
                `Average Hours`, family = "binomial", data = train_data)
summary(model_16)
sort(vif(model_16))



cor(train_data$TotalWorkingYears,train_data$YearsSinceLastPromotion)

#Very less correlation, now all CIF is around 1 and all 10 variables are significant with 3 stars

#Predicting the price for the test data 
test_pred = predict(model_16, type = "response", newdata = test_data[,-5])

test_data$prob <- test_pred

# lets use probability cutoff = 0.5

test_pred_Attr <- factor(ifelse(test_pred>= 0.5, "yes", "No"))

typeof(test_pred_Attr)

test_actual_attr <- factor(ifelse(test_data$Attrition == 1, "yes","No"))


table(test_actual_attr,test_pred_Attr)

test_conf <- confusionMatrix(test_pred_Attr,test_actual_attr, positive = "yes")
test_conf

#Sensitivity is around 22 % for 0.5 probability cut off
#Sensitivity is around 38 % for 0.4 probability cut off
#Sensitivity is around 49% for 0.3 probability cut off
#Sensitivity is around 68% for 0.2 probability cut off

#Specificity is around 98%
#Accuracy is around 86%

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr<- factor(ifelse(test_pred >= cutoff, "yes", "No"))
  conf <- confusionMatrix(predicted_attr,test_actual_attr, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff
# Let's choose a cutoff value of 0.1696 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.1696, "yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#Now with the cut off at 0.16 accuracy, sensitivity and specificity is all around 74%
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.4940269

# we obtained KS statistic as 49% which is greater than 40%. Thus we can say it as a good model.

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

Attr_decile = lift(test_actual_attr, test_pred, groups = 10)
Attr_decile

# from the gain chart we got to know that 70% of attrition can be found at decile 3rd. 
# By lift we get the factor by which our model outperform the random model. thus at 3rd decile,
# lift is 2.3

# Thus if we have resourses to survey 30% of population, by our model we could get 70% of attrition.
