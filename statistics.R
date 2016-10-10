# created: Oct 9 2016
# @author Alexandra Ugolnikova
#
#
# Dataiku test as part of the interview process
#
# Census Income dataset consists of around 300,000 samples with 42 variables
# with different information about ages, income, education etc.
# The goal is to predict whether an individual has more or less than $50k savings per year
# ("savingYear" variable)
#
#
#
#

#--------------Step 0

#
# packages that will be used for the analysis (previously installed or updated)
library("ggplot2")
library("dplyr")

#------------- Step 1
#--------------loading data

# setting working directory with setwd + path 
myData = read.csv("census_income_learn.csv")
#adjusting columns' names
# names.csv file is attached, contains names of attributes
myNames = read.csv("names.csv")
names(myData) = myNames[,1]

#summary(myData)
# Changing lsome evel names for simplicity of use
levels(myData$savingsYear) = c("50k-", "50k+")
levels(myData$sex) = c("F", "M")
levels(myData$race) = c("AmIndian", "Asian", "Black", "Other", "White")
levels(myData$citizenship) = c("Foreign", "USbyNatural", "Native(bornAbr)", "Native(bornPRico)", "Native(bornUS)")



#-----------checking for missing values 
sum(myData == " ?")/(nrow(myData)*42) #0.04
nrow(myData[myData$migrationCodeMsa == " ?",]) # interesting, seems like we found some missing values
# we will recheck more later

#-----------delete outliers 
# Just do it manually for a couple of variables that stand out
names(myData)
outlierCheck = function(atr, value){
  nrow(myData[atr > value, ])/nrow(myData)  
}

outlierCheck(myData$capitalGains, 30000)
outlierCheck(myData$capitalGains -myData$capitalLosses + myData$dividendsStock,20000)

myData = myData[which(myData$capitalGains <=30000) ,]
myData = myData[which(myData$capitalGains -myData$capitalLosses + myData$dividendsStock <= 20000) ,]



#-------------Step 2
#-------------Statistics on the last column "savingsYear" 

# Percentage of people with more than 50k savings:
#  In total: 6,2%
nrow(myData[myData$savingsYear  == "50k+",])/nrow(myData)
#  In 1994: 5,8 %
myData94 = myData[myData$year == 94,]
nrow(myData94[myData94$savingsYear  == "50k+",])/nrow(myData94)
#  In 1995: 6,5 %
myData95 = myData[myData$year == 95,]
nrow(myData95[myData95$savingsYear  == "50k+",])/nrow(myData95)

# Subsetting on savingsYear = 50k+ 
myData50k = myData[myData$savingsYear == "50k+",]
male = myData50k$sex  == "M"
white = myData50k$race  == "White"
senior = myData50k$age >= 60
active = myData50k$age >= 20 & myData50k$age < 60
young = myData50$age < 20

# Percentage of men: 78%
nrow(myData50k[male,])/nrow(myData50k)
# Percentage of white people: 91%
nrow(myData50k[white,])/nrow(myData50k)
# Percentage  of white men: 72% 
nrow(myData50k[white & male ,])/nrow(myData50k)
# Percentage of senior white men: 8%
nrow(myData50k[senior & white & male ,])/nrow(myData50k)
# Percentage of age 20 to 60 (active) white men :63%
nrow(myData50k[active & white & male,])/nrow(myData50k)


#### 
#----------- Step 3
#
#
#  Adding new discrete variables 
#
#-----1 Age group
addAgeGroup = function(age, Nrow) {
  c = c()
  for (i in 1:Nrow) {
    if (age[i] >=60) {
      c[i] = "Senior"
    } else if (age[i] >= 20 & age[i] < 60) {
      c[i] = "Active"
    } else{
      c[i] = "Young"
    }
  }
  return(c)
}
# ageGroup is now the 43th column in our DF
myData$ageGroup = c()
myData$ageGroup = factor(addAgeGroup(myData$age, nrow(myData)))
levels(myData$ageGroup) = c( "Active","Senior", "Young")
#-----2 total gains 

addGains = function(Gains, Nrow) { 
  c = c()
  for (i in 1:Nrow) {
    if( Gains[i]>  50000) {
      c[i] = "Very high"
    } else if (Gains[i] <= 50000 & Gains[i] >15000){
      c[i] = "High"
    } else if (Gains[i] <=25000 & Gains[i] > 5000) {
      c[i] = "Medium"
    } else if (Gains[i] > 0 & Gains[i] <=5000){
      c[i] = "Low"
    } else{
      c[i] = "No gain"
    }
  }
  return(c)
}  

Gains = myData$capitalGains - myData$capitalLosses + myData$dividendsStock
myData$Gains =  c()
myData$Gains = factor(addGains(Gains, nrow(myData)))
levels(myData$Gains) = c("High", "Low", "Medium", "No gain", "Very high")
#str(myData)

#-----3 Weeks Worked in Groups

addWeekGroups = function(weeksWorked, Nrow) { 
  c = c()
  for (i in 1:Nrow) {
    if(weeksWorked[i] == 52) {
      c[i] = "Full year"
    } else if (weeksWorked[i] < 52 & weeksWorked[i] >= 40 ){
      c[i] = "Most of year"
    } else if ( weeksWorked[i] < 40 & weeksWorked[i] > 0) {
      c[i] = "Part of year"
    } else {
      c[i] = "Zero weeks"
    }
  }
  return(c)
}  

myData$weekGroups = c()
myData$weekGroups = factor(addWeekGroups(myData$weeksWorkedYear, nrow(myData)))
levels(myData$weekGroups) = c("Full year", "Most of year", "Part of year", "Zero weeks")



#----------------Step 4
# ----- Basic Geom bar plots
# Plot 1
# 
# Dependencies between savingsYear and categor. variables
# One can run it for different attributes to see dependencies
barPlot = function(catAttr, position = position_dodge()){
  ggplot(data = myData, aes(x = catAttr, fill = savingsYear)) +
    geom_bar(color = "Black", position = position) +
    scale_fill_manual(values=c("Dark Green", "Orange"))
  }

# Age cat
plt_age = barPlot(myData$ageGroup) + xlab("Age categories")
# sex
plt_sex = barPlot(myData$sex) + xlab("Sex")
# Citizenship
plt_cit = barPlot(myData$citizenship) + xlab("Citizenship")
  
nrow(myData[myData$citizenship == "Native(bornUS)",])/nrow(myData) # US born and raised : 88% of samples
nrow(myData50k[myData50k$citizenship == "Native(bornUS)",])/nrow(myData50k) # 90% out of 50k+ group

# Race 
plt_race = barPlot(myData$race) + xlab("Race")

nrow(myData[myData$race == "White",])/nrow(myData) # 83%
nrow(myData50k[myData50k$race == "White",])/nrow(myData50k) # 91%

# EmploymentStatus
plt_empl = barPlot(myData$EmploymentStatus) + xlab("Employment Status")

nrow(myData[myData$EmploymentStatus == " Children or Armed Forces",])/nrow(myData) # 62%
nrow(myData[myData$EmploymentStatus == " Full-time schedules",])/nrow(myData) # 20%

# weekGroups
p1 = barPlot(myData$weekGroups) + xlab("Weeks worked categories")

# Gain
plt_gain = barPlot(myData$Gains) + xlab("Gain category")

library(cowplot)
Plot1 = plot_grid(p1, plt_age, plt_race, plt_sex,plt_cit, plt_empl, ncol = 3, nrow = 2)


#----------Step 5
#---------- Reducing number of attributes for making our predictive model easier
#
#
# Some attributes do not seem to bring any information to the savingYear value
# Some have way too many unknown values
#
# I basically just geom_bar plotted the cathegorical variables
# ex : barPlot(myData$regionPreviousResidence)

columns_to_delete = c("enrolledEducation", "memberLaborUnion", "hispanicOrigin", "majorIndustryCode", 
                      "reasonUnemployment", "regionPreviousResidence", "migrationCodeMsa",
                      "statePreviousResidence", "HouseholdFamilyStatus", "instanceWeight", 
                      "migrationCodeChangeInReg" , 'migrationCodeMoveWithinReg', "countryBirthSelf",
                      "liveInHouseLastYear\t",  "migrationPrevResSunbelt", "familyMembersUnder18",
                      "countryBirthFather",  "countryBirthMother", "fillQuestionnaireVeteran",
                      "weeksWorkedYear", "occupationCode", "IndustryCode")

myData = myData[, !(colnames(myData) %in% columns_to_delete)]


# sum(myData == " ?")/(nrow(myData)*ncol(myData))





# -------Step 6
#
#  Let's make some fancier plots
#
p = ggplot(data = myData)


# Plot 2: 
# Two histograms according to age, color separation F/M for 2 savingsYear groups
#

fS = facet_grid(savingsYear~., scale = "free")

plot2 = p + geom_histogram(binwidth = 1, aes(x = age, fill = sex), color = "black") +
  fS +
  xlab("Age") +
  ylab("Number of people") +
  ggtitle("Age distribution")

# Plot 3:
# plot where we see how a bunch of parameters such as
# race, sex, waheHour and capital influence the amount of savingsYear
plot3 = p+  geom_point(aes(x = age, capitalGains-capitalLosses+ dividendsStock, color = race, 
                   shape = citizenship) )+
  geom_smooth() +
  ylab("Personal gain") +
  facet_grid(savingsYear~weekGroups)








#-------Step 7
#
#-------------training myData using Decision Trees 
#  With rpart package 
library(rpart)

# adding some factors 
# we will want to use random forest

myData$veteransBenefits = factor(myData$veteransBenefits)
myData$ownBusinessOrSelfEmployed = factor(myData$ownBusinessOrSelfEmployed)
myData$numPersonsWorkedForEmployer = factor(myData$numPersonsWorkedForEmployer)
str(myData)

# Let us get rid of some variables 
columns_not_for_tree = c("age", "industryCode", "majorOccupationCode", "occupationCode", "year", "capitalGains", 
                         "capitalLosses", "dividendsStock","wageHour")

myDataTree = myData[, !(colnames(myData) %in% columns_not_for_tree)]
str(myDataTree)
summary(myDataTree)


fitTree = rpart(myDataTree$savingsYear ~.,data = myDataTree, method = "class") 
# Variables used in tree construction: education Gains     sex       taxStatus
printcp(fit) # Root node error: 11593/198579 = 0.05838


# Prediction on fitted data and calculation of misclassification percentage
out = predict(fit)
pred.response = colnames(out)[max.col(out, ties.method = c("random"))] # predict response
mean(myDataTree$savingsYear != pred.response) # misclassification error 5%



#-------------training myData using Random Forest
library(randomForest)

fitForest = randomForest(x = myDataTree[-13], 
                         y = myDataTree$savingsYear, ntree=20)






#------Step 8
#
#---load and adjust test data
# 

myDataTest = read.csv("census_income_test.csv")
names(myDataTest) = myNames[,1]


levels(myDataTest$savingsYear) = c("50k-", "50k+")
levels(myDataTest$sex) = c("F", "M")
levels(myDataTest$race) = c("AmIndian", "Asian", "Black", "Other", "White")
levels(myDataTest$citizenship) = c("Foreign", "USbyNatural", "Native(bornAbr)", "Native(bornPRico)", "Native(bornUS)")

Nrow = nrow(myDataTest)
myDataTest$ageGroup = c()
myDataTest$ageGroup = factor(addAgeGroup(myDataTest$age, Nrow))
levels(myDataTest$ageGroup) = c("Active", "Senior", "Young")

myDataTest$Gains = c()
myDataTest$Gains = factor(addGains(myDataTest$capitalGains - myDataTest$capitalLosses +
                                 myDataTest$dividendsStock, Nrow))
levels(myDataTest$Gains) =  c("High", "Low", "Medium", "No gain", "Very high")

myDataTest$weekGroups = c()
myDataTest$weekGroups = factor(addWeekGroups(myDataTest$weeksWorkedYear, Nrow))
levels(myDataTest$weekGroups) = c("Full year", "Most of year", "Part of year", "Zero weeks")


myDataTest$veteransBenefits = factor(myDataTest$veteransBenefits)
myDataTest$ownBusinessOrSelfEmployed = factor(myDataTest$ownBusinessOrSelfEmployed)
myDataTest$numPersonsWorkedForEmployer = factor(myDataTest$numPersonsWorkedForEmployer)

myDataTest = myDataTest[, !(colnames(myDataTest) %in% columns_to_delete)]

myDataTreeTest = myDataTest[, !(colnames(myDataTest) %in% columns_not_for_tree)]

str(myDataTreeTest)
#
#
#------ Step 9 
#
# testing Decision Trees on myDataTest set

TestTree_y = myDataTreeTest[ ,13]

# predicting the test results
predicted_y = predict(fitTree, myDataTreeTest[-13], type = "class")

# making the confusion matrix
cm = table(myDataTreeTest[ ,13], predicted_y)
cm
# 5,7% of incorrect predictions in total
(cm[2,1] + cm[1,2])/sum(cm[,]) 
# 80% incorrect predictions for the 50k+ class ...
cm[2,1]/sum(cm[2,])
# <1%  incorrect predictions for the 50k- class..
cm[1,2]/sum(cm[1,])
#
# Reasons: 
# There are very few samples of 50k+ class, it is hard to make predictions
# Clearly the constructed tree classifier is not the most efficient way to go
# 
# Would be interesting to plot it but we are not going to do it here
# Would be interesting to try out different formulas too

#
# testing Random Forest on myDataTest set

predicted_y = predict(fitForest, myDataTreeTest[-13])
cm = table(myDataTreeTest[ ,13], predicted_y)
cm
# 5,1% of incorrect predictions in total
(cm[2,1] + cm[1,2])/sum(cm[,]) 
# 70% incorrect predictions for the 50k+ class ...
cm[2,1]/sum(cm[2,])
# <1%  incorrect predictions for the 50k- class..
cm[1,2]/sum(cm[1,])


#----------- Step 10
#
# Let's use SVM  wirh gaussian kernel
#
# check for caTools and e1071 
library(caTools)
library(e1071)

fitSVM = svm(formula = myData$savingsYear~.,
             data = myData, 
             type = "C-classification", 
             kernel = "radial")

predicted_y = predict(fitSVM, myDataTreeTest[-13])
cm = table(myDataTreeTest[ ,13], predicted_y)
cm
# 5,1% of incorrect predictions in total
(cm[2,1] + cm[1,2])/sum(cm[,]) 
# 70% incorrect predictions for the 50k+ class ...
cm[2,1]/sum(cm[2,])
# <1%  incorrect predictions for the 50k- class..
cm[1,2]/sum(cm[1,])



###########
#-------------------------Conclusion
#
#
# There is a lot to investigate in this dataset. It is extremely interesting.
# The analysis of data and cleansing was very time consuming, since, as many of people who
# have never worked with raw data, I am used to clean and nice datasets.
#
# I had to spend most of the time on analyzing and cleaning it according to
# what I found the most logical.
# And then I had no time to really construct some cool predictive models
# and restrained myself to the Tree classifier and Random forest.
#
#

# censorUS_Dataiku
