require(rms)
library(rms)
library(caret)
require(dplyr)
contact_data;contacts_with_volunteer;hired_contacts;hired_contacts_with_volunteers;
#hired with volunteering
nrow(subset(hired_contacts,volunteer_c==1)) #668
nrow(subset(hired_contacts,volunteer_c==0)) #6706
table(contact_data$volunteer_c,contact_data$Blue_c)
#Does working with a volunteer raise the probability of getting hired?

trainIndex <- createDataPartition(contact_data1$Blue_c, p = 0.70, 
                                  list = FALSE, 
                                  times = 1)

contact.data.training = contact_data1[trainIndex,]
contact.data.testing = contact_data1[-trainIndex,]
dim(contact.data.training);dim(contact.data.testing)

glm.fit<-glm(Blue_c~volunteer_c,family=binomial, data = contact_data,na.action = na.omit)
glm.fit<-glm(Blue_c~volunteer_c,family=binomial, data = contact.data.training,na.action = na.omit)
summary(glm.fit)

mod1b <- lrm(contact.data.training$Blue_c~contact.data.training$volunteer_c,na.action = na.omit)
nullmod <- glm(Blue_c~1, family="binomial", data = contact.data.training,na.action = na.omit)
1-logLik(glm.fit)/logLik(nullmod)

require(MASS)
step <- stepAIC(glm.fit, direction="both")
step$coefficients

#2-sample test for equality of proportions
prop.test(table(contact_data$volunteer_c,contact_data$Blue_c))
x1 <- 6940 
x2 <- 426 
n1 <- 44417 
n2 <- 842
p1 <- x1 / n1 #without volunteer
p2 <- x2 / n2 #with volunteer
ppooled <- (x1+x2) / (n1+n2) 
# Calculate test statistic
zdata <- (p1-p2) / sqrt(ppooled*(1-ppooled)*((1/n1)+(1/n2))) 
# Find the p-value 
pvalue <- 2*pnorm(abs(zdata), lower.tail =FALSE)
pvalue
prop.test(c(6921,424),c(44396,840))


#Does working with a volunteer raise the probability of getting hired SOONER?
trainIndex1 <- createDataPartition(hired_contacts1$volunteer_c, p = 0.70, 
                                   list = FALSE, 
                                   times = 1)
hired.contacts.training = hired_contacts1[trainIndex1,]
hired.contacts.testing = hired_contacts1[-trainIndex1,]
dim(hired.contacts.training);dim(hired.contacts.testing)

lm.out <- lm(hired_contacts$days_to_get_hired~hired_contacts$volunteer_c, na.action = na.omit)
lm.out <- lm(hired.contacts.training$days_to_get_hired~hired.contacts.training$volunteer_c, na.action = na.omit)
summary(lm.out)
summary(lm.out)$coefficients[,4] 
summary(lm.out)

#two-sample t-test for means
v1 <- subset(hired_contacts,volunteer_c==1)
date_diff_with_volunteer <- na.omit(v1$days_to_get_hired)
v2 <- subset(hired_contacts,volunteer_c==0)
date_diff_without_volunteer <-na.omit(v2$days_to_get_hired)
mean(date_diff_with_volunteer);mean(date_diff_without_volunteer)
var.test(date_diff_with_volunteer, date_diff_without_volunteer)
t.test(date_diff_with_volunteer, date_diff_without_volunteer, var.equal=FALSE, paired=FALSE) #p-value < 2.2e-16


#Are there certain volunteer activities that are more effective than others?
contacts_with_volunteer <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/contacts_with_volunteer.csv", header=T, na.strings=c("","NA"))

t <- table(hired_contacts_with_volunteers$Volunteer_Services__c)
t #Mock Interview
glm.hired.service <- glm(contacts_with_volunteer$Blue_c~contacts_with_volunteer$Volunteer_Services__c,na.action = na.omit)
glm.hired.service <- glm(Blue_c~Volunteer_Services__c,data=contacts_with_volunteer,family = "binomial",na.action = na.omit)
summary(glm.hired.service)

lm.hiredsooner.service <- lm(hired_contacts_with_volunteers1$days_to_get_hired~hired_contacts_with_volunteers1$Volunteer_Services__c,na.action = na.omit)
lm.hiredsooner.service.step <- step(lm(hired_contacts_with_volunteers$days_to_get_hired~hired_contacts_with_volunteers$Volunteer_Services__c,na.action = na.omit)
                                    ,direction="both") #error
summary(lm.hiredsooner.service)
summary(lm.hiredsooner.service)$coefficients[,4] 
nullmod <- glm(Blue_c~1, family="binomial", data = contacts_with_volunteer,na.action = na.omit)

#Do volunteer activities increase the quality job obtained?
hired_contacts <- read.csv("C:/Users/saipr/Desktop/Sai Priya/ABA/Salesforce Data/Question4/hired_contacts.csv", header=T, na.strings=c("","NA"))

t1 <- table(hired_contacts$Salary_Range__c,hired_contacts$volunteer_c)

as.numeric(substr(hired_contacts$Salary_Range__c,2,7))

hired_contacts$x <- substr(hired_contacts$Salary_Range__c,2,8)
hired_contacts$y <- substr(hired_contacts$Salary_Range__c,12,17)
hired_contacts$x <- gsub(",","",hired_contacts$x)
hired_contacts$y <- gsub(",","",hired_contacts$y)

hired_contacts$x<-as.numeric(hired_contacts$x)
hired_contacts$y<-as.numeric(hired_contacts$y)
typeof(hired_contacts$y)

View

hired_contacts$avg_salary <- (hired_contacts$x + hired_contacts$y)/2
index <- hired_contacts$x == 1e+05
hired_contacts$avg_salary[index] <- 1e+05
index1 <- hired_contacts$x == 20000 & is.na(hired_contacts$y)
hired_contacts$avg_salary[index1] <- 20000
View(hired_contacts)

s1 <- subset(hired_contacts,volunteer_c==1)
salary_with_volunteer <- na.omit(s1$avg_salary)
s2 <- subset(hired_contacts,volunteer_c==0)
salary_without_volunteer <- na.omit(s2$avg_salary)
mean(salary_with_volunteer)
mean(salary_without_volunteer)
t.test(salary_with_volunteer, salary_without_volunteer, var.equal=FALSE, paired=FALSE) #p-value =0.0008235
trainIndex2<- createDataPartition(hired_contacts1$volunteer_c, p = 0.70, 
                                  list = FALSE, 
                                  times = 1)
hired.contacts.training = hired_contacts1[trainIndex2,]
hired.contacts.testing = hired_contacts1[-trainIndex2,]
dim(hired.contacts.training);dim(hired.contacts.testing)
lm.salary <- lm(hired_contacts$avg_salary~hired_contacts$volunteer_c,na.action = na.omit)
lm.salary <- lm(hired.contacts.training$avg_salary~hired.contacts.training$volunteer_c,na.action = na.omit)
summary(lm.salary)
summary(lm.salary)$coefficients[,4] 
