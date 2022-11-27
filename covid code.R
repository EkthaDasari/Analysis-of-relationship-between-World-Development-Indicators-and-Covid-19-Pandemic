library(dsEssex)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(MASS)
library(factoextra)
library(gridExtra)
library(nnet)
library(caret)

fd<-read.csv("project_data.csv", header = TRUE) #reading data from csv stored in working directory
names(fd)[1]<- "Country.Name" # assigning new name to i..Country.Name
fd <- fd[-186,] #removing last column of NAs from table
#manipulating data- replacing empty and NAs data
fd$Continent[fd$Country.Name=="Solomon Islands"]<-"Australia/Oceania" #assigning continent name which is empty previosuly
fd$Covid.deaths<-as.numeric(gsub(",","",fd$Covid.deaths)) #converting type of Covid.deaths to numeric
fd$Comp.education<-as.numeric(gsub(",","",fd$Comp.education)) #converting type of Comp.education to numeric
#replacing NAs
fd$Life.expec[is.na(fd$Life.expec)] = mean(fd$Life.expec, na.rm=TRUE)
fd$Elect.access[is.na(fd$Elect.access)] = mean(fd$Elect.access, na.rm=TRUE)
fd$Net.nat.income[is.na(fd$Net.nat.income)] = mean(fd$Net.nat.income, na.rm=TRUE)
fd$Net.nat.income.capita[is.na(fd$Net.nat.income.capita)] = mean(fd$Net.nat.income.capita, na.rm=TRUE)
fd$Mortality.rate[is.na(fd$Mortality.rate)] = mean(fd$Mortality.rate, na.rm=TRUE)
fd$Primary[is.na(fd$Primary)] = mean(fd$Primary, na.rm=TRUE)
fd$Pop.growth[is.na(fd$Pop.growth)] = mean(fd$Pop.growth, na.rm=TRUE)
fd$Pop.density[is.na(fd$Pop.density)] = mean(fd$Pop.density, na.rm=TRUE)
fd$Pop.total[is.na(fd$Pop.total)] = mean(fd$Pop.total, na.rm=TRUE)
fd$Health.exp.capita[is.na(fd$Health.exp.capita)] = mean(fd$Health.exp.capita, na.rm=TRUE)
fd$Health.exp[is.na(fd$Health.exp)] = mean(fd$Health.exp, na.rm=TRUE)
fd$Unemployment[is.na(fd$Unemployment)] = mean(fd$Unemployment, na.rm=TRUE)
fd$GDP.growth[is.na(fd$GDP.growth)] = mean(fd$GDP.growth, na.rm=TRUE)
fd$GDP.capita[is.na(fd$GDP.capita)] = mean(fd$GDP.capita, na.rm=TRUE)
fd$Birth.rate[is.na(fd$Birth.rate)] = mean(fd$Birth.rate, na.rm=TRUE)
fd$Water.services[is.na(fd$Water.services)] = mean(fd$Water.services, na.rm=TRUE)
fd$Comp.education[is.na(fd$Comp.education)] = mean(fd$Comp.education, na.rm=TRUE)

#Q1:
summary(fd) #gives the summary of dataframe
fd%>% ggplot(aes(x=`Covid.deaths`, y=`Health.exp`, colour=`Continent`))+geom_point() #plotting of covid deaths to current health expenditure with respect to continents
fd%>% ggplot(aes(`Covid.deaths`, `Continent`, colour=`Continent`)) +geom_point() #plotting of Life expectancy and birth rate with respective to continents

#Q2: Clustering

fd_nochar<- fd%>% dplyr:: select(-c(Continent, Covid.deaths))#creating a new dataframe from fd by removing columns Continent and Covid.deaths
fd_nochar$Country.Name<- as.numeric(as.factor(fd$Continent))#converting Country.Name to numeric

distance.Euclidean<- get_dist(fd_nochar[-1])# calculating Euclidean distance
distance.corr <- get_dist(fd_nochar, stand = TRUE, method = "pearson") #calculating correlation distance

#k-means clustering
fd_scale<- scale(fd_nochar) #scaling the data for clustering
set.seed(3030)
kmeans2<-kmeans(fd_scale,centers=2,nstart=20) #no.of clusters=2 and initial assignments=20
kmeans3<-kmeans(fd_scale,centers=3,nstart=20)#no.of clusters=3 and initial assignments=20
kmeans4<-kmeans(fd_scale,centers=4,nstart=20)#no.of clusters=4 and initial assignments=20
kmeans7<-kmeans(fd_scale,centers=7,nstart=20)#no.of clusters=7 and initial assignments=20
kmeans7 #printing output of kmeans7
fviz_cluster(kmeans7, geom = "point", data = fd_scale) + ggtitle("k = 7") #plotting of kmeans7 clusters
fviz_nbclust(fd_scale, kmeans, method = "wss")+ #calculating optimal number of clusters
  geom_vline(xintercept = 7, linetype = 2, colour="red")

mdata<-bind_cols(fd$Continent, fd_scale)#binding continent to dataframe
names(mdata)[1]<-"Continent" #updating the column name
mdata<-cbind(cluster=as.integer(kmeans7$cluster), mdata) #adding clusters to dataframe
mdata%>%ggplot(aes(`Continent`,`cluster`))+geom_point() #plotting of continents and clusters


#Q3:

set.seed(2456)
fd_copy<-fd %>% dplyr::select(-c(Continent)) #removing continent from dataframe
fd_copy$Country.Name<- as.numeric(as.factor(fd$Continent)) #converting country name to numeric
fd_copy$Covid.deaths<-ifelse(fd_copy$Covid.deaths>mean(fd_copy$Covid.deaths), 1, 0) #converting covid.deaths into binary variable
fd_copy$Covid.deaths<- as.factor(fd_copy$Covid.deaths)#factoring covid.deaths
train <-((fd$Mortality.rate)<15) #assigning train criteria
fd_main<-fd_copy[!train, ] #test data without train data
Covid.Rest<-fd_copy$Covid.deaths[!train] #taking covid deaths test data which are not train data
glm.fits<-glm(Covid.deaths~., data = fd_copy, family = binomial, subset=train) #binomial logistic regression
summary(glm.fits) #summary of binomial logistic regression

glm.probs<-predict(glm.fits, fd_main, type="response") #calculating predictions
glm.pred<-rep(0, 89) #creating new variable with 89 0s
glm.pred[glm.probs>0.5]=1 #assigning predictions as 1 for which probability is greater than 0.5
table(glm.pred, Covid.Rest) #getting the predictions vs actual test data table
mean(glm.pred==Covid.Rest) #accuracy of the model

#Q4:
fd_categ<-fd
#categorising covid deaths into 4
fd_categ$Covid_deaths<-case_when(fd_categ$Covid.deaths<=100~ "Very Low", fd_categ$Covid.deaths>100 & fd_categ$Covid.deaths<=800 ~ "Low", fd_categ$Covid.deaths>800 & fd_categ$Covid.deaths<=1800~"Medium", fd$Covid.deaths>1800 ~"High")
fd_categ$Covid_deaths<-as.factor(fd_categ$Covid_deaths) #factorizing Covid deaths
fd_categ<-fd_categ %>% dplyr::select(-c(Covid.deaths, Continent, Country.Name)) #removing continent and covid deaths as covid deaths are saved under Covid_deaths after categorizing
fd_categ$Country.Name<- as.numeric(as.factor(fd$Continent)) #converting county name to numeric
set.seed(5050)
training.samples <- fd_categ$Covid_deaths %>% 
  createDataPartition(p = 0.8, list = FALSE) # taking samples from data set for train data
train.data  <- fd_categ[training.samples, ] #creating new dataframe for train data
test.data <- fd_categ[-training.samples, ] #creating new dataframe for test data
model <- multinom(Covid_deaths ~., data = train.data) #performing multinomial logistic regression
summary(model)# Summarize the model
predicted.classes <- model %>% predict(test.data) # Make predictions
table(predicted.classes) #getting the predictions vs actual test data table
mean(predicted.classes == test.data$Covid_deaths) #calculating accuracy

#LDA
set.seed(2013)
lda.fit<-lda(Covid_deaths~., data = train.data) #calculating LDA method
lda.fit #printing the output
lda.pred<-predict(lda.fit, test.data) #make predictions
mean(lda.pred$class==test.data$Covid_deaths) #calculating accuracy

#QDA
set.seed(5230)

qda.fit<-qda(Covid_deaths~., data = train.data)#calculating QDA method
qda.fit#printing the output
qda.pred<-predict(qda.fit, test.data) #make predictions
mean(qda.pred$class==test.data$Covid_deaths) #calculating accuracy
?trainControl

#cross validation with lda method
trControl<-trainControl(method = "cv", number=5) #getting control parameters for train
set.seed(410)
lda.fit <- train(Covid_deaths~.,
                 method = "lda",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = fd_categ) #calculating LDA using train function
lda.pred<-predict(lda.fit, fd_categ) #making predictions
table(lda.pred, fd_categ$Covid_deaths) #getting the predictions vs actual test data table
cmatrix<-confusionMatrix(lda.fit) #creating confusion matrix
cmatrix #printing confusion matrix
set.seed(2031)
qda.fit <- train(Covid_deaths~.,
                 method = "qda",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = fd_categ)#calculating QDA using train function
qda.pred<-predict(qda.fit, fd_categ) #making predictions
table(qda.pred, fd_categ$Covid_deaths)#getting the predictions vs actual test data table
cmatrix<-confusionMatrix(qda.fit)#creating confusion matrix
cmatrix #printing confusion matrix
set.seed(2011)
mlm.fit <- train(Covid_deaths~.,
                 method = "multinom",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = fd_categ)#calculating Multinom using train function


mlm.pred<-predict(mlm.fit, fd_categ)#making predictions

table(mlm.pred, fd_categ$Covid_deaths) #getting the predictions vs actual test data table
cmatrix<-confusionMatrix(mlm.fit) #creating confusion matrix
cmatrix #printing confusion matrix
fd$Country.Name<-as.factor(fd$Continent)
fd_categ%>%ggplot(aes(bin=100,`GDP.capita`, colour=`Covid_deaths`, fill=`Country.Name` ))+geom_histogram()
#Q5
fd_categ%>%ggplot(aes(`GDP.capita`, `Covid_deaths`, colour=`Country.Name`))+geom_boxplot()

#writing console output to notepad
my_log <- file("my_log.txt")
sink(my_log, append = TRUE, type = "output")
sink(my_log, append = TRUE, type = "message")
cat(readChar(rstudioapi::getSourceEditorContext()$path, # Writing currently opened R script to file
             file.info(rstudioapi::getSourceEditorContext()$path)$size))


closeAllConnections()