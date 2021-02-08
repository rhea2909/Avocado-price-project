#intalling packages corresponding to the project
install.packages("GGally")
install.packages("plotly")
install.packages("gridExtra")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("readxl")
install.packages("googledrive")
install.packages("gsheet")
install.packages("cluster")
install.packages('ROCR') 

#loading libraries for the project corresponding to the packages
library(readxl)
library(GGally)
library(plotly)
library(gridExtra)
library(factoextra)
library(tidyverse)
library(googledrive)
library(gsheet)
library(cluster)
library(ROCR)

######################################################################################################################


                                            #K-means Clustering


######################################################################################################################

#importing avocado data for 2015 using the google sheet function
a15 <- gsheet2tbl('https://drive.google.com/file/d/1U55dmuM01gJX5-ANtyho73fHgpWzMha-/view?usp=sharing')
a15 #displaying the output data
df1 <- data.frame(a15) #converting data to dataframe
df1 <- na.omit(df1) #omiting nas from the dataframe
View(df1) #viewing the data in tabular form
#importing avocado data for 2016 using the google sheet function
b16 <- gsheet2tbl('https://drive.google.com/file/d/1DB0b6Mfv-kU4zH5OcaMBt-8a1DOii5Rw/view?usp=sharing')
b16 #displaying the output data
df2 <- data.frame(b16) #converting data to dataframe
df2 <- na.omit(df2) #omiting nas from the dataframe
View(df2) #viewing the data in tabular form
#importing avocado data for 2017 using the google sheet function
c17 <- gsheet2tbl('https://drive.google.com/file/d/1L1BlgIkainsT5QH8rZtrbAiVKkTuUVH1/view?usp=sharing')
c17 #displaying the output data
df3 <- data.frame(c17) #converting data to dataframe
df3 <- na.omit(df3) #omiting nas from the dataframe
View(df3) #viewing the data in tabular form
#importing avocado data for 2018 using the google sheet function
d18 <- gsheet2tbl('https://drive.google.com/file/d/1GgVXvLuH1UtteY9PblSB3qIaKTkQSHlm/view?usp=sharing')
d18 #displaying the output data
df4 <- data.frame(d18) #converting data to dataframe
df4 <- na.omit(df4) #omiting nas from the dataframe
View(df4) #viewing the data in tabular form

#setting seed and initializing input to calculate the wss and kmeans
set.seed(101)
#initializing variables for clustering
input1 <- df1[,2:6] #variable for 2015
input2 <- df2[,2:6] #variable for 2016
input3 <- df3[,2:6] #variable for 2017
input4 <- df4[,2:6] #variable for 2018

#calculating WSS

#calculation of wss for input 1
wssplot1 <- function(df1, nc=20, seed=123){
  wss1 <- (nrow(df1)-1)*sum(apply(df1,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss1[i] <- sum(kmeans(df1, centers=i)$withinss)}
  plot(1:nc, wss1, type="b", xlab="Number of groups",ylab="Sum of squares within a group")}

wssplot1(input1, nc = 20) #plotting within sum of squares to get the best k value

#calculation of wss for input 2
wssplot2 <- function(df2, nc=20, seed=123){
  wss2 <- (nrow(df2)-1)*sum(apply(df2,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss2[i] <- sum(kmeans(df2, centers=i)$withinss)}
  plot(1:nc, wss2, type="b", xlab="Number of groups",ylab="Sum of squares within a group")}

wssplot2(input2, nc = 20) #plotting within sum of squares to get the best k value

#calculation of wss for input 3
wssplot3 <- function(df3, nc=20, seed=123){
  wss3 <- (nrow(df3)-1)*sum(apply(df3,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss3[i] <- sum(kmeans(df3, centers=i)$withinss)}
  plot(1:nc, wss3, type="b", xlab="Number of groups",ylab="Sum of squares within a group")}

wssplot3(input3, nc = 20) #plotting within sum of squares to get the best k value

#calculation of wss for input 4
wssplot4 <- function(df4, nc=20, seed=123){
  wss4 <- (nrow(df4)-1)*sum(apply(df4,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss4[i] <- sum(kmeans(df4, centers=i)$withinss)}
  plot(1:nc, wss4, type="b", xlab="Number of groups",ylab="Sum of squares within a group")}

wssplot4(input4, nc = 20) #plotting within sum of squares to get the best k value

set.seed(101)
#implmenting Kmeans

#kmeans for 2015
a1 <- kmeans(input1, centers = 3, nstart = 20)
a1 #displaying the output

#kmeans for 2016
b1 <- kmeans(input2, centers = 3, nstart = 20)
b1 #displaying the output

#kmeans for 2017
c1 <- kmeans(input3, centers = 3, nstart = 20)
c1 #displaying the output

#kmeans for 2018
d1 <- kmeans(input4, centers = 3, nstart = 20)
d1 #displaying the output

#using the silhouette function to validate the clustering data for each year from 2015-2018
#silhouette2015
sil15 <- silhouette(a1$cluster, dist(input1))
fviz_silhouette(sil15) #plotting the cluster for 2015

#silhouette2016
sil16 <- silhouette(b1$cluster, dist(input2))
fviz_silhouette(sil16) #plotting the cluster for 2016

#silhouette2017
sil17 <- silhouette(c1$cluster, dist(input3))
fviz_silhouette(sil17) #plotting the cluster for 2017

#silhouette2018
sil18 <- silhouette(d1$cluster, dist(input4))
fviz_silhouette(sil18) #plotting the cluster for 2018

df1$cluster <- as.factor(a1$cluster) #encode the cluster vector for a1 as factor in df1
df2$cluster <- as.factor(b1$cluster) #encode the cluster vector for b1 as factor in df2
df3$cluster <- as.factor(c1$cluster) #encode the cluster vector for c1 as factor in df3
df4$cluster <- as.factor(d1$cluster) #encode the cluster vector for d1 as factor in df4

#plots for 2015-2018
#using ggparcoord for interactive represenattion of 2015 cluster
p1 <- ggparcoord(df1, columns = c(1:6), groupColumn = "cluster", scale = "std") + labs(x = "avocado constituent", y = "value (in standard-deviation units)", title = "Clustering for 2015")
ggplotly(p1) #plotting clsuter for 2015
#using ggparcoord for interactive represenattion of 2016 cluster
p2 <- ggparcoord(df2, columns = c(1:6), groupColumn = "cluster", scale = "std") + labs(x = "avocado constituent", y = "value (in standard-deviation units)", title = "Clustering for 2016")
ggplotly(p2) #plotting clsuter for 2016
#using ggparcoord for interactive represenattion of 2017 cluster
p3 <- ggparcoord(df3, columns = c(1:6), groupColumn = "cluster", scale = "std") + labs(x = "avocado constituent", y = "value (in standard-deviation units)", title = "Clustering for 2017")
ggplotly(p3) #plotting clsuter for 2017
#using ggparcoord for interactive represenattion of 2018 cluster
p4 <- ggparcoord(df4, columns = c(1:6), groupColumn = "cluster", scale = "std") + labs(x = "avocado constituent", y = "value (in standard-deviation units)", title = "Clustering for 2018")
ggplotly(p4) #plotting clsuter for 2018

grid.arrange(p1, p2, p3, p4) #to represent all the cluster together in one plot


######################################################################################################################


                                        #Logistic Regression


######################################################################################################################

set.seed(678) #randomize the train and test set

avocado2018<-gsheet2tbl('https://drive.google.com/file/d/1GgVXvLuH1UtteY9PblSB3qIaKTkQSHlm/view?usp=sharing') #Load all sheets into different variables
avocado2017<-gsheet2tbl('https://drive.google.com/file/d/1L1BlgIkainsT5QH8rZtrbAiVKkTuUVH1/view?usp=sharing')
avocado2016<-gsheet2tbl('https://drive.google.com/file/d/1DB0b6Mfv-kU4zH5OcaMBt-8a1DOii5Rw/view?usp=sharing')
avocado2015<-gsheet2tbl('https://drive.google.com/file/d/1U55dmuM01gJX5-ANtyho73fHgpWzMha-/view?usp=sharing')

avocado2018$Year<-rep(c('2018'),nrow(avocado2018))#adding a column to each sheet to justify the year
avocado2017$Year<-rep(c('2017'),nrow(avocado2017))
avocado2016$Year<-rep(c('2016'),nrow(avocado2016))
avocado2015$Year<-rep(c('2015'),nrow(avocado2015))

region<-c(avocado2018$State,avocado2017$Region,avocado2016$Region,avocado2015$Region)#to merge all sheets into one data frame
averagePrice<-c(avocado2018$AveragePrice,avocado2017$AveragePrice,avocado2016$AveragePrice,avocado2015$AveragePrice)
year<-c(avocado2018$Year,avocado2017$Year,avocado2016$Year,avocado2015$Year)


avocado<-data.frame(region,year,averagePrice)#create a single data frame to create the model

hist(avocado$averagePrice) #creates a histogram to check the distribution of average price
high = ifelse(avocado$averagePrice<=1.3, "No", "Yes") #create a binary categorical variable to predict price range
avocado = data.frame(avocado, high) # adding the new variable to the data frame
table(avocado$high) #checking the distribution of Yes/No in the new column
str(avocado) #to check how many factor column and num column are present in the data frame

#To split the data frame into train and test data to put it into the model
#TRain is 70% and test is 30% in this case
trainA<- sample(1:nrow(avocado),0.7*nrow(avocado))

trainSet <- avocado[trainA,]
nrow(trainSet)

testSet <- avocado[-trainA,]
nrow(testSet)

# glm is the generalized linear model we will be using. high ~ region+year
#family = binomial() is used because we are predicting a binary outcome
logit <- glm(high ~ region+year, family = binomial(), trainSet)
predict<- predict(logit, trainSet, type = "response") #predict the probabilities for the train data

ROC_pred = prediction(predict, trainSet$high) #Extract predicted values via predict from a model object, 
#conditional on data, and return a dataframe
ROC_perf = performance(ROC_pred, "tpr", "fpr")#to check the validity of our model
plot(ROC_perf) #this plots the ROC curve which traces the percentage of true positives predicted by our model
#The plot is rising steeply indicating TPR is increasing faster than FPR
abline(a=0, b=1)  #reference to show that the curve is rising steeply


Pred_Test <- predict(logit, type = "response", newdata = testSet)#gives the list of the predicted values from the model created

#To describe the performance of the model and how many false positives are generated
Confusion_matrix<-table(testSet$high, Pred_Test >= 0.5)
accuracy <- sum(diag(Confusion_matrix))/ sum(Confusion_matrix)#from the confusion matrix we can calculate how accurate our model is to predict any new entry


######################################################################################################################


                                      #Hypothesis Test


######################################################################################################################


# assigning variables
# avocado sale and price for 2015
d1<-gsheet2tbl('https://drive.google.com/file/d/1U55dmuM01gJX5-ANtyho73fHgpWzMha-/view?usp=sharing')
# avocado sale and price for 2016
d2<-gsheet2tbl('https://drive.google.com/file/d/1DB0b6Mfv-kU4zH5OcaMBt-8a1DOii5Rw/view?usp=sharing')
# avocado sale and price for 2017
d3<-gsheet2tbl('https://drive.google.com/file/d/1L1BlgIkainsT5QH8rZtrbAiVKkTuUVH1/view?usp=sharing')
# avocado sale and price for 2018
d4<-gsheet2tbl('https://drive.google.com/file/d/1GgVXvLuH1UtteY9PblSB3qIaKTkQSHlm/view?usp=sharing')

# finding average total avocados sale for each year
# for the purpose of this assginment, we use the total sale of all three types of avocado

# average avocado sale for 2015
avg15<-mean(d1$`Total Volume`)
avg15

# average avocado sale for 2016
avg16<-mean(d2$`Total Volume`)
avg16

# average avocado sale for 2017
avg17<-mean(d3$`Total Volume`)
avg17

# average avocado sale for 201
avg18<-mean(d4$`Total Volume`)
avg18

# finding average total sale for 2015-2018
a<-mean(avg15,avg16,avg17,avg18)
a
# using one sample t-test
# comparing the mean of total avocado sale in each year with the total average sale for all 4 years

# Is there evidence that the mean of total avocado sale in 2015 is less than the 
# average total sale for all four years (a)
# Ho: µ = a
# Ha: µ > a
# one sided 95% confidence interval for µ
# alpha = 0.05
b4<- t.test(d4$`Total Volume`,mu=a,alternative = "less")
b4
# p-value > alpha
# do not reject the null hypothesis 

# Is there evidence that the mean of total avocado sale in 2016 is less than the 
# average total sale for all four years (a)
# Ho: µ = a
# Ha: µ > a
# one sided 95% confidence interval for µ
# alpha = 0.05
b3<- t.test(d3$`Total Volume`,mu=a,alternative = "less")
b3
# p-value > alpha
# do not reject the null hypothesis 

# Is there evidence that the mean of total avocado sale in 2017 is less than the 
# average total sale for all four years (a)
# Ho: µ = a
# Ha: µ > a
# one sided 95% confidence interval for µ
# alpha = 0.05
b2<- t.test(d2$`Total Volume`,mu=a,alternative = "less")
b2
# p-value > alpha
# do not reject the null hypothesis 

# Is there evidence that the mean of total avocado sale in 2018 is less than the 
# average total sale for all four years (a)
# Ho: µ = a
# Ha: µ > a
# one sided 95% confidence interval for µ
# alpha = 0.05
b1<- t.test(d1$`Total Volume`,mu=a,alternative = "less")
b1
# p-value > alpha
# do not reject the null hypothesis