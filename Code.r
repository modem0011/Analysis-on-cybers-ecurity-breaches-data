# Data loading

x<-read.csv(file.choose())
x

dataset=x
dataset = subset(dataset, select = -c(breach_end,Summary,Business_Associate_Involved) )
data


# Data Preprocessing


str(x)
summary(x)
ncol(x)
nrow(x)
dim(x)
colnames(x)
head(x)


x=sapply(x, function(x){is.na(x)<-which(x == '');x})
x<-as.data.frame(x)

x[x==' ']<-NA


library("Amelia")

missmap(x, main = "Missing Map",col=c("skyblue","pink"))


data = subset(x, select = -c(breach_end,Summary,Business_Associate_Involved) )
data

missmap(data, main = "Missing Map",col=c("skyblue","pink"))




#data visualisation


par(bg='gray')

count<-table(dataset$State)
barplot(count,col=rainbow(30))
count


count2<-table(dataset$Type_of_Breach)
barplot(count2,legend=rownames(count2),col=rainbow(30))
count2


count3<-table(dataset$Location_of_Breached_Information)
a=barplot(count3,col=rainbow(30))
length(count3)


count4<-table(dataset$year)
barplot(count4,col = rainbow(10),beside = T,
        space = c(0,1))
abline(0,0)
legend("bottomright", fill = c(1:8), legend = c(1:8), ncol = 4)



pie(count4, main="Pie Chart of Countries",width=10,height=34)
pie(count3, main="Pie Chart of Countries",width=10,height=34)

pie(count2, main="Pie Chart of Countries",width=10,height=34)
pie(count, main="Pie Chart of Countries",width=10,height=34)


boxplot(count)
boxplot(count2)
boxplot(count3)
boxplot(count4)


a=density(count)
plot(a)
a=density(count2)
plot(a)
a=density(count3)
plot(a)
a=density(count4)
plot(a)


barplot(count4,count2)
count2
count4



library(cluster)



library(caTools)
split <- sample.split(data, SplitRatio = 0.7)
split




train <- subset(data, split== "TRUE")
test <- subset(data, split== "FALSE")
str(train)
str(test)

library(nnet)

test<-multinom(x$Type_of_Breach ~ x$State)
summary(test)
fitted(test)







#...........................................................................


x<-read.csv(file.choose())
x

dataset=x
dataset = subset(dataset, select = -c(breach_end,Summary,Business_Associate_Involved) )
data


# Data Preprocessing


str(x)
summary(x)
ncol(x)
nrow(x)
dim(x)
colnames(x)
head(x)


x=sapply(x, function(x){is.na(x)<-which(x == '');x})
x<-as.data.frame(x)

x[x==' ']<-NA



data = subset(x, select = -c(breach_end,Summary,Business_Associate_Involved) )
data

data$Type_of_Breach<-as.factor(data$Type_of_Breach)
mymodel<-multinom(data$Type_of_Breach~State+Individuals_Affected+Location_of_Breached_Information+year,Date_Posted_or_Updated,data=data)
summary(mymodel)

s=predict(mymodel,data,type="prob")
cm<-table(predict(mymodel),data$Type_of_Breach)
print(cm)
accu=mean(predict(mymodel) != data$Type_of_Breach)
accu


train$State=as.factor(train$State)
mymodel<-multinom(State~Type_of_Breach+Individuals_Affected+Location_of_Breached_Information+year,data=data)




























x<-read.csv(file.choose())
x


x=sapply(x, function(x){is.na(x)<-which(x == '');x})
x<-as.data.frame(x)

x[x==' ']<-NA



data = subset(x, select = -c(breach_end,Summary,Business_Associate_Involved) )
data



library(caTools)
split <- sample.split(data, SplitRatio = 0.8)
?s
train <- subset(data, split== "TRUE")
test <- subset(data, split== "FALSE")



library(nnet)
library(caret)

mymodel<-multinom(Type_of_Breach~State+Individuals_Affected+Location_of_Breached_Information+year,data=train)

cm<-table(predict(mymodel,test),test$Type_of_Breach)
print(cm)
confusionMatrix(cm)
predict(mymodel,test)
cm[34]

predict(mymodel,test)
test$Type_of_Breach

acc=mean(predict(mymodel,test) != test$Type_of_Breach)
acc


mymodel<-multinom(State~Type_of_Breach+Individuals_Affected+Location_of_Breached_Information+year,data=train)

cm<-table(predict(mymodel,test),test$State)
print(cm)

predict(mymodel,test)
cm[34]

predict(mymodel,test)
test$Type_of_Breach

acc=mean(predict(mymodel,test) != test$State)
acc

train$Type_of_Breach=as.factor(train$Type_of_Breach)

test$Type_of_Breach=as.factor(test$Type_of_Breach)
library(e1071)
model<-naiveBayes(x=c(train$State+train$Individuals_Affected+train$ Location_of_Breached_Information+train$year),y=train$Type_of_Breach)
model
a=predict(model,newdata = train$Type_of_Breach)
a=as.factor(a)
cm<-table(train$Type_of_Breach,a)
cm

acc=mean(train$Type_of_Breach != a)
acc

model<-naiveBayes(x=c(train$Type_of_Breach+train$Individuals_Affected+train$ Location_of_Breached_Information+train$year),y=train$State)
model
a=predict(model,newdata = train$State)
a=as.factor(a)
cm<-table(train$State,a)
cm

acc=mean(train$Type_of_Breach != a)
acc


library(rpart)
DC<-rpart(Type_of_Breach ~ State+year+Individuals_Affected+Location_of_Breached_Information,data=train,method="class")
library(rpart.plot)
rpart.plot(DC)
fitval<-predict(DC,newdata = test,type="class")
table(test$Type_of_Breach,fitval)
acc<-mean(fitval != test$Type_of_Breach)
acc

DC<-rpart(State~Type_of_Breach+year+Individuals_Affected+Location_of_Breached_Information,data=train,method="class")
library(rpart.plot)
rpart.plot(DC)
fitval<-predict(DC,newdata = test,type="class")
table(test$State,fitval)
acc<-mean(fitval != test$State)
acc

library(randomForest)
ran=randomForest(x=train[-7],y=train$Type_of_Breach,ntree = 500)
plot(ran)
importance(ran)
varImpPlot(ran)
y_pred = predict(ran, newdata = test[-7])

cm = table(test[, 7], y_pred)
cm







kmeans.result <- kmeans(test, centers = 3, nstart = 20)
for (i in 1:10) wcss[i] = sum(kmeans(test, centers = i,nstart = 20)$withinss)
# Plot WSS vs. number of clusters
plot(1:10, 
     wcss, 
     type = "b", 
     main = paste('The Elbow Method'),
     xlab = "Number of Clusters", 
     ylab = "WCSS")
kmeans.result$cluster


table(test$State, kmeans.result$cluster)

plot(test[c("Location_of_Breached_Information", "State")])
plot(test[c("Location_of_Breached_Information", "State")], col = kmeans.result$cluster)
plot(test[c("Location_of_Breached_Information", "State")], col = kmeans.result$cluster, main = "K-Means with 3 clusters")


y_kmeans <- kmeans.result$cluster

clusplot(test[,c("Location_of_Breached_Information", "State")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of test'),
         xlab = 'Location_of_Breached_Information',
         ylab = 'State')







kmeans.result <- kmeans(test, centers = 3, nstart = 20)

for (i in 1:10) wcss[i] = sum(kmeans(test, centers = i,nstart = 20)$withinss)
# Plot WSS vs. number of clusters
plot(1:10, 
     wcss, 
     type = "b", 
     main = paste('The Elbow Method'),
     xlab = "Number of Clusters", 
     ylab = "WCSS")
kmeans.result$cluster


table(test$State, kmeans.result$cluster)

plot(test[c("Type_of_Breach", "State")])
plot(test[c("Type_of_Breach", "State")], col = kmeans.result$cluster)
plot(test[c("Type_of_Breach", "State")], col = kmeans.result$cluster, main = "K-Means with 3 clusters")


y_kmeans <- kmeans.result$cluster

clusplot(test[,c("Type_of_Breach", "State")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of test'),
         xlab = 'Type_of_Breach',
         ylab = 'State')






