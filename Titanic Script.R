#after you load data combine test and train and add a column named survived with 0 in it for now...
#then clean it up how ever you would like...squeaky clean...
#if you use randomForest model make sure you fill in blanks just dont use column with blanks...


test$Survived <- rep(0,418) 
full <- rbind(train , test)






str(train.data)
#convert Survived and Pclass to factors
full$Survived=factor(train.data$Survived)
full$Pclass=factor(train.data$Pclass)

#percentage of missing values
sum(is.na(full$Age)==T)/ length(full$Age)

#get all perentage of missing values
sapply(full, function(df){
                    sum(is.na(df)==T)/length(df)
} 
  )

library(Amelia)
missmap(full, main="Missing Map")

#Imputing missing values

#Show distribution of Embarcation with missing values
table(full$Embarked, useNA = "always")

#Assign these NA values to the most probable port S for south hampton
full$Embarked[which(is.na(full$Embarked))] = "S"


#first convert name to a character
full$Name=as.character(full$Name)

#use regular expression pattern (\\s+) 
# (i think)splits up all the words into diffrent words and we can now count the most repeated
table_words=table(unlist(strsplit(full$Name, "\\s+")))
#sort our counted words out (grep looks for paterns)
sort(table_words [grep('\\.', names(table_words))], decreasing=T)


# obtian titles with missing values
library(stringr)
tb = cbind(full$Age, str_match(full$Name, "[a-zA-Z]+\\."))
table(tb[is.na(tb[,1]),2])

#assign find averge mean value
mean.mr = mean(full$Age[grepl("Mr\\.", full$Name) & !is.na(full$Age)])
mean.mrs = mean(full$Age[grepl("Mrs\\.", full$Name) & !is.na(full$Age)])
mean.dr = mean(full$Age[grepl("Dr\\.", full$Name) & !is.na(full$Age)])
mean.miss = mean(full$Age[grepl("Miss\\.", full$Name) & !is.na(full$Age)])
mean.master = mean(full$Age[grepl("Master\\.", full$Name) & !is.na(full$Age)])

#now assing the missing values with mean value
train.data$Age[grepl("Mr\\.", full$Name) & is.na(full$Age)] =mean.mr
train.data$Age[grepl("Mrs\\.", full$Name) & is.na(full$Age)] =mean.mrs
train.data$Age[grepl("Dr\\.", full$Name) & is.na(full$Age)] =mean.dr
train.data$Age[grepl("Miss\\.", full$Name) & is.na(full$Age)] =mean.miss
train.data$Age[grepl("Master\\.", full$Name) & is.na(full$Age)] =mean.master







#visualize the data



#Passenger Survival
barplot(table(full$Survived), main = "Passenger Survival", names= c("Perished", "Survived"))

#Passenger Class
barplot(table(full$Pclass), main="Passenger Class", names= c("First","Second", "Third"))

#Passenger Gender
barplot( table(full$Sex) , main="Passenger Gender")

#Histogram of ages
hist(full$Age, main="Passanger Age", xlab = "Age")

#Passenger Siblings
barplot(table(full$SibSp), main = "Passenger Siblings")

#Passenger Parch
barplot(table(full$Parch), main="Passenger Parch")

#Passenger Fair
hist(full$Fare, main = "Passenger Fare", xlab = "Fare")
        
#Port of Embarcation
barplot(table(full$Embarked), main="Port of Embarcation", names= c("C","Q", "S"))
        
#Which gender is more likely to perish at shipwreck?
counts= table(full$Survived, full$Sex)
barplot(counts, col = c("darkblue","red"), legend=c("Perished", "Survived"), main= "Passanger Survival by Sex")
        
#Did class effect survival rate?
counts= table(full$Survived, full$Pclass)
barplot(counts, col = c("darkblue","red"), legend=c("Perished", "Survived"), main= "Titanic Class Bar Plot")

#Passanger Gender by class
counts=table(full$Sex, full$Pclass)
barplot(counts, col = c("darkblue","red"), legend=rownames(counts), main= "Passanger Gender by Class")

#histogram of passanger ages
hist(full$Age[which(train.data$Survived=="0")], main = "Passager Age Histogram", xlab = "Age", ylab = "Count", col = "blue", breaks = seq(0,80,by=2))
hist(full$Age[which(train.data$Survived=="1")], col = "red",add=T,  breaks = seq(0,80,by=2))

#examine more relationship between age and survival rate with box plot
boxplot(train$Age ~ train$Survived, main="Passenger Survival by Age", xlab= "survived", ylab="age")



#percentage of each age group that survived

#percentage of child that survived
train.child = train$Survived[train.data$Age < 13]
length(train.child[which(train.child==1)])/length(train.child)

#percentage youth that survived
train.youth = train$Survived[train.data$Age >=13 & train.data$Age < 25]
length(train.youth[which(train.youth==1)])/length(train.youth)

#percentage adult that survived
train.adult = tarin$Survived[train.data$Age >=20 & train.data$Age < 65]
length(train.adult[which(train.adult==1)])/length(train.adult)

#percentage senior that survived
train.senior = train$Survived[train.data$Age >= 65]
length(train.senior[which(train.senior==1)])/length(train.senior)




#Prediction Model
traindata <- as.data.frame(train.data)

#split data

split.data = function(data=traindata , p = 0.7, s=666) {
  set.seed(s)
  index = sample(1:dim(data)[1])
  trian1 = data[index[1:floor(dim(data)[1]*p)],]
  test = data[index[((ceiling(dim(data)[1]*p))+1):dim(data)[1]], ]
  return(list(train=train, test=test))
}

#split data 70% training and 30 % testing
allset = split.data(traindata, p=0.7)
trainset = allset$train
testset = allset$test

#condition tree
#we need party of ctree
install.packages('party')
require('party')

#Generate prediction Model
train.ctree = ctree(Survived~Pclass+Sex+Age+SibSp+Fare+Parch+Embarked, data=trainfiltered)
train.ctree

#plot the tree
plot(train.ctree, main = "Conditional inference tree of Titanic Data set")


#svm machine learning 

install.packages('e1071')
require('e1071')

svm.model= svm(Survived ~ Pclass+Sex+Age+SibSp+Fare+Parch+Embarked,data = trainset, probability=T)


#valadating ctree model
#predict survival of the testing set:
ctree.predict = predict(train.ctree, testset)

install.packages("caret")
require("caret")

#use caret to generate confusion matrix to genrate the statistics of the ouptput matrix
confusionMatrix(ctree.predict, testset$Survived)

#ROC Curve (true possitive vs flase possitive)
train.ctree.pred = predict(train.ctree, testset)
train.ctree.prob = 1- unlist(treeresponse(train.ctree, testset), use.names = F)[seq(1,nrow(testset)*2,2)]

#install rocr package
install.packages("ROCR")
require(ROCR)

#create rocr prediction from probabilities
train.ctree.prob.rocr = prediction(train.ctree.prob,testset$Survived)

#prepare rocr performance object for the ROC curve
train.ctree.perf = performance(train.ctree.prob.rocr,"tpr","fpr")
train.ctree.auc.perf=performance(train.ctree.prob.rocr, measure = "auc", x.measure = "cutoff")

#plot roc curve with colorze as T, and put AUC as the title
plot(train.ctree.perf, col=2, colorize= T, main=paste("AUC:", train.ctree.auc.perf@y.values))




#Seperate back into train and test too train the model
train <- full[1:891,]
test <- full[892:1309,]


#making submisions....
testfiltered$Survived <- NULL

#write submission

prediction <- predict(#what ever model you want, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediciton)
write.csv(submit9, file = "name of your file", row.names = FALSE)

