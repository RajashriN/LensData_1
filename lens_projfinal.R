library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. library(caret)
library(xlsx)
##getwd()
##setwd("C:/Users/Narasimman/Documents")
lensdata<- read.csv(file.choose())
lensdata

str(lensdata)

lensdata$Astigmatic <- as.factor(lensdata$Astigmatic)
lensdata$Specs.Pres<-as.factor(lensdata$Specs.Pres)
lensdata$Age<-as.factor(lensdata$Age)
lensdata$Tear.Production<-as.factor(lensdata$Tear.Production)
lensdata$Lens.Type<-as.factor(lensdata$Lens.Type)

##Depending on Astigmatism what is the lens prescription
Train <- createDataPartition(lensdata$Tear.Production, p=0.7, list=FALSE)
training <- lensdata[ Train, ]
testing <- lensdata[ -Train, ]

mod_fit_one <- train(Lens.Type ~ ., data = training,method = "glm", 
                     family="binomial")

mod_fit_one


str(testing)

pred = predict(mod_fit_one, newdata=testing)
#accuracy <- table(pred, testing[,"Lens.Type"])
#sum(diag(accuracy))/sum(accuracy)


confusionMatrix(data=pred, testing$Lens.Type) 





##Random forest
mod_fit_one <- train(Lens.Type ~ ., data = training,method = "rf") 
                     
mod_fit_one
varImp(mod_fit_one)
summary(lensdata)
str(lensdata)

pred1 = predict(mod_fit_one, newdata=testing)

confusionMatrix(data=pred1, testing$Lens.Type)
## accuracy - 83.33%


mod_fit_two <- train(Lens.Type ~ ., data = training,method = "knn") 

mod_fit_two
varImp(mod_fit_two)
summary(lensdata)
str(lensdata)



pred2 = predict(mod_fit_two, newdata=testing)

confusionMatrix(data=pred2, testing$Lens.Type)





mod_fit_3 <- train(Lens.Type ~ ., data = training,method = "nb") 

mod_fit_3
varImp(mod_fit_3)
summary(lensdata)
str(lensdata)



pred3 = predict(mod_fit_3, newdata=testing)

confusionMatrix(data=pred3, testing$Lens.Type)

###trial 
