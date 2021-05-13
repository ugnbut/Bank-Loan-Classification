library(rpart)
library(readr)
library(RColorBrewer)
library(readr)
library(rattle)

Banks = read.csv("UniversalBank.csv", stringsAsFactors=FALSE, header = TRUE)
colSums(is.na(Banks))

hist(Banks$Personal.Loan, freq=FALSE, main="Density plot")
curve(dnorm(x, mean=mean(Banks$Personal.Loan), sd=sd(Banks$Personal.Loan)), add = TRUE, col='darkblue', lwd=2)

Banks$CCA <- ifelse(Banks$Personal.Loan == 0, "No", "Yes")
Banks$Age.f <- as.factor(Banks$Age)
Banks$Education.f <- as.factor(Banks$Education)
Banks$Income.f <- as.factor(Banks$Income)
Banks$Experience.f <- as.factor(Banks$Experience)
Banks$CCAvg.f <- as.factor(Banks$CCAvg)

hist(Banks$Age, freq=FALSE, main="Density plot")
curve(dnorm(x, mean=mean(Banks$Age), sd=sd(Banks$Age)), add = TRUE, col='darkblue', lwd=2)

sort(unique(Banks$Age))
Banks$Age.f <- cut(Banks$Age, 4, labels=c('23-34','35-46','47-58','59-70'))

sort(unique(Banks$Education))
Banks$Education.f <- cut(Banks$Education, 3, labels=c('High-School Completed', 'Bachelors Degree', 'Masters Degree'))

sort(unique(Banks$Income))
Banks$Income.f <- cut(Banks$Income, 3, labels=c('Low', 'Middle', 'High'))

sort(unique(Banks$Experience))
Banks$Experience.f <- cut(Banks$Experience, 3, labels=c('Beginner', 'Advanced', 'Expert'))

sort(unique(Banks$CCAvg))
Banks$CCAvg.f <- cut(Banks$CCAvg, 3, labels=c('Low', 'Middle', 'High'))

inTrain <- createDataPartition(y = Banks$CCA,
                               p = 0.7,
                               list = FALSE)

training <- Banks[ inTrain,]
testing <- Banks[-inTrain,]

set.seed(123)
tree <- rpart(CCA ~ Age.f + Education.f + Income.f + Experience.f + CCAvg.f, data = training, method='class')
fancyRpartPlot(tree)
printcp(tree)
tree$variable.importance

tree.pred <- predict(tree, testing, type = "class")
table(tree.pred, testing$CCA)
(1337 + 94) / nrow(testing)

######################## THE END ################################################