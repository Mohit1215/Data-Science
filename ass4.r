install.packages("randomForest")

require(randomForest)
man1<-read.csv("manhattan.csv")

dim(man1)

train=sample(1:nrow(man1),21916)

Manhattan.rf=randomForest(man1$COMMERCIAL.UNITS ~ . , data = man1 , subset = train, na.rm=TRUE)
Manhattan.rf

install.packages("ggplot2")
library(ggplot2)
require(ggplot2)

x <- seq(1,10, length.out=20)
y <- seq(1,10, length.out=20)
man1<- expand.grid(X=x, Y=y)
man1$Z <- runif(400, 0, 5)
ggplot(man1, aes(x=man1$`SALE PRICE`))+ geom_tile(aes(fill = Z)) + theme_bw()



install.packages("ggplot2")
require(ggplot2)
library(ggthemes)
library(dplyr)
install.packages("dplyr")
require(dplyr)
install.packages("corrplot")
require(corrplot)
install.packages("corrgram")
require(corrgram)
install.packages("caTools")
require(caTools)
install.packages("MASS")
require(MASS)





#Exploring
head(man1)
str(man1)
summary(man1)
num.cols <- sapply(manc1, is.numeric)
cor.data <- cor(manc1[,num.cols])
cor.data
corrplot(cor.data,tl.cex=0.65)
typeof(man1$BOROUGH)

#Cleaning the data
any(is.na(man1))
colSums(man1 !=0)
colSums(manc2 !=0)
man1[is.na(man1)] <- 0
man1[is.na(man1)] <- 0
manc2[is.na(manc2)] <- 0
manc1 <- select(man1, -starts_with("EASE"))
manc1 <- select(man1, -starts_with("APART"))


manc2 <-select(manc1, -starts_with("BOROUGH"))
head(manc2)
head(manc1)
dim(man1)
dim(manc2)


# AGAIN Plotting heat map
num.cols1 <- sapply(manc2, is.numeric)
cor.data1 <- cor(manc2[,num.cols1])
cor.data1
corrplot(cor.data1,method='color')

#Plotting corrgram
corrgram(manc2,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

ggplot(manc2,aes(x=manc2$`SALE
                   PRICE`)) + theme_minimal()

#BUILDING A MODEL

#LINEAR REGRESSION MODEL
set.seed(101)
sample <- sample.split(manc2$SALE.PRICE, SplitRatio=0.80)

train = subset(manc2, sample == TRUE)
test = subset(manc2, sample == FALSE)


# TRAINING A MODEL

model <- lm(manc2$`SALE.PRICE` ~ manc2$`ZIP.CODE`+manc2$`LAND.SQUARE.FEET`+manc2$`GROSS.SQUARE.FEET`,data=train)
model
summary(model)


# Interpretting the Residuals

res <- residuals(model)
res <- as.data.frame(res)
head(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)

plot(model)

#PREDICTION of sale price and actual price

SalePrice.predictions <- predict(model,test)
results <- cbind(SalePrice.predictions,test$`SALE
                 PRICE`) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
print(head(results))


# PREDICTION of sales price for Gross square Feet :1752

Sales<- 2313213.96 + ( -178.24)+(-4.18  )+(76.01*1752)
Sales

#MSE
print(mse)
mse <- mean((results$real-results$pred)^2)
print(mse)
#RMSE
print("squared root of mse")
print(mse^0.5)

#R-SQUARED VALUE

SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
print("R2")
print(R2)

# CONFIDENCE INTERVAL

confint(model)
#PERFORMING K-MEANS

pl1 <- ggplot(manc2, aes(manc2$`GROSS SQUARE FEET`,manc2$`SALE
                           PRICE`, color = manc2$`TAX CLASS AT PRESENT`)) + geom_point()

print(pl1 + geom_point(size=4))




View(manc2)


