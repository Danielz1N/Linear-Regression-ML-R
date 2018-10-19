#########################################################
#               Linear Regression - OLS                 #
#########################################################
## Library's
library(caTools)
library(tidyverse)
data <- data.frame(x = c(4,3.75,3.1,2.76,2.1,1.98,1.76),
                    y  = c(19.74,14.26,12.34,11.45,10.97,9.87,9.02))
set.seed(385802)
sample <- sample.split(x,SplitRatio = 0.7)
train <- subset(data,sample == T)
test <- subset(data,sample==F)
model <- lm(y~x, data=train)
predictions <- predict(model,test)

results <- cbind(predictions,test$y) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)



data %>%
  ggplot(aes(x,y))+
  geom_point()+
  theme_bw()
