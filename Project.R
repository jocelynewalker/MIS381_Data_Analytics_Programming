library(usmap)
library(ggplot2)
library(dplyr)
library(glmnet)

cfb[is.na(cfb)] <- 0
View(cfb)
cfb$time <- substr(cfb$DateTime, start= 12, stop = 20)
cfb$time <- strtoi(gsub("[^0-9.]", "",  cfb$time))

state_attendance <- cfb %>% group_by(State)


state_summary <- state_attendance %>% summarise(
  attend = sum(Attendance),
  team_count = n_distinct(Team),
  mean_attend = mean(Attendance),
  fillRate = mean(Fill.Rate),
  gameDay = sum(GD)
)

state_summary$state = state_summary$State

plot_usmap(data = state_summary, values = "mean_attend", color = "red") + 
  scale_fill_continuous(name = "College Football Average Attendance", label = scales::comma) + 
  theme(legend.position = "right")




cfb.X <- cfb[ ,-c(1,2,4,10, 22, 23, 24, 25, 28)]

train <- cfb.X[cfb.X$Year < 2016,]
test <- cfb.X[cfb.X$Year >= 2016,]

model <- lm(Attendance ~ ., data = train)
summary(model)
length(model$coefficients)

predictions = predict(model, newdata=train)
inRMSE = sqrt(mean(train$Attendance-predictions)^2)
inRMSE

predictions = predict(model, newdata=test)
RMSE = sqrt(mean((test$Attendance-predictions)^2))
RMSE



#lasso
y = cfb.X[,3]
X = cfb.X[,-3]
cfb.model <- model.matrix(~., data = X)

y_train <- cfb.X[,3][cfb.X$Year < 2016]
y_test <- cfb.X[,3][cfb.X$Year >= 2016]
train <- cfb.model[cfb.model[,"Year"] < 2016,]
test <- cfb.model[cfb.model[,"Year"] >= 2016,]

Lasso.Fit = glmnet(train,y_train,alpha=1)

plot(Lasso.Fit)


#Let us do a 10-fold cv for selecting our parameter lambda
CV.L = cv.glmnet(train, y_train,alpha=1) #For Lasso


#Values of lambda (see help for more details)
LamL = CV.L$lambda.1se

#For LASSO
plot(log(CV.L$lambda),sqrt(CV.L$cvm),
     main="LASSO CV (k=10)",xlab="log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamL),lty=2,col=2,lwd=2)

#Now we can check the coefs of each selected model
coef.L = predict(CV.L,type="coefficients",s=LamL)
coef.L[order(coef.L)]

predictions = predict(CV.L, s = LamL, newx = train)
RMSE = sqrt(mean((y_train-predictions)^2))
RMSE

predictions = predict(CV.L, s = LamL, newx = test)
RMSE = sqrt(mean((y_test-predictions)^2))
RMSE


UTEP = cfb[,6][cfb$Team == 'UTEP']
Oklahoma = cfb[,6][cfb$Team == 'Oklahoma']

boxplot(UTEP,Oklahoma, names = c("UTEP", "Oklahoma"))

TexasAM = cfb[,10][cfb$Team == 'Texas A&M']
year = cfb[,29][cfb$Team == 'Texas A&M']
capacity = cfb[,9][cfb$Team == 'Texas A&M']
plot(year,TexasAM, main = "Texas A&M Game Fill Rate by Year")
abline(v=2012, col='red')
abline(v= 2011, col="blue")
line(capacity)

AM <- data.frame(TexasAM, year, capacity)


p = ggplot() + 
  geom_line(data = prescription1, aes(x = dates, y = Difference), color = "blue") +
  geom_line(data = prescription2, aes(x = dates, y = Difference), color = "red") +
  xlab('Dates') +
  ylab('percent.change')

print(p)

coeff <- 100000
ggplot(AM) + geom_line(aes(x=year, y=capacity)) + 
  geom_point(aes(x=year, y=TexasAM*100000)) +
  ggtitle("Texas A&M Fill Rate and Stadium Capacity")+
  geom_vline(xintercept = 2012, linetype="solid", 
             color = "blue", size=1.5) +
  scale_y_continuous(
  
  # Features of the first axis
  name = "Stadium Capacity",
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~./coeff, name="Fill Rate")
) 


library(ggplot2)
# Basic scatter plot
ggplot(AM, aes(x=year, y=TexasAM)) + 
  geom_point(AM, aes(x=year, y=TexasAM)) +
  geom_line(data = AM, aes(x=year, y=capacity))
  
  scale_y_continuous(
  
  # Features of the first axis
  name = "Fill Rate",
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~.*coeff, name="Stadium Capacity")
  
)




