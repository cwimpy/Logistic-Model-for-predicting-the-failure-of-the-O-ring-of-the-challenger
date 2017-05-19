#import data
challenger <- read.csv("C:/Users/prana/Desktop/R Project/R Logistic Regression/challenger_data.txt", sep="")
View(challenger)

#removing the column flight
challenger <- challenger[,-1]

#Removing the three outliers:
#challenger <- challenger[ -(which(challenger$Temp>=70 & challenger$Failure==1)), ]

#sorting the data with respect to temperature
if(is.unsorted(challenger$Temp)){
  challenger <- challenger[order(challenger$Temp),]
  temp <- challenger$Temp
  failure <- challenger$Failure 
}

challenger <- challenger[c(1:4,6,8),]

#try considering linear model

#pairs
pairs(challenger [,1:2])

#logistic model
logmodel <- glm(failure~temp,family = binomial())
summary(logmodel)

intercept <- logmodel$coefficients[1]
temp_coefficients <- logmodel$coefficients[2]

# probability (PI)
predicted_probability <- function(intercept,temp_coefficients,temp){
  t <- (exp(intercept+temp_coefficients*temp))
  prob <- t/(1+(t))
  return(prob)
}

P <- predicted_probability(intercept,temp_coefficients,temp)
#predicted <- predict(P, challenger, type="response")
plot(failure~temp,data=challenger)
#lines(temp, P, type="l", col="red")
plot(temp,P, xlim = c(25,85), ylim = c(0,1))
t = seq(1,length(temp),0.01)
lines(temp,P,type="l",col="blue", lwd=2)
abline(a=0.5,b=0,col = "red" )

#predicting the probability of failure when temperature is 29
predicted_temp <- predicted_probability(intercept,temp_coefficients,29)
points(29,y=predicted_temp,col="red")
segments(temp[1],predicted_probability(intercept,temp_coefficients,temp[1]),29,predicted_temp,col = "blue")

#predicting the probability of failure when temperature is 28
predicted_temp <- predicted_probability(intercept,temp_coefficients,28)
points(28,y=predicted_temp,col="red")

#predicting the probability of failure for any given temperature 
##replace temperature with actual temperature in the below given code
#predicted_probability(intercept,temp_coefficients,temperature)