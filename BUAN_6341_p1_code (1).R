setwd("C:/Education/Courses/Machine Learning/project 1")

#setwd("C:/Users/Karthik/Desktop/Fall-2017/Machine Learning")
data_hour <- read.csv("C:/Education/Courses/Machine Learning/project 1/hour.csv", header = TRUE)
data_day<-read.csv('day.csv')
data_hour<-read.csv('hour.csv')
library('caret')
data_hour$registered<-NULL
data_hour$casual<-NULL
data_hour$instant<-NULL
data_hour$dteday<-NULL
data_hour$yr<-NULL

data_hour$season1<-ifelse(data_hour$season == 1, 1, 0)
data_hour$season2<-ifelse(data_hour$season == 2, 1, 0)
data_hour$season3<-ifelse(data_hour$season == 3, 1, 0)

data_hour$mnth1<-ifelse(data_hour$mnth == 1, 1, 0)
data_hour$mnth2<-ifelse(data_hour$mnth == 2, 1, 0)
data_hour$mnth3<-ifelse(data_hour$mnth == 3, 1, 0)
data_hour$mnth4<-ifelse(data_hour$mnth == 4, 1, 0)
data_hour$mnth5<-ifelse(data_hour$mnth == 5, 1, 0)
data_hour$mnth6<-ifelse(data_hour$mnth == 6, 1, 0)
data_hour$mnth7<-ifelse(data_hour$mnth == 7, 1, 0)
data_hour$mnth8<-ifelse(data_hour$mnth == 8, 1, 0)
data_hour$mnth9<-ifelse(data_hour$mnth == 9, 1, 0)
data_hour$mnth10<-ifelse(data_hour$mnth == 10, 1, 0)
data_hour$mnth11<-ifelse(data_hour$mnth == 11, 1, 0)

data_hour$hr0<-ifelse(data_hour$hr == 0, 1, 0)
data_hour$hr1<-ifelse(data_hour$hr == 1, 1, 0)
data_hour$hr2<-ifelse(data_hour$hr == 2, 1, 0)
data_hour$hr3<-ifelse(data_hour$hr == 3, 1, 0)
data_hour$hr4<-ifelse(data_hour$hr == 4, 1, 0)
data_hour$hr5<-ifelse(data_hour$hr == 5, 1, 0)
data_hour$hr6<-ifelse(data_hour$hr == 6, 1, 0)
data_hour$hr7<-ifelse(data_hour$hr == 7, 1, 0)
data_hour$hr8<-ifelse(data_hour$hr == 8, 1, 0)
data_hour$hr9<-ifelse(data_hour$hr == 9, 1, 0)
data_hour$hr10<-ifelse(data_hour$hr == 10, 1, 0)
data_hour$hr11<-ifelse(data_hour$hr == 11, 1, 0)
data_hour$hr12<-ifelse(data_hour$hr == 12, 1, 0)
data_hour$hr13<-ifelse(data_hour$hr == 13, 1, 0)
data_hour$hr14<-ifelse(data_hour$hr == 14, 1, 0)
data_hour$hr15<-ifelse(data_hour$hr == 15, 1, 0)
data_hour$hr16<-ifelse(data_hour$hr == 16, 1, 0)
data_hour$hr17<-ifelse(data_hour$hr == 17, 1, 0)
data_hour$hr18<-ifelse(data_hour$hr == 18, 1, 0)
data_hour$hr19<-ifelse(data_hour$hr == 19, 1, 0)
data_hour$hr20<-ifelse(data_hour$hr == 20, 1, 0)
data_hour$hr21<-ifelse(data_hour$hr == 21, 1, 0)
data_hour$hr22<-ifelse(data_hour$hr == 22, 1, 0)
#data_hour$hr23<-ifelse(data_hour$hr == 23, 1, 0)


data_hour$holiday1<-ifelse(data_hour$holiday == 1, 1, 0)


data_hour$weekday0<-ifelse(data_hour$weekday == 0, 1, 0)
data_hour$weekday1<-ifelse(data_hour$weekday == 1, 1, 0)
data_hour$weekday2<-ifelse(data_hour$weekday == 2, 1, 0)
data_hour$weekday3<-ifelse(data_hour$weekday == 3, 1, 0)
data_hour$weekday4<-ifelse(data_hour$weekday == 4, 1, 0)
data_hour$weekday5<-ifelse(data_hour$weekday == 5, 1, 0)
#data_hour$weekday6<-ifelse(data_hour$season == 6, 1, 0)


data_hour$workingday1<-ifelse(data_hour$workingday == 1, 1, 0)

data_hour$weathersit1<-ifelse(data_hour$weathersit == 1, 1, 0)
data_hour$weathersit2<-ifelse(data_hour$weathersit == 2, 1, 0)
data_hour$weathersit3<-ifelse(data_hour$weathersit == 3, 1, 0)


data_hour$season<-NULL
data_hour$mnth<-NULL
data_hour$hr<-NULL
data_hour$holiday<-NULL
data_hour$weekday<-NULL
data_hour$workingday<-NULL
data_hour$weathersit<-NULL

library('caret')
trainIndex = createDataPartition(data_hour$cnt,p=0.7,list = FALSE)
train = data_hour[trainIndex,]
test = data_hour[-trainIndex,]

alpha <- 0.1
num_iters <- 1000
threshold<-0.0001
i=1
cost_history <- double()
theta_history <- list()
error_history <- double()
alpha_history <-double()
alpha_cost_history <-double()
deltacost<-1

y<-train$cnt
y<-as.matrix(train[c("cnt")])
temp<-train
temp$cnt<-NULL
x<-temp

y_test<-test$cnt
y_test<-as.matrix(test[c("cnt")])
temp_test<-test
temp_test$cnt<-NULL
x_test<-temp_test


cost2 <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}




theta <- matrix(c(0,0), nrow=53)


x1<-data.matrix(x)
X <- cbind(1,x1)

x1_test<-data.matrix(x_test)
X_test<-cbind(1,x1_test)

#remove
alpha <- 0.1
j=1



repeat{
i=1  
deltacost<-1
cost_history <- double()
theta <- matrix(c(0,0), nrow=53)

repeat{
error2 <- (X %*% theta - y)
delta <- t(X) %*% error2 / length(y)
theta <- theta - alpha * delta
cost_history[i] <- cost2(X, y, theta)
theta_history[[i]] <- theta
#error1<-as.data.frame(error)
#error_history[[i]] <- error1

if(i>1)
{
  deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
}

if(deltacost<threshold)
{
  break
}

i=i+1
}

plot(cost_history,type = "line")
print("Hello")
print(alpha)
print(cost_history[i])
print(deltacost)
print(i)


alpha_history[j]<-alpha
alpha_cost_history[j]<-cost_history[i]


if(alpha<0.075)
{
  break
}
#plot(cost_history,main= alpha,type = "line")

alpha=alpha-0.001
j=j+1
print(j)
}


plot(x = alpha_history,y = alpha_cost_history,type = "o",col="blue")

#--------------------------------ALpha-test---------------------------------------------------

alpha=0.1 
alpha_history <-double()
alpha_cost_history <-double()
  
    repeat{
    i=1  
    deltacost<-1
    cost_history <- double()
    theta <- matrix(c(0,0), nrow=53)
    
    repeat{
      error <- (X_test %*% theta - y_test)
      delta <- t(X_test) %*% error / length(y_test)
      theta <- theta - alpha * delta
      cost_history[i] <- cost(X_test, y_test, theta)
      theta_history[[i]] <- theta
      #error1<-as.data.frame(error)
      #error_history[[i]] <- error1
      
      if(i>1)
      {
        deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
      }
      
      if(deltacost<threshold)
      {
        break
      }
      
      i=i+1
    }
    print("Hello")
    print(alpha)
    print(cost_history[i])
      print(deltacost)
    print(i)
    
    
    alpha_history[j]<-alpha
    alpha_cost_history[j]<-cost_history[i]
    
    
    if(alpha<0.075)
    {
      break
    }
    #plot(cost_history,main= alpha,type = "line")
    
    alpha=alpha-0.001
    j=j+1
    print(j)
  }

#-----------------------------------------------------------------------------------


#-------------------Threshold-Train--------------------------------
  
alpha<-0.1
threshold<-0.01
cost_history <- double()
deltacost<-1
theta <- matrix(c(0,0), nrow=53)

repeat{
  i=1  
  deltacost<-1
  cost_history <- double()
  theta <- matrix(c(0,0), nrow=53)
  
  repeat{
    error <- (X %*% theta - y)
    delta <- t(X) %*% error / length(y)
    theta <- theta - alpha * delta
    cost_history[i] <- cost(X, y, theta)
    theta_history[[i]] <- theta
    #error1<-as.data.frame(error)
    #error_history[[i]] <- error1
    
    if(i>1)
    {
      deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
    }
    
    if(deltacost<threshold)
    {
      break
    }
    
    i=i+1
  }
  
 plot(cost_history,type = "line",xlab = "Threshold=0.01",ylab = "Cost_Function")
  print("Hello")
  print(threshold)
  print(cost_history[i])
  print(deltacost)
  print(i)
  
  if(threshold>0.1)
  {
    break
  }
  
    
  alpha_history[j]<-threshold
  alpha_cost_history[j]<-cost_history[i]
  
  
  #plot(cost_history,main= alpha,type = "line")
  
  threshold=threshold*10
  j=j+1
  print(j)
}

plot(x = alpha_history,y = alpha_cost_history,type = "o",col="blue",xlab = "Threshold",ylab = "Cost Function")

----------------------Test_threshold--------------------------------------------
alpha<-0.1
threshold<-0.0001
cost_history <- double()
deltacost<-1
theta <- matrix(c(0,0), nrow=53)
alpha_history <-double()
alpha_cost_history <-double()


repeat{
  i=1  
  deltacost<-1
  cost_history <- double()
  theta <- matrix(c(0,0), nrow=53)
  
  repeat{
    error <- (X_test %*% theta - y_test)
    delta <- t(X_test) %*% error / length(y_test)
    theta <- theta - alpha * delta
    cost_history[i] <- cost(X_test, y_test, theta)
    theta_history[[i]] <- theta
    #error1<-as.data.frame(error)
    #error_history[[i]] <- error1
    
    if(i>1)
    {
      deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
    }
    
    if(deltacost<threshold)
    {
      break
    }
    
    i=i+1
  }
  
  #plot(cost_history,type = "line",xlab = "Threshold=0.01",ylab = "Cost_Function")
  print("Hello")
  print(threshold)
  print(cost_history[i])
  print(deltacost)
  print(i)
  
  if(threshold>0.1)
  {
    break
  }
  
  
  alpha_history[j]<-threshold
  alpha_cost_history[j]<-cost_history[i]
  
  
  #plot(cost_history,main= alpha,type = "line")
  
  threshold=threshold*10
  j=j+1
  print(j)
}

plot(x = alpha_history,y = alpha_cost_history,type = "o",col="blue",xlab = "Threshold",ylab = "Cost Function")




#--------------------------3-Question-----------
alpha=0.1
threshold=0.0001
deltacost<-1
i<-1
cost_history <- double()
theta <- matrix(c(0,0), nrow=4)


x_new3<-as.matrix(train[c("hum","atemp","windspeed")])
X_new3 <- cbind(1,x_new3)


y_new3<-train$cnt
y_new3<-as.matrix(train[c("cnt")])

x_new3_Test<-as.matrix(test[c("hum","atemp","windspeed")])
X_new3_Test <- cbind(1,x_new3_Test)


y_new3_Test<-test$cnt
y_new3_Test<-as.matrix(test[c("cnt")])



repeat{
  error <- (X_new3 %*% theta - y_new3)
  delta <- t(X_new3) %*% error / length(y_new3)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X_new3, y_new3, theta)
  theta_history[[i]] <- theta
  #error1<-as.data.frame(error)
  #error_history[[i]] <- error1
  
  if(i>1)
  {
    deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
  }
  
  if(deltacost<threshold)
  {
    break
  }
  
  i=i+1
}

plot(cost_history,type = "o",col="blue",xlab = "Iteration with 3 features for train")

-------------------------Q3_Test---------------------
  alpha=0.1
threshold=0.0001
deltacost<-1
i<-1
cost_history <- double()
theta <- matrix(c(0,0), nrow=4)


repeat{
  error <- (X_new3_Test %*% theta - y_new3_Test)
  delta <- t(X_new3_Test) %*% error / length(y_new3_Test)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X_new3_Test, y_new3_Test, theta)
  theta_history[[i]] <- theta
  #error1<-as.data.frame(error)
  #error_history[[i]] <- error1
  
  if(i>1)
  {
    deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
  }
  
  if(deltacost<threshold)
  {
    break
  }
  
  i=i+1
}
plot(cost_history,type = "o",col="blue",xlab = "Iteration with 3 features for test")

#---------------------------all Features-------------------
  
threshold=0.0001
deltacost<-1
i<-1
cost_history <- double()
theta <- matrix(c(0,0), nrow=53)


repeat{
  error <- (X_test %*% theta - y_test)
  delta <- t(X_test) %*% error / length(y_test)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X_test, y_test, theta)
  theta_history[[i]] <- theta
  #error1<-as.data.frame(error)
  #error_history[[i]] <- error1
  
  if(i>1)
  {
    deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
  }
  
  if(deltacost<threshold)
  {
    break
  }
  
  i=i+1
}

plot(cost_history,type = "o",col="blue",xlab = "Iteration with all features for test")



#------------------------Question 4-----------------
  
df<-as.matrix(train[c("hum","atemp","season1","season2","season3")])  
df_test<-as.matrix(test[c("hum","atemp","season1","season2","season3")])  

x_df<-df
X_df <- cbind(1,x_df)

x_df_test<-df_test
X_df_test <- cbind(1,x_df_test)

y_df<-train$cnt
y_df<-as.matrix(train[c("cnt")])
y_df_test<-test$cnt
y_df_test<-as.matrix(test[c("cnt")])

threshold=0.0001
deltacost<-1
i<-1
cost_history <- double()
theta <- matrix(c(0,0), nrow=6)


repeat{
  error <- (X_df_test %*% theta - y_df_test)
  delta <- t(X_df_test) %*% error / length(y_df_test)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X_df_test, y_df_test ,theta)
  theta_history[[i]] <- theta
  #error1<-as.data.frame(error)
  #error_history[[i]] <- error1
  
  if(i>1)
  {
    deltacost=((cost_history[i-1]-cost_history[i])/cost_history[i-1])
  }
  
  if(deltacost<threshold)
  {
    break
  }
  
  i=i+1
}

plot(cost_history,type = "o",col="blue",xlab = "Iteration with selected features for test")
