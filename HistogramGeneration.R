rm(list = ls())
library(ggplot2)
library(tidyverse)
library(TruncExpFam)

totalNums <- 100000
#numbers <-rtruncgamma(totalNums, 3, rate = 1, scale = 1, a =0, b = 1) 
numbers<-runif(totalNums)
data<-as.data.frame(numbers)


data$results <- numbers

data$results2 <- numbers

data$results3 <- numbers

data$results4 <- numbers


for (i in 1:4) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=3.9) 
    data$results[i]= lambda*data$results[i]*(1-data$results[i])
  }
}


for (i in 1:4) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=3.935) 
    data$results2[i]= lambda*data$results2[i]*(1-data$results2[i])
  }
}


ggplot(data,aes(x=results,y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_gray(base_size=18)


ggplot(data,aes(x=results2, y = after_stat(count/sum(count)))) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_gray(base_size=18)




for (i in 1:20) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=4) 
    data$results3[i]= lambda*data$results3[i]*(1-data$results3[i])
  }
}

for (i in 1:200) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=4) 
    data$results4[i]= lambda*data$results4[i]*(1-data$results4[i])
  }
}



data %>%
  ggplot(aes(y = after_stat(count / sum(count)))) +
  geom_freqpoly(aes(x=numbers,color="0 Iterations"),center=0.016) +
  geom_freqpoly(aes(x=results,color="2 Iterations")) +
  geom_freqpoly(aes(x=results2,color="5 Iterations")) +
  geom_freqpoly(aes(x=results3,color="20 Iterations")) +
  geom_freqpoly(aes(x=results4, color="200 Iterations")) +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_grey(base_size=18)



ggplot(data,aes(x=results)) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency')


ggplot(data,aes(x=results)) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency')
# 
# 
# 
# 
# average <-mean(data$results)
# print(average)
# median <- median(data$results)
# print(median)
# standardDev <- sd(data$results)
# print(standardDev)
# 
# 
# 
# 
# 
# ggplot(data, aes(x = results)) +
#   geom_histogram(bins = 125, color = "#002E5A", fill = "#0062a5") +
#   labs(x = 'Numbers', y = 'Frequency') + 
#   geom_vline(xintercept = mean(data$results), col = "red", lwd = 1) + 
#   geom_vline(xintercept = median(data$results), col = "orange", lwd = 1) 


values1 <- rtruncexp(100000,rate=1.25,a=0, b=1)

g <- ggplot(data,aes(x=values1)) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency')
# 
g+theme_grey(base_size=20)


sampleDist = function(n) { 
  sample(x = c(0.11,0.33,0.55,0.6,0.78), n, replace = T, prob = c(0.196, 0.140, 0.233, 0.322,0.107)) 
}

values2<-sampleDist(100000)

ggplot(data,aes(x=values2)) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_grey(base_size=18)


for (i in 1:20) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=3.9)
    data$results2[i]= lambda*data$results2[i]*(1-data$results2[i])
  }
}


ggplot(data,aes(x=results2)) +
  geom_histogram(binwidth=0.01,center=0.005,  color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_grey(base_size=18)+
  xlim(0,1)


for (i in 1:20) {
  for (i in 1:length(numbers)) {
    lambda <- runif(1,min=3.87,max=3.935)
    data$results3[i]= lambda*data$results3[i]*(1-data$results3[i])
  }
}




ggplot(data,aes(x=results3))+
  geom_histogram(aes(y = after_stat(count / sum(count))),binwidth=0.01,center=0.005, color="#002E5A", fill="#0062a5") +
  labs(x = 'Numbers', y = 'Frequency') +
  theme_grey(base_size=18)+
  xlim(0,1)