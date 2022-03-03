library(tidyverse)

setwd("~/Desktop/GradSchool/PhD/spring2022/teaching/CS3130rCurriculumDev/Labs/Lab07")

packetData <- read.csv("data/packet_data.csv")

arg %>% f() ## f(arg)

processed <- packetData %>%
  mutate(time = as.integer(time)) %>%
  group_by(time) %>%
  summarise(packets_per_second = max(total_packets) - min(total_packets))

processPacketData <- function(packetData) {
  timeDifferences <- diff(packetData$time)
  packetDifferences <- diff(packetData$total_packets)
  result <- timeDifferences/packetDifferences
  return (result)
}

processPacketData2 <- function(packetData) {
  secondsPerPacket <- c()
  packetDifferences <- diff(packetData$total_packets)
  timeDifferences <- diff(packetData$time)
  n = nrow(packetData)
  for (i in 1:n) {
   if (packetDifferences[i] > 0) {
     ratio = timeDifferences[i]/packetDifferences[i]
     secondsPerPacket <- c(secondsPerPacket,ratio)
   } 
   else {
     #We'll check if at least one packet has accumulated by the next timestamp
     #First handle the boundary case
     if (i != n) {
       totalTime 
     }
   }
  }
  
}

result <- processPacketData(packetData)

########
pingData <- read.csv("ping_data.csv") %>%
  mutate(pingTime = as.numeric((str_split(pingData$ping,pattern = " ",
                               simplify = TRUE))[,1]))



####################################
d = rpois(100,lambda = 5)
mn = mean(d)
sd = sd(d)
rateEst = 1/mn
h = hist(d,freq=FALSE,xlim = c(0,max(d)))
xlines = seq(0,max(h$breaks),length.out = 100)
f = dnorm(xlines,mean = mn,
          sd = sd)
lines(x = xlines, y =f)

#With Continuous data:

#normal
#exponential
#gamma

#With Discrete data:

#Poisson
#binomial
#Negative Binomial
#Bernoulli






