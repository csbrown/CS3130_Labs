library(tidyverse)
library(reshape2)

setwd("~/Desktop/GradSchool/PhD/spring2022/teaching/CS3130rCurriculumDev/Labs/Lab07")

#Page fault and ip packet data [getting number of seconds per occurence]
pgfault = read.csv("data/pgfault_data.csv",header = TRUE) %>%
  mutate(time = as.integer(time)) %>%
  group_by(time) %>%
  summarise(pgfaults = max(total_pgfaults) - min(total_pgfaults))
ggplot(data = pgfault,aes(x=pgfaults))+geom_histogram()
#Comment: page faults are integer valued [This is number of page faults per second]. Poisson is the candidate here

packetData = read.csv("data/packet_data.csv",header = TRUE) %>%
  mutate(time = as.integer(time)) %>%
  group_by(time) %>%
  summarise(packets = max(total_packets) - min(total_packets))
ggplot(data = packetData,aes(x=packets))+geom_histogram(binwidth = .1)
#Comment: number of packets per second. Poisson is the candidate
#For discrete that takes on only integer values, we will create
M = nrow(packetData)
meanEst = mean(packetData$packets)
maxObs = max(packetData$packets)
vizPacketData <- packetData %>%
  group_by(packets) %>%
  summarise(p = n()/M) %>%
  rbind(data.frame(packets = setdiff(0:maxObs,unique(packetData$packets)),
                   p = rep(0,length(setdiff(0:maxObs,unique(packetData$packets)))))) %>%
  mutate(empirical = rep("Estimated Probability",length(p))) %>%
  rbind(data.frame(packets = 0:maxObs,
                   p = dpois(0:maxObs,lambda = meanEst),
                   empirical = rep("Poisson Probability")))
ggplot(data = vizPacketData,aes(x = packets,y=p,fill = empirical))+geom_bar(position="dodge", stat="identity")+
  scale_x_continuous(breaks=seq(0,60,by=1))

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")


pingData = read.csv("data/ping_data.csv",header = TRUE) %>%
  mutate(ping = as.numeric(str_split(ping,pattern = " ",simplify = TRUE)[,1]))
ggplot(data = pingData,aes(x=ping))+geom_histogram()
#Comment: An amount of elapsed time. An appropriately parameterized gamma distribution may be appropriate

tempData = read.csv("data/temp_data.csv",header = TRUE) %>%
  melt(id = c("time")) %>%
  mutate(machine = variable,temp = value)
ggplot(data = tempData,aes(x = temp))+geom_histogram()+facet_wrap(~machine)
#Comment: a highly discretized distribution. This is just silly to analyze because the data is clearly an artifact of the measuring device. 











