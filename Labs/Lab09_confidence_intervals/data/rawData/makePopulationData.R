library(tidyverse)
library(ggplot2)


setwd("~/xxx/spring2022/teaching/CS3130rCurriculumDev/Labs/Lab09_confidence_intervals")
rawDat <- read.csv("data/rawData/atusresp_2020.dat") %>%
  select("TRTFRIEND","TRTSPONLY","TRTCCC",
         "TRTALONE","TRTFAMILY","TRCHILDNUM",
         "TRERNWA","TRERNHLY","TUCC2","TUCC4")
N = 200

#Save the 'population' data for the columns that need to be modified, print the 'population' means (and sample sizes for those columns that are modified), and save the N sampled data coming from the empirical Cdfs
#If genNewSampleData = TRUE, this function will overwrite the sample data frame that the students will use. Otherwise, it will just print relevant information about the 'population'.
cleanDat <- function(N,rawDat,genNewSampleData) {

  #First do the sampling for the non NA data
  nonNAdataPOP = dat %>%
    select(TRTFRIEND:TRCHILDNUM) 
  write.csv(nonNAdataPOP,"data/populationData/nonNAdata.csv")
  nonNAdataPOPmeans <- apply(nonNAdataPOP,MARGIN = 2, FUN = mean)
  nRow = nrow(nonNAdataPOP)
  
  #Empirical CDF sampling so it is done with replacement
  sampledData <- nonNAdata[sample(1:nRow,size = N, replace = TRUE),]
  
  #Now for the other variables we have to remove NAs
  hourlyPOP = dat$TRERNHLY[which(dat$TRERNHLY > -1)]/100
  hourlyPOPmean = mean(hourlyPOP)
  write.csv(hourlyPOP,"data/populationData/TRERNHLYadjusted.csv")
  hourlySamples = hourlyPOP[sample(1:length(hourlyPOP),size = N,replace = TRUE)]
  weeklyPOP = dat$TRERNWA[which(dat$TRERNWA > -1)]/100
  write.csv(weeklyPOP,"data/populationData/TRERNWAadjusted.csv")
  weeklyPOPmean = mean(weeklyPOP)
  weeklySamples = weeklyPOP[sample(1:length(weeklyPOP),size = N,replace = TRUE)]
  
  #And for the two child variables 
  wakeTimePOP = dat$TUCC2[which(dat$TUCC2 != "-2" & dat$TUCC2 != "-1" & dat$TUCC2 != "-3")]
  wakeTimePOP = wakeTimePOP %>%
    str_split(pattern = ":",simplify = TRUE) %>%
    as.data.frame() %>%
    apply(MARGIN = 2, as.numeric) %>%
    as.data.frame() %>%
    mutate(hour = V1*60, minute = V2, wakeTime = hour+minute)
  wakeTimePOP = wakeTimePOP$wakeTime
  write.csv(wakeTimePOP,"data/populationData/TUCC2adjusted.csv")
  wakeTimePOPmean = mean(wakeTimePOP)
  wakeTimeSamples = wakeTimePOP[sample(1:length(wakeTimePOP),size = N,replace = TRUE)]
  
  sleepTimePOP = dat$TUCC4[which(dat$TUCC4 != "-2" & dat$TUCC4 != "-1" & dat$TUCC4 != "-3")]
  sleepTimePOP = sleepTimePOP %>%
    str_split(pattern = ":",simplify = TRUE) %>%
    as.data.frame() %>%
    apply(MARGIN = 2, as.numeric) %>%
    as.data.frame() %>%
    mutate(hour = V1*60, minute = V2, sleepTime = hour+minute)
  sleepTimePOP = sleepTimePOP$sleepTime
  write.csv(sleepTimePOP,"data/populationData/TUCC4adjusted.csv")
  sleepTimePOPmean = mean(sleepTimePOP)
  sleepTimeSamples = sleepTimePOP[sample(1:length(sleepTimePOP),size = N,replace = TRUE)]
  
  sampledData$TRERNHLY = hourlySamples
  sampledData$TRERNWA = weeklySamples
  sampledData$TUCC2 = wakeTimeSamples
  sampledData$TUCC4 = sleepTimeSamples
  if (genNewSampleData) {
    write.csv(sampledData,"data/ATUS_sampled_data_N.csv")
  }
  #Now we will print population level information
  print(nonNAdataPOPmeans)
  
  #And for the adjusted vectors
  adjustedVariables = c("TRERNHLY","TRERNWA","TUCC2","TUCC4")
  mns = c(hourlyPOPmean,weeklyPOPmean,wakeTimePOPmean,sleepTimePOPmean)
  ns = c(length(hourlyPOP),length(weeklyPOP),length(wakeTimePOP),length(sleepTimePOP))
  for (i in 1:4) {
    print(str_c("For the variable ",adjustedVariables[i]," N = ",ns[i]," and mu = ",mns[i]))
  } 
}

cleanDat(N,rawDat,TRUE)
