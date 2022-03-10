#Milestone 1

#My name
print("Dhimahi Patel")

pacman:: p_load(FSA,FSAdata,magrittr,dplyr,plotrix,ggplot2,moments, tidyr, tidyverse, plyr)
install.packages("ggplot2")
library(ggplot2)


#Importing dataset
library(readr)
df = read.csv(file= 'D://Winter 22//ALY6010//Milestone 1//air-pollution-death-rate.csv', header=TRUE, sep=",")
view(df)

#Cleaning the data by removing the NA's
df <- data.frame(df)
df <- na.omit(df)


#first and last 3 data of dataset
headtail(df,3)

#Structure of the dataset df
str(df)

#The summary of the dataset
summary(df)

#Descriptive statistics of the dataset
psych::describe(df)  


#Scatter plot

plot(Ozone ~ Outdoor, data = df, las= 1, pch=c(16,4), col=c("skyblue","Red"), 
     cex.main= 1, xlim=c(7,140),ylim=c(0,40),
     cex.axis=0.8, ylab = "Ozone pollution (in million)", 
     xlab ="Outdoor air pollution (in million)", 
     main="Death due to diffirent kind of Air pollution")
legend(100,40,legend = c("Outdoor", "Ozone"), col=c("red", "skyblue"), pch=c(4,16), bty="n")


#Histogram

hist(df$Air.pollution, 
     main="Total death from Air Pollution", 
     xlab="Death (in milloin)", 
     border="blue", 
     col="green", 
     xlim=c(0,350), 
     las=1)


#Box Pot

boxplot(df$Indoor,
        main = "Death due to Indoor Air Pollution",
        xlab = "Death (in milloin)",
        col = "orange",
        border = "brown",
        notch = TRUE,
        horizontal = TRUE)




#Taking a subset from dataset

sample1=filter(df,Entity=="India")
headtail(sample1,3)


#Bar Plot

data <- data.frame(sample1$Year, sample1$Air.pollution)

barplot(height=data$sample1.Air.pollution,names= data$sample1.Year, col="lightblue", 
        main= "Total deaths due to Air Pollution", ylab = "Number of deaths (in millions)", xlab="Year", 
        las=1, cex.names = 0.80, ylim=c(0,250))



#Line graph


plot(sample1$Outdoor,type = "o",col = "red", xlab = "Year", ylab = "Number of death (in millions)",  
     main = "Death rate by Outdoor pollution")

library(gcookbook)

sample2 =filter(df,Entity=="South Korea")
headtail(sample2,3)

ggplot(data = sample1, aes(x = Year))+
  geom_line(aes(y = Ozone ), color = "darkred")+
  geom_line(aes(y = Indoor ), color = "Darkblue")+
  geom_line(aes(y = Outdoor ), color = "green")+
  xlab("Year") + ylab("Death number (in million")+
  labs(title= "Overall Air Pollution effect")
