library(dplyr)
library(tidyr)
library(zoo)
setwd("C:\\Users\\Владимир\\Documents\\GitHub\\R\\KR2")

df <- read.table('protein.csv',sep = ";", header=TRUE)

df <- df %>% mutate_all(na_if, "") #пустые ячейки меняю на NA

#df$Ozone[is.na(df$Ozone)]<-mean(df$Ozone, na.rm = T) - замена на среднее в определенном столбце

# замена NA медианными значениями для всех столбцов
for (i in 1:30){
  if (is.na(VVP_growth[i])){
    #VVP_growth[i]=0
    VVP_growth[i]= median(VVP_growth,na.rm=TRUE)
  }
  if (is.na(Population_growth[i])){
    #Population_growth[i]=0
    Population_growth[i]= median(Population_growth,na.rm=TRUE)
  }
  
}

#Проверка на нормальность 
qqnorm(x)
qqline(x, col=4, lwd=2) 

# гистограмма с линией плотности
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
hist(x, freq=FALSE, col="gray", main="Гистограмма по весу", xlab="Значения веса", ylab="Частота")
lines(x2, fun, col=2, lwd=2)

#гистограмма для 3 столбцов which(df[, "Sport"]%in%c("Gymnastics", "Weightlifting"))

