# Страна - ТУрция (TUR), дисциплина - Тяжелая атлетика (weightlifting)

#df_summer <- read.csv('summer.csv')
#df_summer$season = rep("summer", each=nrow(df_summer))
#df_winter <- read.csv('winter.csv')
#df_winter$season = rep("winter", each=nrow(df_winter))
#df <- rbind(df_winter, df_summer)


# Import data from 'MAN.csv' and 'NOMAN.csv'

df_man <- read.csv('MAN.csv', encoding = 'UTF-8')
df_noman <- read.csv('NOMAN.csv', encoding = 'UTF-8')

#Task 1
#Create column diagrams
par(mfrow=c(1,2))
barplot(sapply(df_man[1:nrow(df_man), 2:ncol(df_man)],sum), type= 'h', col = 'lightblue',
        ylab="Количество спортсменов", xlab = "Место", main = "Мужчины")
barplot(sapply(df_noman[1:nrow(df_noman), 2:ncol(df_noman)],sum), type= 'h', col = "lightblue",
        ylab="Количество спортсменов", xlab = "Место", main = "Женщины")

#Task 2
#Delete not needed rows
buf_man <- subset(df_man, df_man$X1 > 0)
buf_noman <- subset(df_noman, df_noman$X1 > 0)
#Create pie-diagrams 
par(mfrow = c(1,2))
pie(buf_man$X1, buf_man$Год, main = "Количество золотых медалий", col= rainbow(nrow(buf_man)))
legend('topleft', legend= buf_man$Год, fill = rainbow(nrow(buf_man)))
pie(buf_noman$X1, buf_noman$Год, main = "Количество золотых медалий", col= rainbow(nrow(buf_noman)))
legend('topleft', legend= buf_noman$Год, fill = rainbow(nrow(buf_noman)))
#Task 3
#Create diagram with two line
plot(df_man$Год, df_man$X1+df_man$X2+df_man$X3, type='b', pch = 19, main = "Тенденция изменения призовых мест", col="green", ylab="Количество призовых мест",
      xlab="Год", fill = 'lightgeen')
lines(df_noman$Год, df_noman$X1+df_noman$X2+df_noman$X3, type='b', pch = 19, col="blue")
axis(side = 1, at = df_man$Год)
legend('topleft', legend= df_man$Год, fill = rainbow(nrow(buf_noman)))
#Task 4


buf_man$X1+buf_man$X2+buf_man$X3
sapply(df_man[1:nrow(df_man), 2:4],sum)
df_man[2:ncol(df_man)]
par(opar)
