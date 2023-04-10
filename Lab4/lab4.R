# Страна - ТУрция (TUR), дисциплина - Тяжелая атлетика (weightlifting)

#df_summer <- read.csv('summer.csv')
#df_summer$season = rep("summer", each=nrow(df_summer))
#df_winter <- read.csv('winter.csv')
#df_winter$season = rep("winter", each=nrow(df_winter))
#df <- rbind(df_winter, df_summer)


# Import data from 'MAN.csv' and 'NOMAN.csv'
setwd('C:/Users/Владимир/Documents/GitHub/R/Lab4')
df_man <- read.csv('./MAN.csv', encoding = 'UTF-8')
df_noman <- read.csv('./NOMAN.csv', encoding = 'UTF-8')

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
par(mfrow=c(1,1))
plot(df_man$Год, df_man$X1+df_man$X2+df_man$X3, type='b', pch = 19, main = "Тенденция изменения призовых мест", col="green", ylab="Количество призовых мест",
      xlab="Год")
lines(df_noman$Год, df_noman$X1+df_noman$X2+df_noman$X3, type='b', pch = 19, col="blue")
axis(side = 1, at = df_man$Год)
legend('topleft', legend= c('Мужчины', 'Женщины'), fill = c("green","blue"))
#Task 4
#Read data about last seven game
df_game_1 <- read.csv('7OlumpycGame1.csv', encoding = 'UTF-8')
df_game_1_3 <- read.csv("7OlimpycGame1-3.csv", encoding = "UTF-8")
countries <- colnames(df_game_1)[2:length(df_game_1)]
#Create a diagram with 7 lines based on data from df_game_1
plot(df_game_1$Год, df_game_1$США, type='b', pch = 19, main = "График изменения спортивных достижений по золотым медалям",
     col = "yellow", xlab= "Год", ylab = "Количество медалей")
lines(df_game_1$Год, df_game_1$Китай, type='b', pch = 19, col = 'red')
lines(df_game_1$Год, df_game_1$Россия, type='b', pch = 19, col = 'violet')
lines(df_game_1$Год, df_game_1$Германия, type='b', pch = 19, col = 'green')
lines(df_game_1$Год, df_game_1$Нидерланды, type='b', pch = 19, col = 'grey')
lines(df_game_1$Год, df_game_1$Норвегия, type='b', pch = 19, col = 'pink')
lines(df_game_1$Год, df_game_1$Великобритания, type='b', pch = 19, col = 'blue')
legend('top',horiz = TRUE,cex = 0.55,xpd = TRUE, inset = c(0, -.06),legend = countries, fill = c('yellow', 'red', 'violet', 'grey', 'pink', 'blue'))
#Create a diagram with 7 lines based on data from df_game_1_3
plot(df_game_1_3$Год, df_game_1_3$США, type='b', pch = 19, main = "График изменения спортивных достижений по призовым местам",
     col = "yellow", xlab= "Год", ylab = "Количество медалей")
lines(df_game_1_3$Год, df_game_1_3$Китай, type='b', pch = 19, col = 'red')
lines(df_game_1_3$Год, df_game_1_3$Россия, type='b', pch = 19, col = 'violet')
lines(df_game_1_3$Год, df_game_1_3$Германия, type='b', pch = 19, col = 'green')
lines(df_game_1_3$Год, df_game_1_3$Нидерланды, type='b', pch = 19, col = 'grey')
lines(df_game_1_3$Год, df_game_1_3$Норвегия, type='b', pch = 19, col = 'pink')
lines(df_game_1_3$Год, df_game_1_3$Великобритания, type='b', pch = 19, col = 'blue')
legend('top',horiz = TRUE,cex = 0.55,xpd = TRUE, inset = c(0, -.06),legend = countries, fill = c('yellow', 'red', 'violet', 'grey', 'pink', 'blue'))
#Task 5
#Create three diagrams based on data from MANlast6game.csv and NOMANlast6game.csv
df_man <- read.csv('MANlast6game.csv', encoding = 'UTF-8')
df_noman <- read.csv('NOMANlast6game.csv', encoding = 'UTF-8')
par(mfrow = c(1, 3))
#Create a line's diagram
plot(df_man$Год, df_man$X1+df_man$X2+df_man$X3, type='b', pch = 19, main = 
       "Тенденция изменения призовых мест \n сборной Турции в тяжелой атлетике \n за последние 6 ОИ",
     col="black", ylab="Количество призовых мест",
     xlab="Год")
lines(df_noman$Год, df_noman$X1+df_noman$X2+df_noman$X3, type='b', pch = 19, col="pink")
axis(side = 1, at = df_man$Год)
legend('topleft',horiz = TRUE, legend= c('Мужчины', 'Женщины'), fill = c("black","pink"))
#Create a column's diagram
table_results <- matrix(data = c(df_man$X1+df_man$X2+df_man$X3, df_noman$X1+df_noman$X2+df_noman$X3),
       nrow = 2, ncol = 6, byrow = TRUE)
rownames(table_results) = c("Мужчины", "Женщины")
colnames(table_results) = df_man$Год
barplot(table_results, main = "Количество призовых мест Турции в тяжелой \n атлетике за последнии 6 ОИ",
        beside = TRUE,col = c('black', 'pink'))
#Create a pie diagram 
pie(c(sum(table_results[1,1:6]), sum(table_results[2,1:6])),c("Мужчины", "Женщины"), main = 
"Общее количество призовых мест сборной \n Турции в тяжелой атлетике \n за последнии 6 ОИ", 
col = c('black', 'pink'))




#experiments
#sapply(df_man[1:nrow(df_man), 2:ncol(df_man)],sum)
#buf_man$X1+buf_man$X2+buf_man$X3
#sapply(df_man[1:nrow(df_man), 2:4],sum)
#df_man[2:ncol(df_man)]

