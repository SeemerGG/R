# Страна - ТУрция (TUR), дисциплина - Тяжелая атлетика (weightlifting)

#df_summer <- read.csv('summer.csv')
#df_summer$season = rep("summer", each=nrow(df_summer))
#df_winter <- read.csv('winter.csv')
#df_winter$season = rep("winter", each=nrow(df_winter))
#df <- rbind(df_winter, df_summer)


# Import data from 'MAN.csv' and 'NOMAN.csv'

df_man <- read.csv('MAN.csv', encoding = 'UTF-8')
df_noman <- read.csv('NOMAN.csv', encoding = 'UTF-8')

#Create column diagrams
par(mfrow=c(1,2))
barplot(sapply(df_man[1:nrow(df_man), 2:ncol(df_man)],sum), type= 'h', col = 'lightblue',
        ylab="Количество спортсменов", xlab = "Место", main = "Мужчины")
barplot(sapply(df_noman[1:nrow(df_noman), 2:ncol(df_noman)],sum), type= 'h', col = "lightblue",
        ylab="Количество спортсменов", xlab = "Место", main = "Женщины")

#Create pie-diagrams 


par(opar)