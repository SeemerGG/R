#install.packages("xlsx")
#library(xlsx)
#dataXml <- read.xlsx("answers.xlsx")
#install.packages("dplyr")

#library(dplyr)
dataCsv <- read.csv("answers.csv", encoding = "UTF-8") #создание фрейма

print(summary(dataCsv)) #производим дискриптивный анализ
boxplot(dataCsv[1:10, 3:ncol(dataCsv)], col = rainbow(10))# строим коробку с усами определяем медиану, нижний ус, первый квартиль, третитий квартиль, и верхний ус

sortedData <- sort(sapply(dataCsv[1:10, 3:ncol(dataCsv)], mean, na.rm = TRUE), decreasing = TRUE)
hist(sortedData,main = "Mean",xlab="Значения",ylab="Количество", col="#000000") #сортировка по выбраному признаку

dataDrive <- subset(dataCsv, Драйв %in% c(8, 9, 10))
print(dataDrive) # сформировал отдельный набор данных по признаку (Оценка для фильма Драйв от 8 до 10)

print(dim(dataDrive)) #подсщитал размерность новой таблицы 
print(summary(dataDrive)) #изучение новых данных 

sortedDrive <- sort(sapply(dataDrive[1:10, 3:ncol(dataDrive)], mean, na.rm = TRUE), decreasing = TRUE)
hist(sortedDrive,main = "Mean",xlab="Значения",ylab="Количество", col="#000000")
boxplot(dataDrive[1:10, 3:ncol(dataDrive)], col = rainbow(10))
#Конец лабы


hist(dataCsv$Бегущий.по.лезвию.2049, breaks = 10) #строим гистограммы определяем моды по моды (не надо это делать на защите не пригодилось)
hist(dataCsv$Драйв, breaks = 10)
hist(dataCsv$Игра.на.понижение, breaks = 10)
hist(dataCsv$Ла.Ла.Ленд, breaks = 10)
hist(dataCsv$Дневник.памяти, breaks = 10)
hist(dataCsv$Славные.парни, breaks = 10)
hist(dataCsv$Место.под.соснами, breaks = 10)
hist(dataCsv$Эта.дурацкая.любовь, breaks = 10)
hist(dataCsv$Фанатик, breaks = 10)
hist(dataCsv$Ларс.и.настоящая.девушка, breaks = 10)

dataCsv <- sorbox2