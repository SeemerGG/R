library(purrr) #подключаем пакет для работы функции map
path = paste(getwd(),"/GitHub/R/Lab2/answers.csv", sep = "") #получаем путь к файлу с данными
data <- data.frame(read.csv(path)) #создаем объект data frame
colMeans(data[seq(3, ncol(data))], na.rm = TRUE, dims = 1) #вычисляем mean по каждому столбцу
map(data[seq(3, ncol(data))],function(x) max(x, na.rm = TRUE)) #получаем вектор максимальных значений для каждого столбца
map(data[seq(3, ncol(data))],function(x) min(x, na.rm = TRUE)) #получаем вектор минимальных значений для каждого столбца
map(data[seq(3, ncol(data))], function(x) length(x[x>7])) #количество типо которым фильм понравился
map(data[seq(3, ncol(data))], function(x) length(x[x<3])) # количество людей которым мало наличия Гослинга, чтобы поставить оценку больше 7
sort(colMeans(data[seq(3, ncol(data))], na.rm = TRUE, dims = 1), decreasing = TRUE) #выводим рейтинг фильмов по убыванию
barplot(height = colMeans(data[seq(3, ncol(data))], na.rm = TRUE, dims = 1)) #делаем столбчатую диаграмму