#7.Известно, что в таблице хранятся показатели по 3 странам за 5 лет (см. рис).
#Создайте вектор, который мог бы послужить столбцом country в этой
#таблице. Создайте вектор, который мог бы послужить столбцом year в этой таблице.
df <- data.frame(country = sort(rep(c('Italy', 'France', 'Spain'), 5)))
df$year <- seq.Date(as.Date('2000/01/01'), as.Date('2004/01/01'), by="year") 
#21.Создать 5 векторов с характеристиками автомобилей – вес, емкость двигателя, тип коробки
#передач, максимальная скорость, тип кузова. Создать из нах data.frame, в качестве названий
#присвоить марки автомобилей. Выбрать только автомбили с одинаковым объемом двигателя,
#упорядочить их по алфавиту. Определить число строк и столбцов созданного набора данных.
ves <- sample(seq(1000, 5000, by=100), 5)
volume <- sample(seq(1.8, 4, by=0.5),5)
typeBox <- sample(c("auto", "meh"), 5, replace = TRUE)
maxSpeed <- sample(seq(160, 280, by = 10), 5)
typeCase <- sample(c('sedan', 'cupe', 'pickap', 'hetchback'), size = 5, replace = TRUE)
cars <- data.frame(name = c('toyota tigr', 'jip longbow', 'tayota maracasi', 'espaniola radrigis', 'espaniola gonsales'),ves = ves, volume = volume, typeBox = typeBox, maxSpeed = maxSpeed, typeCase = typeCase)
#library(data.table)
#cars <- data.table(cars)
#cars[1,3] <- 1.8
print(sort(cars$name[cars$volume == cars$volume[anyDuplicated(cars$volume)]]))
print(nrow(cars))
print(length(cars))

