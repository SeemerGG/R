install.packages(packages)
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('tibble')
install.packages('vctrs')
install.packages('tibble')
install.packages("vctrs")
install.packages('tibble', repos = 'http://cran.rstudio.com/', type = 'source')
install.packages('klaR')
install.packages('party')
install.packages('randomForest')
install.packages('lattice')

install.packages('MASS')
install.packages('grid')
install.packages('mvnorm')
install.packages('modeltools')
install.packages('stats4')
install.packages('strucchange')
install.packages('zoo')

library(tidyverse)
library(scatterplot3d)
library(reshape2)
library(plotly)
library(cluster)
library(factoextra)
library(NbClust)
library(dendextend)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gridExtra)
library(knitr)
#Part 1

data <- read.csv('student-mat.csv')
summary(data)

ggplot(data, aes(x = absences)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Гистограмма школьных прогулов", x = "Количество", y = "Частота") +
  theme_minimal()

ggplot(data, aes(x = studytime)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Гистограмма времени уделяемого учебе", x = "Часы 1*10ч", y = "Частота") +
  theme_minimal()

ggplot(data, aes(x = failures)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Гистограмма отказов", x = "Количество", y = "Частота") +
  theme_minimal()

boxplot(data[,c('G1', 'G2', 'G3')], main='Распределение оценок')

#Построение дендограммы 
#Нормируем
numeric_data <- data %>% select_if(is.numeric)

data_scaled <- scale(numeric_data)

#Выполняем иерархическую кластеризацию
dist_matrix <- dist(data_scaled, method = "euclidean")

#Выбираем количество групп. Строим локтевую диаграмму.

fviz_nbclust(numeric_data, kmeans, method = "wss")

k<- 5
#Строим дендограмму выделяем к групп
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, hang = -1, main="Дендрограмма характеристик студентов")
rect.hclust(hc, k = k, border = "red")
groups <- cutree(hc, k)
#Расчитываем средние значения для каждой группы
numeric_data$Group <- as.factor(groups)
group_means <- aggregate(. ~ Group, numeric_data, mean)

#Преобразуем фрейм в формат long
group_means_long <- melt(group_means, id.vars = "Group")

ggplot(group_means_long, aes(x = Group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Номер группы") +
  ylab("Среднее значение") +
  ggtitle("Среднии значения характеристик для каждой группы") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Диаграмма каменная осыпь 
height_values <- as.data.frame(hc$height)
height_values$groups <- nrow(height_values):1
colnames(height_values) <- c("height", "groups")
ggplot(height_values, aes(x = groups, y = height)) +
  geom_point() +
  geom_line() +
  xlab("Номер группы") +
  ylab("Высота") +
  ggtitle("Каменная осыпь") +
  theme_minimal()
#Построение scatterplot (k-means кластеризация)

ggplot(numeric_data, aes(x = G1, y = G2, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для G1 и G2", x = "G1", y = "G2") +
  theme_minimal()

ggplot(numeric_data, aes(x = G1, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для G1 и G3", x = "G1", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = G2, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для G2 и G3", x = "G2", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = Dalc, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для Dalc и G3", x = "Dalc", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = Walc, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для Walc и G3", x = "Walc", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = absences, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для absenses и G3", x = "absenses", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = studytime, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Диаграмма рассеивания для studytime и G3", x = "studytime", y = "G3") +
  theme_minimal()

#3Д рассеивания 
colors <- as.integer(numeric_data$Group)
scatterplot3d(x = numeric_data$G1, y = numeric_data$G2, z = numeric_data$G3,
              color = colors, pch = 19,
              xlab = "G1", ylab = "G2", zlab = "G3",
              main = "3D Диаграмма рассеивания G1, G2, и G3")


#Часть 2
data$Group <- as.factor(groups)
#Делим данные на обучающие и тестовые 
set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#оБУчаем наивного баеса
nb_model <- naiveBayes(Group ~ ., data = train_data)
#Прогнозируем
test_data$predicted_group <- predict(nb_model, test_data)
#Вычисляем точность
confusion_matrix <- confusionMatrix(test_data$predicted_group, test_data$Group)
accuracy <- confusion_matrix$overall['Accuracy']
print(accuracy)
print(confusion_matrix)
#Реализация деревьев решений
#Точность текстовых 
dt_model <- rpart(Group ~ ., data = train_data, method = "class")
print(dt_model)
test_data$predicted_group_dt <- predict(dt_model, test_data, type = "class")
confusion_matrix_dt <- confusionMatrix(test_data$predicted_group_dt, test_data$Group)
accuracy_dt <- confusion_matrix_dt$overall['Accuracy']
print(accuracy_dt)
print(confusion_matrix_dt)
#
rpart.plot(dt_model, type = 4, extra = 101, tweak = 1.2, main = "Древо решений")


#Случайный лес
rf_model <- randomForest(Group ~ ., data = train_data, importance = TRUE)
test_data$predicted_group_rf <- predict(rf_model, test_data)
confusion_matrix_rf <- confusionMatrix(test_data$predicted_group_rf, test_data$Group)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
print(accuracy_rf)
print(confusion_matrix_rf)

# Вывод случайного веса
rf_var_imp <- importance(rf_model)
rf_var_imp_df <- data.frame(Feature = rownames(rf_var_imp), Importance = rf_var_imp[, 1])
ggplot(rf_var_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "График важности переменной в случайном лесу", x = "Особенность", y = "Коэффициент важности") +
  theme_minimal() +
  coord_flip()

#Сравнение 
cat("Naive Bayes Accuracy:", accuracy, "\n")
cat("Decision Tree Accuracy:", accuracy_dt, "\n")
cat("Random Forest Accuracy:", accuracy_rf, "\n")