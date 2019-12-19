# Machine Learning - https://machinelearningmastery.com/machine-learning-in-r-step-by-step/


data(midhbase)
view(midhbase)
dataset <- midhbase

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
view(dataset)


dim(dataset)

# list types for each attribute
sapply(dataset, class)
# take a peek at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(dataset$Species)

# Class Distribution
# Let's now take a look at the number of instances (rows) that belong to each class. We can view this as an absolute count and as a percentage.

# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
summary(dataset)


# split input and output
x <- dataset[,1:4]
y <- dataset[,5]


# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}


# barplot for class breakdown
plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse") # Muito legal

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")


# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
###########################################################################
############ Otra forma de hacer ML - https://rpubs.com/Joaquin_AR/383283

library(tidyverse)
library(titanic)
datos <- titanic_train
view(datos)

# Resumen del set de datos
glimpse(datos)

# Aunque esta variable está codificada como 1 si el pasajero sobrevivió y 0 si murió, no conviene 
#almacenarla en formato numérico, ya que esto puede llevar a errores como el de tratar de calcular su media. Para evitar este tipo de problemas, se recodifica la variable para que sus dos posibles niveles sean "Si"-"No" y se convierte a factor


datos$Survived <- if_else(datos$Survived == 1, "Si", "No")
datos$Survived <- as.factor(datos$Survived)

# La variable Pclass es cualitativa ordinal, es decir, toma distintos valores cualitativos ordenados 
# siguiendo una escala establecida

datos$Pclass <- as.factor(datos$Pclass)

# Las variables SibSp y Parch son cuantitativas discretas. Para este estudio exploratorio, dado que solo toman unos pocos valores, se decide almacenarlas como factor

datos$SibSp <- as.factor(datos$SibSp)
datos$Parch <- as.factor(datos$Parch)

# Las variables Sex y Embarked también se convierten a tipo factor.
datos$Sex      <- as.factor(datos$Sex)
datos$Embarked <- as.factor(datos$Embarked)

# Como el set de datos no es lo suficientemente grande como para dar problemas de memoria, se crea un segundo dataframe con esta estructura
datos_long <- datos %>% gather(key = "variable", value = "valor", -PassengerId)
head(datos_long)
head(datos)

# Número de observaciones del set de datos
nrow(datos)

# Detección si hay alguna fila incompleta
any(!complete.cases(datos))

# Número de datos ausentes por variable
map_dbl(datos, .f = function(x){sum(is.na(x))})

# Se procede a identificar qué variables contienen valores "", lo q es distinto de NA
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})

# La variable Cabin está almacenada como character
datos$Cabin[datos$Cabin == ""] <- NA
levels(datos$Embarked)
datos$Embarked <- as.character(datos$Embarked)
datos$Embarked[datos$Embarked == ""] <- NA
datos$Embarked <- as.factor(datos$Embarked)
levels(datos$Embarked)

# Este cambio también se aplica al dataframe datos_long
datos_long$valor[datos_long$valor == ""] <- NA

# Número de datos ausentes por variable
map_dbl(datos, .f = function(x){sum(is.na(x))})

# Representación gráfica de los datos ausentes
datos_long <- datos_long %>%  mutate(ausente = is.na(valor))
ggplot(data = datos_long, aes(x = variable, y = PassengerId, fill = ausente)) +
  geom_raster() +
  scale_fill_manual(values = c("gray60", "orangered2")) +
  theme_bw() +
  labs(title = "Valores ausentes por variable") +
  theme(legend.position = "bottom")

# Porcentaje valores ausentes por variable
datos_long %>%
  group_by(variable) %>% 
  summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>%
  ggplot(aes(x = reorder(variable, desc(porcentaje_NA)), y = porcentaje_NA)) +
  geom_col() +
  labs(title = "Porcentaje valores ausentes por variable",
       x = "Variable", y = "Porcentaje NAs") +
  theme_bw()
# Distribuicion de variables respouesta
ggplot(data = datos, aes(x = Survived, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Supervivencia") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias 
table(datos$Survived)
# Proporcion
prop.table(table(datos$Survived)) %>% round(digits = 2)

# Porcentaje de aciertos si se predice para todas las observaciones que no sobrevivieron.
# dado que el 62% de los pasajeros fallecieron, si siempre se predice 
# Survived = No, el porcentaje de aciertos será aproximadamente del 62%
n_observaciones <- nrow(datos)
predicciones <- rep(x = "No",  n_observaciones)
mean(predicciones == datos$Survived) * 100

# Distribución de variables continuas
# Analizando los datos de esta forma, se pueden empezar a extraer ideas sobre qué variables están más relacionadas con la supervivencia.
# Como el objetivo del estudio es predecir qué pasajeros sobrevivieron y cuáles no, el análisis de cada variable se hace en relación a la variable respuesta Survived.

library(ggpubr)
p1 <- ggplot(data = datos, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = Age, color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Age", size = 15))
final_plot

# Estadísticos de la edad de los supervivientes y fallecidos
datos %>% filter(!is.na(Age)) %>% group_by(Survived) %>%
  summarise(media = mean(Age),
            mediana = median(Age),
            min = min(Age),
            max = max(Age))

# Creando una nueva variable. Esta nueva variable se analizará junto con el resto de variables cualitativas.
# Cuando la información de una variable continua reside en si se superan o no determinados límites, 
# los modelos predictivos suelen conseguir mejores resultados si la variable se discretiza en intervalos
datos <- datos %>%
  mutate(Age_grupo = case_when(Age <= 10  ~ "niño",
                               Age > 10 & Age <= 60  ~ "adulto",
                               Age > 60 ~ "anciano"))
datos$Age_grupo <- as.factor(datos$Age_grupo)

#Esta nueva variable se analizará junto con el resto de variables cualitativas.

p1 <- ggplot(data = datos, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = Fare, color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Fare", size = 15))
final_plot

# Estadísticos del precio del billete de los supervivientes y fallecidos
datos %>% filter(!is.na(Fare)) %>% group_by(Survived) %>%
  summarise(media = mean(Fare),
            mediana = median(Fare),
            min = min(Fare),
            max = max(Fare))

# La variable Fare tiene una distribución asimétrica, muchos billetes tenían un coste bajo y unos pocos
# un coste alto. Este tipo de distribución suele visualizarse mejor tras una trasformación logarítmica.

p1 <- ggplot(data = datos, aes(x = log(Fare), fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = datos, aes(x = Survived, y = log(Fare), color = Survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Log(Fare)", size =15))
final_plot

# Distribución de variables cualitativas

ggplot(data = datos, aes(x = Pclass, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Pclass") +
  theme_bw() +
  theme(legend.position = "bottom")
# Tabla de frecuencias relativas de supervivientes por clase
prop.table(table(datos$Pclass, datos$Survived), margin = 1) %>% round(digits = 2)
# Graficando
ggplot(data = datos, aes(x = Sex, y = ..count.., fill = Survived)) +
  geom_bar() +
  labs(title = "Sex") +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por sexo
prop.table(table(datos$Sex, datos$Survived), margin = 1) %>% round(digits = 2)
ggplot(data = datos, aes(x = SibSp, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "SibSp") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por número de familiares
prop.table(table(datos$SibSp, datos$Survived), margin = 1) %>% round(digits = 2)

ggplot(data = datos, aes(x = Parch, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Parch") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por Parch
prop.table(table(datos$Parch, datos$Survived), margin = 1) %>% round(digits = 2)

# Para pasar de factor a numeric primero se convierte a character
datos$SibSp <- as.character(datos$SibSp)
datos$SibSp <- as.numeric(datos$SibSp)
datos$Parch <- as.character(datos$Parch)
datos$Parch <- as.numeric(datos$Parch)

ggplot(data = datos, aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Embarked") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por puerto de embarque
prop.table(table(datos$Embarked, datos$Survived), margin = 1) %>% round(digits = 2)

ggplot(data = datos, aes(x = Age_grupo, y = ..count.., fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Age_grupo") +
  theme_bw() +
  theme(legend.position = "bottom")

# Tabla de frecuencias relativas de supervivientes por grupo de edad
prop.table(table(datos$Age_grupo, datos$Survived), margin = 1) %>% round(digits = 2)

# Correlación entre variables continuas

cor.test(x = datos$Age, y = datos$Fare, method = "pearson")


ggplot(data = datos, aes(x = Age, y = log(Fare))) +
  geom_point(color = "gray30") +
  geom_smooth(color = "firebrick") +
  theme_bw()