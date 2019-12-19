rm(list=ls())
cat("\014") ## limpia la pantalla del R
# Importar base de datos
idhbase <- read.csv("~/Documentos/Datos/IDHBrasil/idhbase.csv")
View(idhbase)

# K Vecinos mas cercanos
library(DMwR)
# knnImputation(df,k=1) https://conocemachinelearning.wordpress.com/2017/06/30/valoresfaltantes/
idhbase <- knnImputation(idhbase,k=1)
View(idhbase)
#FIM K Vecinos mas cercanos

# Mostreo da base de datos
n<-500 # 500 lineas de muestra
midhbase<- sample(1:nrow(idhbase),size=n,replace=FALSE)
midhbase
# Assignar muestreo a dataframe
midhbase<- idhbase[midhbase, ]
View(midhbase)
head(midhbase)
dim(midhbase)
str(midhbase)
# FIM Mostreo da base de datos

# Operaciones simples
mean(midhbase$homemtot)
mean(midhbase$pren20ricos)
sd(midhbase$espvida)
sd(midhbase$pren20ricos)
hist(midhbase$pren20ricos)
hist(midhbase$espvida)
save(midhbase, file = "midhbase.RData") # Grabar una base de datos en formato RData de un data frame
save(idhbase, file = "idhbase.RData") # Grabar una base de datos en formato RData de un data frame

# Sin ninguna variable explicativa

lm1 <- lm(espvida~1, data = midhbase)
lm1
summary(lm1)

# FIM Sin ninguna variable explicativa

# Prediccion com todas as variables

lm2 <- lm(espvida~., data = midhbase)
lm2
summary(lm2)

# FIM Prediccion com todas as variables


# Las elegidas
lmf <- step(lm1, scope = list(lower=lm1, upper=lm2), direction = "forward")
summary(lmf)
# Fim Las Elegidas

# El Modelo

#  lm(formula = espvida ~ sobre60 + rpob + mort1 + ano + rdpc1 + 
#       t_banagua + t_luz + fectot + t_mulchefefif014 + rdpc10 + 
#       pesorur + idhm + emp, data = midhbase)

#Residuals:
#  Min       1Q   Median       3Q      Max 
# -2.37044 -0.53740 -0.01852  0.53497  2.46236 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -1.787e+02  1.758e+01 -10.163  < 2e-16 ***
#  sobre60           3.095e-01  1.638e-02  18.896  < 2e-16 ***
#  rpob             -1.635e-03  3.812e-03  -0.429 0.668130 - Sacado del modelo    
# mort1            -9.454e-02  6.433e-03 -14.696  < 2e-16 ***
#  ano               1.127e-01  9.008e-03  12.508  < 2e-16 ***
#  rdpc1             8.889e-03  1.650e-03   5.386 1.13e-07 ***
#  t_banagua         1.977e-02  3.350e-03   5.902 6.74e-09 ***
#  t_luz            -7.643e-03  3.767e-03  -2.029 0.043009 *  
#  fectot            2.282e-01  6.629e-02   3.442 0.000627 ***
#  t_mulchefefif014 -1.942e-02  6.093e-03  -3.187 0.001528 ** 
#  rdpc10            1.927e-04  6.851e-05   2.813 0.005101 ** 
#  pesorur          -7.572e-06  3.280e-06  -2.308 0.021398 *  
#  idhm             -3.981e-04  2.117e-04  -1.880 0.060683  - Sacado del modelo  
# emp              -5.271e-02  3.722e-02  -1.416 0.157389     - Sacado del modelo
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.8338 on 486 degrees of freedom
# Multiple R-squared:  0.9769,	Adjusted R-squared:  0.9762 
# F-statistic:  1579 on 13 and 486 DF,  p-value: < 2.2e-16

# FIM El Modelo

# Testando el modelo Final

  lmf <- lm(formula = espvida ~ sobre60 + mort1 + ano + rdpc1 + 
       t_banagua + t_luz + fectot + t_mulchefefif014 + rdpc10 + 
       pesorur, data = midhbase)

lmf
summary(lmf) # 97,66% de certesa

# FIM Testando el modelo

# Sobre la Flexibilidad y interpretabilidad de los metodos, SVM - Suporte Vetor Machines e muy flexible e poco interpretable
# Bagging, Boosting tambien es muy flexible e um poco mas interpretable
# Trees es meio flexible e meio interpetable
# Least Square es poco flexible e ´mas interpretable
# Lasso es poco flexible e muy interpretable



# Tree
tree(midhbase, split = c("deviance", "gini")) 


# FIM Tree

# Boosting
library(gbm)
# FIM Boosting

# GLM

glm1 <- glm(espvida~1., data = midhbase)
glm2 <- glm(espvida~., data = midhbase)
glft <- step(glm1, scope = list(lower=glm1, upper=glm2), direction = "forward")
summary(glft)

glft <- glm(formula = espvida ~ sobre60  + ano + rpob + t_fora6a14 + 
      rdpc + t_m15a17cf + rind + agua_esgoto + t_m10a14cf + mort5 + 
      p_formal + mulhertot + theiltrab + ppob, data = midhbase)
summary(glft)

glft <- glm(formula = espvida ~ sobre60  + ano + 
              rdpc + t_m15a17cf + mort5 + 
              p_formal + mulhertot + ppob, data = midhbase)
summary(glft)
glft

# FIM GLM



# Selecion de un modelo com forward
library(leaps)
regfit.fwd=regsubsets(espvida~.- município, data=midhbase,method="forward") 
plot(regfit.fwd,scale="r2")
summary(regfit.fwd) 
coef(regfit.fwd,8)
# FIM Selecion de un modelo com forward

#

lm.fit = lm(espvida~., data=midhbase)
lm.pred = predict(lm.fit, newdata = midhbase)
mean((midhbase[, "espvida"] - lm.pred)^2) ## 1438114
error.mco <- mean((midhbase[, "espvida"] - lm.pred)^2)
error.mco ## 1438114

# FIM


set.seed(111)
train.size = dim(midhbase)[1] / 2
train = sample(1:dim(midhbase)[1], train.size)
test = -train
datos.train = midhbase[train, ]
datos.test = midhbase[test, ]

nvariables <- as.numeric(dim(midhbase)[2] -1)
x=model.matrix(espvida~.,midhbase)[,-1]
y=midhbase$espvida
y.test=y[test]

### RLASSO con penalizaci�n independiente de los datos ### 
lasso.reg = rlasso(y[train]~x[train,],post=FALSE) # use lasso, not-Post-lasso, lamda independiente de 
sum.lasso <- summary(lasso.reg, all=FALSE)
sum.lasso
yhat.lasso.new = predict(lasso.reg, newdata=x[test,]) #out-of-sample prediction
error.rlasso <- mean((yhat.lasso.new-datos.test[, "Apps"] )^2)
error.rlasso # error absoluto

### RLASSO con penalizaci�n dependiente de los datos ###
lasso.reg.dep = rlasso(y[train]~x[train,],post=FALSE, X.dependent.lambda = TRUE) # use lasso, not-Post-lasso
sum.lasso.dep <- summary(lasso.reg.dep, all=FALSE)
sum.lasso.dep 
yhat.lasso.dep.new = predict(lasso.reg.dep, newdata=x[test,]) #out-of-sample prediction
error.rlasso.dep <- mean((yhat.lasso.dep.new-datos.test[, "Apps"] )^2)
error.rlasso.dep # error absoluto, el mismo lambda del anterior


### Post-LASSO con penalizaci�n independiente de los datos ###
post.lasso.reg = rlasso(y[train]~x[train,],post=TRUE) #now use post-lasso
print(post.lasso.reg, all=FALSE) # or use summary(post.lasso.reg, all=FALSE)
yhat.postlasso.new = predict(post.lasso.reg, newdata=x[test,]) #out-of-sample prediction
error.postlasso <- mean((yhat.postlasso.new - datos.test[, "Apps"] )^2)
error.postlasso

## Mira que el error de predicci�n del Post-LASSO es menor porque este m�todo
## tiene un sesgo menor. As� que lo utilizaremos para hacer predicciones.

### INFERENCIA ###

### RLASSO con penalizaci�n independiente de los datos ###
lasso.effect = rlassoEffects(x = x[train,], y = y[train])
summary(lasso.effect)
plot(lasso.effect)

x.nuevo <- x[, -c(5:7,10:16)] # vaiables de 5 a 7 e de 10 a 16

lasso.effect = rlassoEffects(x = x.nuevo[train,], y = y[train])
summary(lasso.effect) # Private    Estimate. -420.75984 Std.Error  175.14925, numeros muy grandes de forma que esta variable sera despresada

x.nuevo <- x.nuevo[, -c(1,7)]

lasso.effect = rlassoEffects(x = x.nuevo[train,], y = y[train])
summary(lasso.effect) # sin la variable Private

lasso.reg.2 = rlasso(y[train]~x.nuevo[train,],post=FALSE) # use lasso, not-Post-lasso
yhat.lasso.new.2 = predict(lasso.reg.2, newdata=x.nuevo[test,]) #out-of-sample prediction
error.rlasso.2 <- mean((yhat.lasso.new.2-datos.test[, "Apps"] )^2)
error.rlasso.2

### Post-LASSO con penalizaci�n independiente de los datos ###
post.lasso.reg.2 = rlasso(y[train]~x.nuevo[train,],post=TRUE) #now use post-lasso
summary(post.lasso.reg.2, all=FALSE) # or use summary(post.lasso.reg, all=FALSE)

yhat.postlasso.new.2 = predict(post.lasso.reg.2, newdata=x.nuevo[test,]) #out-of-sample prediction
error.postlasso.2 <- mean((yhat.postlasso.new.2 - datos.test[, "Apps"] )^2)
error.postlasso.2

library(xtable)
table= rbind(error.rlasso,
             error.rlasso.2,
             error.postlasso,
             error.postlasso.2)
colnames(table)= c("ECM") 
rownames(table)= c("RLASSO", "RLASSO con inferencia",
                   "Post-LASSO", "Post-LASSO con inferencia")
tab= xtable(table)
tab





































