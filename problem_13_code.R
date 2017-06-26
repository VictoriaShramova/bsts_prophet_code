library("psych")  # описательные статистики
library("dplyr")  # манипуляции с данными
library("ggplot2")  # графики
library("GGally")  # еще графики
library("memisc")  # две и более регрессий в одной табличке
library("car")
library("hexbin")  # графики
library("tidyverse") # вместо ggplot2 (графики) и dplyr (манипуляции с данными)
library("lmtest")  # тестирование гипотез в линейных моделях
library("sjPlot")  # графики
library("sgof")

# A) Загрузить данные из .csv файла в R

# первая (сознательно неудачная) попытка чтения файла
t <- read.csv("flats_moscow.txt")
glimpse(t)

# попытка чтения файла с указанием параметров:
t <- read.csv("flats_moscow.txt", sep = "\t", dec = ".", header = TRUE)
glimpse(t)

# Б) Посчитать описательные статистики: среднее, мода, медиана и т.д.

# по переменным из .csv файла: min, 1stq, median, mean, 3rdq, max
summary(t)
describe(t)
# по одной переменной
# задаём векторы:
x <- c(23, 15, 46, NA)
describe(x) # все статистики сразу
# среднее арифметическое
mean(x)
# среднее арифметическое с удалением пропущенных значений
mean(x, na.rm = TRUE)
# сумма всех элементов вектора
sum(x)
sum(x, na.rm = TRUE)
max(x, na.rm = TRUE) # аналогично mean, sd, var, min, max, median, range, quantile
# quantile(hip[,1],0.10) quantile(x,0.10)
# more http://www.iiap.res.in/astrostat/School08/tuts/descr.html
sd(x, na.rm = TRUE)
describe(x)

# В) Построить подходящие описательные графики для переменных

# отберём мужчин в отдельную табличку
#h4 <- filter(h3, sex == "МУЖСКОЙ")

# создение факторной переменной 
#https://stats.idre.ucla.edu/r/modules/coding-for-categorical-variables-in-regression-models/
# creating the factor variable
#hsb2$race.f <- factor(hsb2$race)
#is.factor(hsb2$race.f)
#hsb2$race.f[1:15]
#summary(lm(write ~ race.f, data = hsb2))

# диаграмма рассеяния
qplot(data = t, dist, price)
plot(t$dist, t$price)
# диаграмма рассеивания для многих признаков
# more http://www.statmethods.net/graphs/scatterplot.html
pairs(~price+totsp+dist+metrdist,data=t, 
      main="Simple Scatterplot Matrix")
# гистограмма
qplot(data = t, price)
hist(t$price,prob=T,main="Price")
points(density(t$price),type="l",col="blue")#fit density function
m <- mean(t$price)
std <- sd(t$price)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)#fit normal density function
# Ящик с усами
boxplot(t[,2:4]) # for columns 2,3,4



# Г) Оценить линейную регрессию с помощью МНК. Провести диагностику на что-нибудь.
# множественная регрессия. проверка гипотез

h <- swiss  # набор данных по кантонам Швейцарии (встроенный в R)
glimpse(h)  # бросим взгляд на данные
help(swiss)

# оценим модель множественной регрессии
model <- lm(data = h, Fertility ~ Catholic + Agriculture + Examination)
# посмотрим результаты оценивания
summary(model)

# отдельно табличка с тестами
coeftest(model)

confint(model)  # доверительные интервалы для коэффициентов
sjp.lm(model)  # графическое представление интервалов

# проверка гипотезы b_Cath=b_Agri построение вспомогательной модели
model_aux <- lm(data = h, Fertility ~ Catholic + I(Catholic + Agriculture) + Examination)
summary(model_aux)

# проверка гипотезы без построения вспомогательной модели
linearHypothesis(model, "Catholic-Agriculture=0")

# test Breusch-Pagan
bptest(model, varformula = ~ log(table)+log(carat), data = diamonds) #на втором шаге по умолчанию включает изначальные регрессоры, 
# но можно задать c помощью varformula= ~ 

# гомоскедстичность отвергается

# test Goldfeld-Quandt
gqtest(model, fraction = 0.2, order.by = diamonds$table) # выкинем 20% и упорядочим по переменной table
gqtest(model, fraction = 0.2, order.by = diamonds$carat) # выкинем 20% и упорядочим по переменной carat

#от table не зависит разброс, разброс зависит от carat

# Д) Оценить logit, probit модели, посчитать предельные эффекты
# смотри lab_07_after, там все-все есть

# е) Оценить ARMA модель -- все это смотри в lab_08_after
# загрузка данных, модель временной регрессии, устойчивые остатки, а\к в lab_06_after

# ж) Выделить главные компоненты -- смотри в конец lab_04_after





