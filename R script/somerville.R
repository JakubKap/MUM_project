library(BCA)
library('corrplot')
library('car')
library('RcmdrMisc')
library(caret)
library(sqldf)
library(neuralnet)
library(RSNNS)

# Zaczytanie zbioru danych 
somerville<-read.csv("D:/Studia/Semestr 9/Metody uczenia maszynowego II/Projekt/Git/MUM_project/dataset/SomervilleHappinessSurvey2015.csv", header=TRUE)

# Dodanie etykiet
names(somerville)<-c("rate","cityServiceInfoAvailability","housingCost", "schoolQuality","policeTrust", "infrastructureMaintance", "eventsAvailability")

# Tworzenie zbioru walidacyjnego i estymacyjnego
somerville$Sample <- create.samples(somerville, est = 0.7, val = 0.3)

#zaleznosc rate od policeTrust
scatterplot(rate~policeTrust, reg.line=lm, smooth=TRUE, spread=TRUE, 
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=somerville)

#zaleznosc rate od schoolQuality
scatterplot(rate~schoolQuality, reg.line=lm, smooth=TRUE, spread=TRUE, 
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=somerville)

#zaleznosc rate od cityServiceInfoAvailability
scatterplot(rate~cityServiceInfoAvailability, reg.line=lm, smooth=TRUE, spread=TRUE, 
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=somerville)

#zaleznosc rate od housingCost
scatterplot(rate~housingCost, reg.line=lm, smooth=TRUE, spread=TRUE, 
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=somerville)

# Analiza korelacji zmiennych
somervilleFrame <- data.frame(somerville$rate, somerville$cityServiceInfoAvailability, somerville$housingCost, somerville$schoolQuality, somerville$policeTrust, somerville$infrastructureMaintance, somerville$eventsAvailability)

somervilleCor <- cor(somervilleFrame)
corrplot(somervilleCor,  method="number")



# Uogólniony model liniowy
GLM.MAX <- glm(rate ~ cityServiceInfoAvailability+ housingCost+ schoolQuality+ policeTrust+ infrastructureMaintance+ eventsAvailability, family=binomial(logit), data=somerville)
summary(GLM.MAX)

# Wspó³czynnik R2 McFaddena
1 - (GLM.MAX$deviance/GLM.MAX$null.deviance) # McFadden R2

# Redukcja zmiennych algorytm Viariable Step Selection

GLM.STEP <- step(GLM.MAX, direction="both")
summary(GLM.STEP)
1 - (GLM.STEP$deviance/GLM.STEP$null.deviance) # McFadden R^2

a<-predict(GLM.STEP, newdata = somerville) 
#[somerville$Sample=="Validation"])
a<-ifelse(a > 0.5, 0, 1)
confusionMatrix(factor(a), factor(somerville$rate))

#neuralnet
SN <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Estimation'")

SNV <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Validation'")

# Zmiana typu zmiennej class na czynnikowa
SN$rate<-factor(SN$rate)
SNV$rate<-factor(SNV$rate)

#Budowa modelu sieci neuronowej

nnet <- neuralnet(rate ~ . , data=SN, hidden=5, linear.output=FALSE)

# wydrukowanie wag
nnet$result.matrix

# wygenerowanie grafu sieci
plot(nnet)

# Podstawienie nowych wartosci
predicted.output=predict(nnet,SNV)

# Zaokraglenie wyliczonych wartosci do calkowitych
predicted.output=round(predicted.output,0)

# Wydrukowanie przewidywanych wartosci
print(predicted.output)

# Rozkodowanie wartosci wyjsciowych z macierzy binarnej na wektor o wartosciach 1 i 2
# z wykorzystaniem funkcji encodeClassLabels z pakietu RSNNS
predicted.output=encodeClassLabels(predicted.output)

# Zmiana typu zmiennej predicted.output na czynnikowa
predicted.output=factor(predicted.output)

# Wygenerowanie macierzy pomylek
caret::confusionMatrix(SNV$rate, predicted.output)

#neuralnet - dodanie liczby neuronów w warstwie
SN <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Estimation'")

SNV <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Validation'")

# Zmiana typu zmiennej class na czynnikowa
SN$rate<-factor(SN$rate)
SNV$rate<-factor(SNV$rate)

#Budowa modelu sieci neuronowej

nnet <- neuralnet(rate ~ . , data=SN, hidden=10, linear.output=FALSE)

# wydrukowanie wag
nnet$result.matrix

# wygenerowanie grafu sieci
plot(nnet)

# Podstawienie nowych wartosci
predicted.output=predict(nnet,SNV)

# Zaokraglenie wyliczonych wartosci do calkowitych
predicted.output=round(predicted.output,0)

# Wydrukowanie przewidywanych wartosci
print(predicted.output)

# Rozkodowanie wartosci wyjsciowych z macierzy binarnej na wektor o wartosciach 1 i 2
# z wykorzystaniem funkcji encodeClassLabels z pakietu RSNNS
predicted.output=encodeClassLabels(predicted.output)

# Zmiana typu zmiennej predicted.output na czynnikowa
predicted.output=factor(predicted.output)

# Wygenerowanie macierzy pomylek
caret::confusionMatrix(SNV$rate, predicted.output)


#Drzewa klasyfikacyjne
library(tree)


drzewo.DATA <-  tree(rate~.,data=somerville)

# Funkcja summary wyÅ›wietla satystyki opiowe dla zbudowanego drzewa

sd <- summary(drzewo.DATA)

# Funkcje plot i text umoÅ¼liwiajÄ… wyÅ›wietlenie graficznej reprezentacji drzewa

plot(drzewo.DATA)
text(drzewo.DATA)

#Sprawdzenie statystyk drzewa


# Funkcja predict umoÅ¼liwia podstawienie nowych danych do drzewa.

a<-predict(drzewo.DATA, newdata=somerville)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(somerville$rate))

#Przyciecie drzewa o jeden poziom mniej
drzewo2.DATA<-prune.tree(drzewo.DATA, best=4)
plot(drzewo2.DATA)
text(drzewo2.DATA)


# Statystyki przycitego drzewa

b<-predict(drzewo2.DATA, newdata=somerville)
b<-round(b,0)

caret::confusionMatrix(factor(b),factor(somerville$rate))

#Bagging
SN <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Estimation'")

SNV <- sqldf("SELECT rate+1 AS rate,cityServiceInfoAvailability, housingCost, schoolQuality, policeTrust, infrastructureMaintance, eventsAvailability  FROM somerville WHERE Sample = 'Validation'")

library(ipred)
mod <- bagging(rate~., data=SN, nbagg=150)
a<-predict(mod, newdata=SN)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(SN$rate))

#Dla zbioru walidacyjnego

b<-predict(mod, newdata=SNV)
b<-round(b,0)
caret::confusionMatrix(factor(b),factor(SNV$rate))

#Zwiekszenie liczby nbagg do 250
mod <- bagging(rate~., data=SN, nbagg=50)
a<-predict(mod, newdata=SN)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(SN$rate))

#Dla zbioru walidacyjnego

b<-predict(mod, newdata=SNV)
b<-round(b,0)
caret::confusionMatrix(factor(b),factor(SNV$rate))


# Random Forest
library(randomForest)
mod <- randomForest(rate~., data=SN, ntrees=150)
a<-predict(mod, newdata=SN)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(SN$rate))

#zbiór walidacyjny
b<-predict(mod, newdata=SNV)
b<-round(b,0)
caret::confusionMatrix(factor(b),factor(SNV$rate))

#zmniejszenie ntrees do 10
mod <- randomForest(rate~., data=SN, ntrees=10)
a<-predict(mod, newdata=SN)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(SN$rate))

#zbiór walidacyjny
b<-predict(mod, newdata=SNV)
b<-round(b,0)
caret::confusionMatrix(factor(b),factor(SNV$rate))

#SVM
library(e1071)

mod <- svm(rate~., data=SN)
a<-predict(mod, newdata=SN)
a<-round(a,0)

caret::confusionMatrix(factor(a),factor(SN$rate))

# Dla zbioru walidacyjnego

b<-predict(mod, newdata=SNV)
b<-round(b,0)
caret::confusionMatrix(factor(b),factor(SNV$rate))
