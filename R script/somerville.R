library(BCA)
library('corrplot')
library('car')
library('RcmdrMisc')
library(caret)

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

