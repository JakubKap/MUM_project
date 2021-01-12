library(BCA)

# Zaczytanie zbioru danych 
somerville<-read.csv("D:/Studia/Semestr 9/Metody uczenia maszynowego II/Projekt/Git/MUM_project/dataset/SomervilleHappinessSurvey2015.csv", header=TRUE)

# Dodanie etykiet
names(somerville)<-c("rate","cityServiceInfoAvailability","housingCost", "schoolQuality","policeTrust", "infrastructureMaintance", "eventsAvailability")

# Tworzenie zbioru walidacyjnego i estymacyjnego
somerville$Sample <- create.samples(somerville, est = 0.7, val = 0.3)

scatterplot(rate~policeTrust, reg.line=lm, smooth=TRUE, spread=TRUE, 
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=somerville)

# Wykres œrednich dla policeTrust
with(somerville, plotMeans(rate, policeTrust, error.bars="none"))
