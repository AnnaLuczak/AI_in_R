install.packages("AMORE")
library(AMORE)
install.packages("mlbench")
library(mlbench)
data("PimaIndiansDiabetes")  # dane uczące się
PimaIndiansDiabetes
table(PimaIndiansDiabetes$diabetes)
l.danych <- nrow(PimaIndiansDiabetes)
set.seed(8)  #  podział danych oraz ziarno generatora liczb pseudolosowych
idxTren <- sample(1:l.danych, 512) # 512 losowych indeksów (metoda lambda = 2/3)
idxTest <- setdiff(1:l.danych, idxTren) # reszta rekordów
# przekształcenie danych do postaci liczbowej
target<-function(x)
{
n<-length(x)
wartosci<-levels(x)
l<-length(wartosci)
T<-matrix(0,nrow=n,ncol=l)
for(i in 1:l)
T[,i]<-(x==wartosci[i])
colnames(T)<-wartosci
return(T)
}
# zastosowanie dla danych określających etykiety
wZadane<-target(PimaIndiansDiabetes$diabetes)
wZadane
PimaIndiansDiabetes
set.seed(2)
# tworzę strukturę sieci (8 wejść, 6 warstw ukrytych, 2 wyjścia)
siec <- newff(n.neurons = c(8,6,2), learning.rate.global = .02, momentum.global = 0.5, hidden.layer = "sigmoid",
output.layer = "purelin", method = "ADAPTgdwm", error.criterium = "LMS")
# trenowanie sieci
wynik<-train(siec, PimaIndiansDiabetes[idxTren, -9], wZadane[idxTren,], error.criterium = "LMS", report=TRUE, show.step = 10, n.shows=600)
plot(wynik$Merror, type = "l", xlab=" Iteracja (x10)", ylab="Błąd", col="darkred") # wykres
y <- sim(wynik$net, PimaIndiansDiabetes[idxTest, -9]) # wytrenowana sieć zastosowana do danych testowych
y
# funkcja oceny klasyfikacji
test.klasyf <- function(zad, wy) {
zadane <- max.col(zad)
rozpoznane <- max.col(wy)
print(table(zadane, rozpoznane))
}
wynik <- test.klasyf(wZadane[idxTest,], y)
cat("Dokładność klasyfikacji: ", sum(diag(wynik))/sum(wynik)*100, "%") # dokładność klasyfikacji ~ 62%
savehistory("C:/Users/studentwsb/Desktop/ai.r")
