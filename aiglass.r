library(AMORE)
install.packages("AMORE")
install.packages("mlbench")
library(mlbench)
install.packages("AMORE")
data("Glass")
dim(Glass)
levels(Glass$Type)
head(Glass)
Glass  # sprawdzam strukturę danych
table(Glass$Type)
ile = nrow(Glass)
set.seed(112) # ziarno generatora liczb pseudolosowych
idxTren <- sample(1:ile, 2*ile/3) # 2/3 losowych indeksów do trenowania sieci
idxTest <- setdiff(1:ile, idxTren) #pozostałe indeksy
target<- function(x) # przekształcanie danych (etykiet) do postaci liczbowej
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
wZadane<-target(Glass$Type) #zastosowanie dla danych określających etykiety
wZadane
set.seed(12)
siec<-newff(n.neurons=c(10,24,6), # tworzenie struktury sieci
learning.rate.global=.002,
momentum.global=0.05,
hidden.layer="sigmoid",
output.layer="purelin",
method="ADAPTgdwm",
error.criterium="LMS")
wynik<-train(siec, #trenowanie
Glass[idxTren,-10],
wZadane[idxTren,],
error.criterium="LMS",
report=TRUE,
show.step=20,
n.shows=1000)
plot(wynik$Merror,type="l",xlab="Ileracja (x10)", #wykres
ylab="Błąd", col="darkred")
y<-sim(wynik$net, Glass[idxTest, -10]) # zastosowanie wytrenowanej sieci do danych testowych
y
test.klasyf<-function(zad,wy) # funkcja oceny klasyfikacji (zmieniam liczby na etykietę)
{
zadane<-max.col(zad)
rozpoznane<-max.col(wy)
print(table(zadane,rozpoznane))
}
wynik<-test.klasyf(wZadane[idxTest,],y)
cat("Dokkładność klasyfikacji:", # określenie dokładności klasyfikacji ~55.5%
sum(diag(wynik))/sum(wynik)*100, "%\n")
savehistory("C:/Users/studentwsb/Desktop/aiglass.r")
