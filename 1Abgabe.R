#1 a)
x <- c(5,2,1,4) #c = character
y <- rep(1,5) #rep = repeat, die 1 5mal wiederholen
nam=c("Hans", "Axel", "Steph")


#1 b)
x; y; # gibt x und y aus
str(x) #Ausgabe: str is a function() = length + Vektor selbst
class(x) #gibt den Datentypen an
c(x, y, 13) #fügt neue Elemente hinzu
sum(x) #Summe von x

#1 c)
range(x) #kleinste und größte Zahl
length(y) #Länge von y
mean(x) #summe/ Anzahl der Elemente, Durchschnitt
max(x) #größte Zahl
min(x) #kleinste Zahl


#1 d)
nam=="Axel"  #Ausgabe False True False, zeigt true an der richtigen Stelle
nam[3]="Stef" #überschreibt die Stelle (Stef statt Steph)
length(nam) #Länge array

x[2] #2 an der 2.Stelle
x[4] * y[2] #4*1 = 4
x[2:4] #die Zahl an der Stelle 2 - 4
x[2:4]+1 #Zahlen die an der Stelle 2-4 sind wird eine + 1 dazu gerechnet, Ausgabe: 3, 2, 5


#1 e)
x <= 2 #x kleiner oder gleich 2, Ausgabe: FALSE  TRUE  TRUE FALSE
x[x <=2] #Ausgabe: 2 1, die Zahlen werden ausgegeben die <=2


#1 f) wie viele Elemente > 1
sum(x>1) #Ausgabe 3



#------------------------------------------


#R Funktionen um Vektoren zu erzeugen

#2 a)
my.vec <- c(3, -2, 5, 0) #vector my.vec erzeugen


#2 b)
v2 <- my.vec[1:2] #auf erste 2 elemente zugreifen und in v2 speichern

#2 c)
append(my.vec,10:20) #vektor mit Zahlen 10 bis 20 und an my.vec dranhängen

#2 d)
my.vec[my.vec<5] <-0 #alle Elemente aus my.vec <5 auf 0 setzen

#2 e)
rep(1:4, 2) #wiederholt zwei mal Zahlen 1 bis 4
rep(c("ja","nein"), each = 2) #wiederholt 2 mal ja und nein, 
#c ist ein Vektor mit Elementen ja und nein
rep(letters[1:4], c(2,1,2,1)) #wiederholt die ersten 4 Buchstaben wie in c beschriben
#"a" "a" "b" "c" "c" "d"

#2 f)
seq(from=2, to=5, by=0.5) # 2 bis 5 jede 0,5te Zahl
seq(from=2, to=5, length.out=30)*c(-1,1) #Zahlen von 2 bis 5 mit 30 Elementen
#es wird mit -1 multipliziert und danach mit 1


#2 g) 123451234512345 mit rep und seq erzeugen

rep(seq(1:5),3)


#Aufgabe3
date <- read.delim("/Users/katarinastevanovic/Desktop/Studium/3.Semester/Wahrscheinlichkeitstheorie und Statistik/Abgaben/Churn.csv") 
#read.delim: read delimiter
#date: Variablenname

View(date) #zeigt alle daten an


#3 a) Missing values?
is.na(date) #gibt viele Daten aus
sum(is.na(date)) #0, gibt keine missing values, alle Datens sind vorhanden


#3 b) Wie viele haben gekündigt?
nrow(date) #returns the number of rows or columns present
sum(date$Churn) #483 wie viele gekündigt haben, Reihe Churn gibt es an
sum(date$Churn) / nrow(date) #wie viel Prozent von der Gesamtsumme ist das


#3 c) data frame erzeugen mit Kunden, die min. 5 CustServCalls hatten
date_called = date[date$CustServCalls > 4, ]
nrow(date_called) #Anzahl Zeilen
sum(date_called$Churn) #62
sum(date_called$Churn) / nrow(date_called) #61%


#3 d)Mit den Methoden in b). Sind Kunden die langjährig dabei sind treuer? 
#Teilen Sie dazu die Kunden je nach AccountLength in 2 gleich grosse Teile.

m = median(date$AccountLength)
date_treu = date[date$AccountLength > m, ]
nrow(date_treu) #1629
sum(date_treu$Churn) / nrow(date_treu) #14%













