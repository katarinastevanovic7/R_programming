data <- read.delim("/Users/katarinastevanovic/Desktop/Studium/3.Semester/Wahrscheinlichkeitstheorie und Statistik/Abgaben/Churn.csv")

#Aufgabe 1 a) Visualisierung des Anteils der Kunden, die gekündigt haben
barplot(table(data$Churn))



#1 b) Visualisierung der Häufigkeit der Kundenanrufe
barplot(table(data$CustServCalls))
#table(): Die Funktion zählt, wie oft jeder eindeutige Wert im Vektor vorkommt,
#und gibt eine Tabelle mit den eindeutigen Werten und ihren Häufigkeiten aus
#barplot:  zeichnet einen Säulen- bzw. Balkendiagramm
# $: wird verwendet, um auf eine bestimmte Spalte in einem Datenrahmen 
#oder auf ein bestimmtes Element in einer Liste zuzugreifen.



#1 c) Zusammenhang zwischen CustServCalls und Tatsache, dass ein Kunde küdigt
boxplot(CustServCalls ~ Churn, data=data)
# Kunden, die kündigen machen in der Regel mehr Service Calls



#1 d) 
# Kreuztabelle, die die Anzahl der Beobachtungen für verschiedene
# Kombinationen von Churn und Kundendienstanrufen zählt. 
counts <- table(data$Churn, data$CustServCalls)
# Anzahl der Beobachtungen, bei denen Churn gleich 1 ist,  
# geteilt durch die Summe der Beobachtungen für Churn gleich 0 und Churn gleich 1.
relative_churns = counts[2, ] / (counts[1, ] + counts[2,])
# Streudiagramm, das die Anzahl der Kundendienstanrufe (0 bis 9) auf der
# x-Achse und die relativen Abwanderungsraten auf der y-Achse darstellt.
plot(0:9, relative_churns, xlab = 'Number of Service Calls')
# xlab Diagramm Name

#-------------------------------------------------------


#Aufgabe 2 
# 7 - 10.000
# 1 - 12.000
# 3 - 15.000
# 2 - 24.000
# 1 - 60.000
# 1 - 125.000
#
# Mittelwert: 246.000/15 = 24.000 
# Median: die 8. Person bekommt einen "Mittleren Lohn" - 12.000
# Modus: der Lohn, der am häufigsten ausgezahlt wird - 10.000
#


#-------------------------------------------------------

#Aufgabe 3
Arrivals <- read.table("http://www-home.htwg-konstanz.de/~oduerr/data/Arrivals.txt", header = TRUE, sep = "\t")



#3 a) Wie viele Zeilen, respektive Spalten hat der Datensatz?
dim(Arrivals)





#3 b) von welchem Datentyp sind die Variablen
#
# FLugnummer : nominal kategoriell
# Transferpassagiere: diskret metrisch
# Herkunft: nominal kategoriell
#



#3 c) Bestimmen Sie die Häufigkeitstable für die Variable Landebahn.

table(Arrivals$Landebahn)


#3 d) Stellen Sie die Variable Landebahn graphisch dar.

barplot(table(Arrivals$Landebahn), main="Balkendiagramm Landebah")


#3 e) Histogramm Variable Verspaetung graphisch darstellen

hist(Arrivals$Verspaetung, main="Verspätungen", xlab="in min")
# rechtsschiefe Verteilung


#3 f)  Berechnen Sie den Mittelwert und den Median der Variable Verspaetung. 
# Welches Lagemass ist hier besser geeignet?

mean(Arrivals$Verspaetung)

median(Arrivals$Verspaetung)

# da die Verteilung etwa rechtsschief ist, eignet sich der Median besser


#3 g) Bestimmen Sie die Quartile des Datensatzes

quantile(Arrivals$Verspaetung, 0.25) # 25% -4

quantile(Arrivals$Verspaetung, 0.5) # 50% 4

quantile(Arrivals$Verspaetung, 0.75) # 75% 16











