versicherung <- read.delim(("/Users/katarinastevanovic/Desktop/3.Semester/Wahrscheinlichkeitstheorie und Statistik/private-unfall-versicherung.csv"), sep = ";")

#Aufgabe 1


#a) Wie viele Schäden wurden insgesamt von der Versicherung beglichen?
str(versicherung) #str(): Anzeige von Informationen über den dataframe
#Datenrahmen mit 22036 Zeilen und zwei Spalten


#Welche Merkmale wurden beobachtet und welchen Datentyp haben diese Merkmale?
summary(versicherung) #errechnet verschiedene  Lagemaße für alle Spalten des Dataframes
#Der erste Quartilwert (25. Perzentil) beträgt 6297. 25% der Datenpunkte kleiner oder gleich 6297 
#Der Median (50. Perzentil) beträgt 13854
#Der dritte Quartilwert (75. Perzentil) beträgt 35123




#b
range(versicherung[, 1]) #min und max Wert der ersten Spalte

sqrt(length(versicherung[, 1])) #square root
#gibt die Länge der ersten Spalte zurück
#sqrt um Quadratwurzel der Länge zu berechnen

#Histogramm der Schäden
hist(versicherung$Schaden, nclass = 150, main = "Histogramm Schäden", xlab = "$", xaxt = "n")
axis(1, at = c(0, seq(1000000, 4000000, by = 1000000)), labels = c("0", "1M", "2M", "3M", "4M"))
#Histogramm in 150 Klassen aufteilen
#4e+06 = 4_000_000 (4*10^6)


versicherung_2 <- subset(versicherung, Schaden > 0 & Schaden <= 100000)
#subset um Teilmengen von Daten aus einem Datenrahmen zu entnehmen
hist(versicherung_2$Schaden, main = "Histogramm Schäden", xlab= "in $", xaxt = "n")
axis(1, at = c(0, seq(20000, 100000, by = 20000)), labels = c("0", "20k", "40k", "60k", "80k", "100k"))
#1e+05 = 100_000 (1*10^5)
# Achsenbeschriftung anpassen








#c) Erweitern um eine weitere Spalte, die den Logarithmus der Schadenhöhe enthält
versicherung$Logarithmus = log(versicherung$Schaden)
hist(versicherung$Logarithmus, nclass = 150, xlab = "log Schäden", main = "Histogramm der logarithmierten Schäden")





#d)
sd(versicherung$Schaden) #Standardabweichung

mad(versicherung$Schaden) #Median der absoluten Abweichungen

median(versicherung$Schaden)

mean(versicherung$Schaden) 

#logarithmierte Daten
median(versicherung$Logarithmus) #9.53632

log(median(versicherung$Schaden)) #9.53632
#Median und Logarithmus werden in 2 Hälften geteilt 50% kleiner als Median und 50% größer

mean(versicherung$Logarithmus) #9.556373
#Ausreißer beeinflussen stark den Mittelwert, weil sie die Summe der Daten stark verändern
log(mean(versicherung$Schaden)) #10.55496






#e) Verteilung der Schäden und der logarithmierten Schäden Boxplot 
boxplot(versicherung$Schaden, data = versicherung)

boxplot(versicherung$Logarithmus, data = versicherung)






#f) Verteilung der Schäden und der log-Schäden nach Schadenstyp aufgetrennt mit Boxplots
boxplot(Schaden ~ Typ, data =versicherung)

boxplot(Logarithmus ~ Typ, data = versicherung)




#Aufgabe 2 im Heft Z-Transformation
