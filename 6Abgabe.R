# Übungsblatt 6 / Uebungsblatt 6


#Aufgabe 1: Fahrerflucht im Heft


#Aufgabe 2
#a

runs = 1000  # Anzahl der Durchläufe der Simulation
gew = 0  # Zähler für die Anzahl der Gewinne
for (r in 1:runs){  # Schleife über die Anzahl der Durchläufe
  if (sum(sample(0:1, 15, replace = T)) == 4){  # Ziehe 15 mal mit Zurücklegen und prüfe, ob die Summe der Ziehungen 4 ist
    gew = gew + 1  # Erhöhe den Gewinnzähler, wenn die Bedingung erfüllt ist
  }
}
gew / runs  # Berechne die Wahrscheinlichkeit des Ereignisses

#b    
runs = 1000
gew_0_15 = rep(0,16)  # Erstelle einen Vektor von 16 Nullen, um die Zählungen für jede mögliche Summe (0 bis 15) zu speichern
for (r in 1:runs){
  k = sum(sample(0:1, 15, replace = T))  # Summe der Ziehungen
  gew_0_15[k+1] = gew_0_15[k+1] + 1  # Erhöhe den Zähler für die entsprechende Summe
}
ps = gew_0_15 / runs  # Berechne die Wahrscheinlichkeiten für jede Summe
ps

#c
plot(k= 0:15, ps, type='h', xlab='Gewinn', ylab="W'keit für Gewinn")  # Erstelle ein Histogramm der Wahrscheinlichkeiten
points(k=0:15, dbinom(0:15, 15, 0.5), col="red")  # Füge die theoretischen Wahrscheinlichkeiten (Binomialverteilung) hinzu


#d
runs = 1000
gew = 0
for (r in 1:runs){
  gew= gew + sum(sample(0:1, 15, replace = T))  # Summiere die Gewinne
}
gew / runs  # Berechne den durchschnittlichen Gewinn

#e
sum(ps * 0:15)  # Berechne den erwarteten Gewinn basierend auf den simulierten Wahrscheinlichkeiten


#f
sum(dbinom(0:15, 15, 0.5) * 0:15)  # Berechne den theoretischen Erwartungswert basierend auf der Binomialverteilung


