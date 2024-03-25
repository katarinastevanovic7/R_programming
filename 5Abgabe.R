# Übungsblatt 5 / Uebungsblatt 5

#aufgabe1

#a
k = 20  # Anzahl der Personen
N = 365 # Anzahl der Tage im Jahr

prod =1.0 # Initialisiert das Produkt auf 1.0
for (n in seq(N, N-(k-1), -1)) {
  prod = prod * n / N # Multipliziert das Produkt mit dem Anteil der "neuen" Tage
}
1 - prod # Berechnet die Wahrscheinlichkeit, dass mindestens zwei Personen am selben Tag Geburtstag haben



#b

k <- 20 # Anzahl der Personen
n <- 365 # Anzahl der Tage im Jahr
runs <- 1000 # Anzahl der Simulationen

counts <- 0 # Zähler für die Anzahl der Simulationen, in denen mindestens zwei Personen am selben Tag Geburtstag haben
for (run in 1:runs){
  geburtstage <- sample(n, size=k, replace=T) # Zieht k Geburtstage mit Zurücklegen
  if (max(table(geburtstage)) > 1) { # Überprüft, ob der maximale Wert in der Tabelle der Geburtstage größer als 1 ist
    counts <- counts + 1 # Erhöht den Zähler, wenn mindestens zwei Personen am selben Tag Geburtstag haben
  }
}
counts # Gibt die Anzahl der erfolgreichen Simulationen zurück


#Aufgabe 2

#a
factorial(11)/factorial(6)  # 11!/6!

#b
factorial(8)   #6!

#c
3^4

#d
choose(5,2)*choose(5,2) # Berechnet die Anzahl der Möglichkeiten, 2 aus 5 zu wählen und dann erneut 2 aus 5 zu wählen


#e Lösung im Heft