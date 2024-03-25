# Übungsblatt 7 / Uebungsblatt 7

#Aufgabe1
#a
(x <- 223/5656) # Berechnet und speichert das Verhältnis 223 zu 5656 in x

#b
n <- 21
(1-p)^n # Berechnet die Wahrscheinlichkeit, dass ein Ereignis nach n Versuchen nicht eintritt, basierend auf p

#c
k <- 0:18 # Erstellt einen Vektor k von 0 bis 18
plot(k, dbinom(k, 18, p), type='h', main="wahrscheinlichkeit für k tote") 
# Plottet die Wahrscheinlichkeitsverteilung für k Tote, basierend auf der Binomialverteilung mit Parameter p

#i) # Berechnet die Wahrscheinlichkeit von 0 Toten in 18 Versuchen
dbinom(0,18,p)

#ii) # Berechnet die Wahrscheinlichkeit von genau 5 Toten
dbinom(5,18, p)

#iii) # Berechnet die Wahrscheinlichkeit von mehr als 2 Toten (3 bis 18)
sum(dbinom(3:18,18, p))

#vi) # Berechnet die Wahrscheinlichkeit von 0 bis 2 Toten
sum(dbinom(0:2, 18, p))


#d)
# Es können unerwartete Ereignisse passieren und es könnten 
# immer jeweils mehrere Personen betroffen sein. Somit ist die Annahme
# der Unabhängigkeit am ehesten verletzt.


#Aufgabe 2
#a
# Definiert einen Vektor von Wahrscheinlichkeiten für jedes Ergebnis (0 bis 8), 
# wobei die letzten zwei explizit gesetzt sind und die anderen gleichverteilt sind 
# abzüglich der Summe der letzten zwei
p = c(rep(1,7) / 7 * (1-(0.1 + 0.05)), 0.1, 0.05)
p
sum(p)

#b 
# Berechnet den Erwartungswert E basierend auf dem Wahrscheinlichkeitsvektor p
E = sum(0:8*p)
E

#c

N= 10000

xs = sample(0:8, prob = p, replace = T, size = N)
mean(xs)

#d)


simu = function(auf_lager){ # Definiert eine Funktion zur Simulation des Gewinns basierend auf der Menge der vorrätigen Einheiten
  gewinn = -auf_lager * 0.1 # Initialisiert den Gewinn basierend auf den Lagerkosten 
  
  b = sample(0:8, prob = p, replace = TRUE, size = 1) # Simuliert die Nachfragem Size = N heisst 10000 Versuche
  
  if (b <= auf_lager){ # Berechnet den Gewinn, wenn die Nachfrage geringer oder gleich dem Lagerbestand ist
    gewinn = gewinn + b
  }                 
  
  if (b > auf_lager){        # Berechnet den Gewinn (oder Verlust) wenn die Nachfrage den Lagerbestand übersteigt
    gewinn = gewinn + auf_lager
    gewinn = gewinn - (b - auf_lager)*0.5
  }
  return (gewinn)
}


runs = 10000                      # Setzt die Anzahl der Durchläufe der Simulation auf 10.000

gewinn = rep(0,25)                # Initialisiert einen Vektor `gewinn` mit 25 Nullen, um den durchschnittlichen Gewinn für jede Lagermenge zu speichern

for (a in 0:24){                  # Startet eine Schleife von 0 bis 24, die die Anzahl der auf Lager gehaltenen Einheiten darstellt
  res = rep(NA, runs)             # Erstellt einen Vektor `res` mit `NA` (steht für "nicht verfügbar"), um die Ergebnisse jeder Simulation zu speichern
  for (r in 1:runs){              # Startet eine innere Schleife, die 10.000 Mal durchläuft, entsprechend der Anzahl der Simulationen
    res[r] = simu(a)              # Ruft die Funktion `simu` auf, die den Gewinn für eine gegebene Lagermenge `a` simuliert, und speichert das Ergebnis in `res[r]`
  }
  gewinn[a+1] = mean(res)         # Berechnet den durchschnittlichen Gewinn aus allen Simulationen für die Lagermenge `a` und speichert ihn in `gewinn[a+1]`
}

plot(0:24, gewinn, xlab='Auf Lager', ylab='Erwarteter Gewinn') # Erstellt ein Diagramm, das den erwarteten Gewinn (y-Achse) in Abhängigkeit von der Lagermenge (x-Achse) zeigt

