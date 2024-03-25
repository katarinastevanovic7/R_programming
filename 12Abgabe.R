# Übungsblatt 12 / Uebungsblatt 12

#Aufgabe1

#a Konfidenzintervall

#Binomialverteilt

#b

s = sample(c(0,1), size=100, replace = TRUE, prob = c(0.3, 0.7))  # Erstellt eine Stichprobe von 100 binomialverteilten Zufallsvariablen mit Erfolgswahrscheinlichkeit 0.7
binom.test(sum(s), 100, conf.level = 0.95)    # Führt einen Binomialtest durch und berechnet das 95% Konfidenzintervall für die Erfolgswahrscheinlichkeit


#c
ok = 0     # Zähler
N = 1000   # Anzahl Simulationen
for (r in 1:N) { # Schleife, die N Mal durchgeführt wird, um die Abdeckung des Konfidenzintervalls zu überprüfen
  s = sample(c(0,1), size=100, replace= TRUE, prob = c(0.3, 0.7)) # Erstellt eine neue Stichprobe für jede Iteration
  res = binom.test(sum(s), 100, conf.level = 0.95) # Führt einen Binomialtest für die aktuelle Stichprobe durch
  if (res$conf.int[1] < 0.7 & 0.7 < res$conf.int[2]){ # Überprüft, ob das Konfidenzintervall den wahren Wert 0.7 enthält
    ok = ok + 1 # Erhöht den Zähler, wenn das Konfidenzintervall den wahren Wert enthält
  }
}
ok / N          # Berechnet den Anteil der Konfidenzintervalle, die den wahren Wert enthalten


# Anmerkung: Der binom.test verwendet die Methode von Clopper und Pearson und liefert i.A. ein
# grösseres Konfidenzintervall, als eigendlich nötig ist. Strenggenommen muss nur gelten, das der
# wahre Wert in 95 Prozent alle Fälle enthalten ist. Siehe Hilfe von ‘binom.test‘


#Aufgabe2

#a

binom.test(120, 125)
# Führt einen Binomialtest durch für 120 Erfolge von 125 Versuchen, 
# ohne explizite Angabe der Erfolgswahrscheinlichkeit

