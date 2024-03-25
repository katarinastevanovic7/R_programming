# Übungsblatt 13 / Uebungsblatt 13

#Aufgabe1

#a
attach(hwrunoff) # Macht die Variablen in 'hwrunoff' direkt zugänglich
plot(rain, runoff) # Erstellt einen Scatterplot mit 'rain' auf der x-Achse und 'runoff' auf der y-Achse


#b

lm.erg <- lm(runoff ~ rain) # Erstellt ein lineares Modell von 'runoff' als Funktion von 'rain'
summary(lm.erg) # Zeigt eine Zusammenfassung des linearen Modells, einschließlich Koeffizienten und Statistiken


plot(rain,runoff) # Erstellt einen Scatterplot, der die Beziehung zwischen Niederschlag (rain) und Abfluss (runoff) zeigt.
abline(lm.erg) # Fügt dem Scatterplot eine Regressionsgerade basierend auf einem linearen Modell (lm.erg) hinzu, um den Trend zwischen rain und runoff zu visualisieren.

#c
predict(lm.erg, newdata = data.frame(rain=50)) # Macht eine Vorhersage für 'runoff' bei 50 Einheiten 'rain' mit dem linearen Modell


#d
# Aus dem Output erhält man = 5.24

#e
# Das Bestimmtheitsmaß "Multiple R-Squared", das den ANteil der durch
# die Regressionsgerade erklärten Variation angibt, ist mit 97.53% sehr hoch

#f
# Die Modellvoraussetzungen sind hier nur mässig gut erfüllt. Im Tukey-Anscombe-Plot ist eine
# gewisse "Badewannenform" zu erkennen. Und auch der Scale-Location-Plot zeigt eine nichtkonstante Varianz über den Wertebereich. Während hier sicher ein Zusammenhang zwischen
# Rainfall und Runoff Volume besteht und das Modell auch einigermassen zur Vorhersage
# taugt, ist insbesondere beim Einzeichnen von Vertrauens- und Vorhersageintervallen Vorsicht
# geboten, dort wirken sich die nicht perfekt erfüllten Annahmen stärker aus.

plot(mfrow=c(2,2)) # Teilt das Plotfenster in ein 2x2-Raster, um vier Plots gleichzeitig anzuzeigen
plot(lm.erg) # Erstellt diagnostische Plots für das lineare Modell 'lm.erg'


#Aufgabe2
xi = seq(1,10,0.5) # Erstellt eine Sequenz von x-Werten
n = length(xi) # Berechnet die Länge der Sequenz, d.h. die Anzahl der x-Werte
in_ci = 0 # Initialisiert Zähler für die Anzahl der Male, dass die wahre Steigung im Konfidenzintervall liegt
runs = 1000 # Anzahl der Simulationen
for (i in 1:runs){ # Startet eine Schleife für die Simulationen
  yi = rnorm(n, mean=1*xi+2, sd = 0.5) # Generiert y-Werte als normalverteilte Zufallszahlen basierend auf der linearen Beziehung y=1*xi+2 mit Standardabweichung 0.5
  res = lm(yi ~ xi) # Erstellt ein lineares Modell von yi als Funktion von xi
  ci_slope = confint(res, level=0.6)[2,1:2] # Berechnet das 60%-Konfidenzintervall für die Steigung des linearen Modells
  if (ci_slope[1] < 1 && 1 < ci_slope[2]) { # Überprüft, ob die wahre Steigung (1) im Konfidenzintervall liegt
    in_ci = in_ci + 1 # Erhöht den Zähler, wenn die wahre Steigung im Konfidenzintervall liegt
  }
}
in_ci / runs # Berechnet den Anteil der Simulationen, bei denen die wahre Steigung im Konfidenzintervall lag









