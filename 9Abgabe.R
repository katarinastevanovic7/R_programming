# Übungsblatt 9 / Uebungsblatt 9

#aufgabe1

#a

hist(abodauer, nclass=20, freq=FALSE, col="green")
# freq=FALSE weist hist() an, 
# relative Häufigkeiten (Dichtewerte) statt absoluter Häufigkeiten anzuzeigen.

#b
# Eine Abodauer ost sozusagen eine Lebensdauer und anhand des
# des Histogramms erkennt man eine Exponentialfunktion.
# Dazu sind alle Werte positiv. Es gibt kürzere und längere Abodauern,
# d.h. die nötige Rechtsschiefe ist vorhanden. Den Paramter
# kann man als Kehrwert der mittleren Lebensdauer schätzen: Lambda dach oder 1 durch mean(x)
(lambda <- 1/mean(abodauer))

#c)

xx <- seq(0, 3000, length = 300)  # seq(0, 3000, length = 300) erzeugt eine Sequenz von Werten von 0 bis 3000, die als x-Werte für eine Exponentialverteilung dienen.
yy <- dexp(xx, lambda)            # dexp(xx, lambda) berechnet die Dichtefunktion der Exponentialverteilung für die x-Werte in xx mit dem Parameter lambda.
lines(xx,yy,col="red")            # lines(xx, yy, col="red") fügt dem Histogramm eine rote Linie hinzu, die die Dichtefunktion der Exponentialverteilung darstell


#d)
library(car)                                          # lädt car-Paket
qqPlot(abodauer, distribution = "exp", rate = lambda) 
# qqPlot aus dem car-Paket erstellt einen Quantil-Quantil-Plot (Q-Q Plot), der die Quantile der abodauer-Daten mit den theoretischen Quantilen einer Exponentialverteilung vergleicht.
# distribution = "exp" gibt an, dass die theoretischen Quantile aus einer Exponentialverteilung stammen.
# rate = lambda spezifiziert den Parameter der Exponentialverteilung.
abline(a = 0, b= 1, col = "red") 
# abline(a = 0, b = 1, col = "red") fügt eine rote Linie mit Steigung 1 und Achsenabschnitt 0 hinzu, um die ideale Anpassung zu visualisieren.




#aufgabe2

s20 = rep(NA,1000)                                # rep(NA,1000) initialisiert einen Vektor s20 mit 1000 NA-Werten.
for (i in 1:1000){                                # for (i in 1:1000) wiederholt 1000 Mal die Simulation, bei der jeweils 20 gleichverteilte Zufallszahlen
  s20[i] <- sum(runif(n=20, min = 0, max = 1))  # zwischen 0 und 1 generiert (runif(n=20, min = 0, max = 1)) und aufsummiert werden.
}
hist(s20, freq=F, breaks=30)                      # hist(s20, freq=F, breaks=30) erstellt ein Histogramm der Summen mit 30 Bins und zeigt Dichtewerte an.

mean(s20)   # Mittelwert von s20  
var(s20)    # Varianz von s20

#b
hist(s20, freq=F, breaks=30)                      # Histogramm von s20, freq=false weist an, das hist() relative Häufigkeiten (Dichtwerte) statt absoluter Häufigkeiten anzuzeigen
pts <- seq(5,15,0.1)                              # seq(5,15,0.1) erstellt eine Sequenz von Punkten für die x-Achse.
lines(pts, dnorm(pts, mean=10, sd= sqrt(20/12)))  # lines() fügt eine Linie hinzu, die eine Normalverteilung mit dem Mittelwert 10 und der Standardabweichung sqrt(20/12) repräsentiert,
# basierend auf den Eigenschaften der Summe gleichverteilter Zufallszahlen.

#c
xs = seq(-10,10,6/100)  # Erstellt eine Sequenz von x-Werten von -10 bis 10, in Schritten von 0.06 (6/100),
# die als Grundlage für den Plot der t-Verteilung dienen.
plot(xs, dt(xs, df=1), type='l')
# Erstellt einen Plot der t-Verteilung mit 1 Freiheitsgrad (df=1) für die x-Werte in 'xs'.
# 'dt(xs, df=1)' berechnet die Dichtewerte der t-Verteilung an den Punkten in 'xs'.
# 'type='l'' bedeutet, dass die Punkte als Linie ('line') gezeichnet werden, um die Form der Verteilung zu zeigen.


#d
s20 = rep(NA,1000)                # Vektor mit 1000 NA Werten
for (i in 1:1000){                # forschleife geht für 1000 mal durch
  s20[i] <- sum(rt(n=20, df=1))   # Innerhalb der Schleife wird 's20[i]' auf die Summe von 20 t-verteilten Zufallszahlen gesetzt.
  # 'rt(n=20, df=1)' generiert 20 t-verteilte Zufallszahlen mit einem Freiheitsgrad (df=1).
  
}
hist(s20, freq=F, breaks=30)      # Erstellt Hist aus s20, freq=F macht relative Häufigkeiten statt absoluter Häufigkeiten, mit 30 Balken
mean(s20)                         # macht mittelwert aus s20

