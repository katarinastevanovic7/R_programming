# Übungsblatt 10 / Uebungsblatt 10

#Aufgabe1

#a

vals.ecdf <- c(0.1, 0.5, 1.4, 2.9, 4.7, 7.3)

plot(ecdf(kunden_simu_data$x), xlab="wartezeit in minuten")
p = c(0.05, 0.25, 0.5, 0.75, 0.9, 0.975)
segments(x0 <- rep(-100, 6), y0 = p, vals.ecdf, y1 = p, col = "blue") # Zeichnet horizontale blaue Linien im ECDF-Plot
segments(x0 <- vals.ecdf, y0 = rep(0,6), x1 = vals.ecdf, y1 = p, col = "blue") # Zeichnet vertikale blaue Linien im ECDF-Plot

#b im Heft


#c

q <- c(0.05, 0.25, 0.5, 0.75, 0.9, 0.975)
(vals.165 <- -log(1-q) / 0.65)              # Berechnet Quantile für eine Exponentialverteilung mit λ = 0.65

(vals.150 <- -log(1-q) / 0.50)              # Berechnet Quantile für eine Exponentialverteilung mit λ = 0.50

plot(vals.ecdf, vals.150, t="p", col = "blue") # Plottet die berechneten Quantile für λ = 0.50 gegen 'vals.ecdf'
points(vals.ecdf, vals.165)                   # Fügt die berechneten Quantile für λ = 0.65 dem Plot hinzu
abline(a=0,b=1, col="black")                  # Zeichnet eine Referenzlinie y=x im Plot
legend(x="topleft", legend = c("lambda=0.5", "lambda=0.65", "y=1*x"),
       col=c("blue", "black", "green"), pch = c(1,1,-1))       # Fügt eine Legende hinzu, die die Farbkodierung erklärt
abline(lm(vals.165 - vals.ecdf))              # Zeichnet eine Regressionslinie basierend auf den Differenzen zwischen den Quantilen und 'vals.ecdf'

#d
par(mfrow = c(2,2))   # Stellt das Layout für die nächsten Plots auf 2x2

# Erstellt vier QQ-Plots für 'vals.ecdf' gegen Normalverteilungsquantile mit verschiedenen Mittelwerten und Standardabweichungen
plot(qnorm(q), vals.ecdf, main="mean = 0, sigma = 1")
plot(qnorm(q, mean = 10), vals.ecdf, main="mean = 10, sigma = 1")
plot(qnorm(q, mean= 0, sd= 10), vals.ecdf, main="mean = 0, sigma = 10")
plot(qnorm(q, mean= 10, sd= 10), vals.ecdf, main="mean = 10, sigma = 10")

par(mfrow = c(1,1))   # Stellt das Layout zurück auf 1x1 für zukünftige Plots

#e
library(car)          # lädt das Paket car für qqplot
par(mfrow = c(2,2))   # ändert format des Layouts auf 2x2

# Erstellt QQ-Plots für 'kunden_simu_data$x' gegen eine Normal- und eine Exponentialverteilung
qqPlot(kunden_simu_data$x)
qqPlot(kunden_simu_data$x, distribution = "exp")
par(mfrow = c(1,1))    # ändert format des Layouts auf 2x2


#Aufgabe2

#i

n = 3; N = 1000
s.var1 <- rep(NA, N)          # s.var1 und s.var2 sind Vektoren, die jeweils die Varianzschätzungen aus den 1000 Simulationen speichern. 
s.var2 <- s.var1              # rep(NA, N) initialisiert diese Vektoren mit NA-Werten, die später überschrieben werden.
for(i in 1:N){
  x <- rnorm(n)               # generiert eine Stichprobe von n normalverteilten Zufallszahlen mit einem Mittelwert von 0 und einer Standardabweichung von 1.
  m <- sum((x-mean(x))^2)     # berechnet die Summe der quadrierten Abweichungen der Stichprobenwerte vom Stichprobenmittelwert, was ein Schritt in der Berechnung der Varianz ist.
  s.var1[i] <- m/(n-1)        # speichert in s.var1 die Varianzschätzung für die i-te Simulation, wobei die Formel m/(n-1) die korrigierte Stichprobenvarianz (mit Bessel's Korrektur) verwendet, die einen unverzerrten Schätzer der Populationsvarianz darstellt.
  s.var2[i] <- m/n            # speichert in s.var2 die Varianzschätzung für die i-te Simulation ohne Bessel's Korrektur, was typischerweise zu einem leicht verzerrten Schätzer der Populationsvarianz führt, besonders bei kleinen Stichprobengrößen
}

par(mfrow = c(1,2))
hist(s.var1, main = expression(paste('Histogramm von ', s[1]^2, sep ='')), xlim = c(0,6)) # Histogramme
hist(s.var2, main = expression(paste('Histogramm von ', s[2]^2, sep ='')), xlim = c(0,6))
par(mfrow = c(1,1))
mean(s.var2) # MIttelwert von s.var2

#ii
n.tot <- 30               # Vektor mit Gesamtzahl der Stichprobengrößen
s.var1.n <- rep(0,n.tot)  # Vektor gefüllt mit Nullen
s.var2.n <- s.var1.n     

for ( n in 1:n.tot) {     # For-schleife, geht von 1 bis 30
  s.var1 <- rep(0,N)      # Für jede Stichprobengröße n initialisiert zwei Vektoren mit Nullen, 
  s.var2 <- s.var1        # um Varianzen für N Simulationen zu speichern
  for(i in 1:N){
    x <- rnorm(n)         # Generiert eine Stichprobe von n normalverteilten Zufallszahlen
    m <- sum( ( x-mean(x))*(x-mean(x))) #  # Berechnet die Summe der quadrierten Abweichungen vom Stichprobenmittelwert
    s.var1[i] <- m/(n/1)  # Speichert die Varianzschätzung mit Bessel's Korrektur in s.var1
    s.var2[i] <- m/n      # Speichert die Varianzschätzung ohne Bessel's Korrektur in s.var2
  }
  s.var1.n[n] <- mean(s.var1) # Speichert den Durchschnitt der Varianzschätzungen mit Bessel's Korrektur für die aktuelle Stichprobengröße
  s.var2.n[n] <- mean(s.var2) # Speichert den Durchschnitt der Varianzschätzungen ohne Bessel's Korrektur für die aktuelle Stichprobengröße
  
}
par( mfrow = c(1,1))
plot( 1:n.tot,s.var1.n, type ='o', ylim = c(0,1.5), col= 'red', xlab = 'n', ylab ='') # Erstellt einen Plot der durchschnittlichen Varianzschätzungen mit Bessel's Korrektur (s.var1.n) und ohne (s.var2.n)
lines( 1:n.tot,s.var2.n, type ='o', ylim = c(0,1.5), col= 'blue')                     # Fügt dem Plot die durchschnittlichen Varianzschätzungen ohne Bessel's Korrektur hinzu
legend( x = 'topright', legend = c(expression(s[1]^2), expression(s[2]^2)),           # Fügt eine Legende hinzu, um die Farben zu erklären
        col= c('red', 'blue'), bty = 'n',pch = 1, lty = 1)
abline( h = 1, lty = 2) # Zeichnet eine horizontale Linie bei y=1, um den erwarteten Wert der Varianz (unter der Annahme, dass die Daten normalverteilt sind) zu markieren


