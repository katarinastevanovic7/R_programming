
# Übungsblatt 11 / Uebungsblatt 11

#Augabe1

#a

xs <- c(-0.033, -0.76, 2.02, 1.13, 0.65, 0.49, 0.76, 0.40, 0.10, 0.61) # Definiert eine Stichprobe von Datenpunkten

l <- function(xs, mu, sigma) { # Definiert eine Funktion `l`, die die Log-Likelihood für eine Normalverteilung berechnet
  lik <- 0;                   # Initialisiert die Likelihood als 0
  for (x in xs) { # Durchläuft alle Datenpunkte in xs
    lik <- lik + log(dnorm(x=x, mean=mu, sd=sigma)); # Summiert die Logarithmen der Dichtewerte der Normalverteilung mit den Parametern mu und sigma
    
  }
  return (lik); # Gibt die berechnete Log-Likelihood zurück
}
l(xs,0,1) # Ruft die Funktion `l` mit den Parametern mu=0 und sigma=1 für die Datenpunkte xs auf


#b

require ('manipulate') # Lädt das 'manipulate'-Paket für interaktive Grafiken in RStudio
manipulate (          # Erstellt eine interaktive Grafik, um die Likelihood-Funktion zu visualisieren
  { 
    xvals <- seq(-5,5,0.05);    # Erstellt eine Sequenz von x-Werten für die Darstellung der Normalverteilung
    lik <- l(xs, mu, sigma);    # Berechnet die Log-Likelihood für die aktuellen Werte von mu und sigma
    plot(xvals, dnorm(xvals, mean=mu, sd=sigma),
         type="l",xlab="Daten", ylab="", main=paste0("Likelihood = ",
                                                     round(lik,digits=3)), xlim=c(-2,2)); 
    points(xs, rep(0,length(xs)),cex=2)   # Zeichnet die Normalverteilung und markiert die Datenpunkte xs
  },
  mu = slider (-5 ,5, initial=0.1, step=0.01),
  sigma = slider (0,5, initial=0.1, step=0.01)  # Definiert Slider für die interaktive Auswahl von mu und sigma
)

#c
min = -Inf    # Initialisiert den minimalen Likelihood-Wert als negativ unendlich (-Inf)
mu.min = NA;
sig.min = NA;  # Initialisiert Variablen für die Werte von mu und sigma, die zur maximalen Likelihood führen, als NA (nicht verfügbar)
for (mu in seq(0.5,0.6,0.001)) {
  for(sigma in seq(0.5,0.6,0.001)) {  # Durchläuft ein Gitter von Werten für mu und sigma innerhalb des Bereichs 0.5 bis 0.6 in kleinen Schritten (0.001)
    lik <- l(xs, mu, sigma)           # Berechnet die Likelihood für die aktuellen Werte von mu und sigma
    if (lik > min){                   # Prüft, ob die aktuelle Likelihood größer als das bisherige Minimum ist
      min <- lik
      mu.min <- mu;
      sig.min <- sigma;               # Aktualisiert das Minimum und die zugehörigen Werte von mu und sigma
      
    }
    
  }
  
}
min;mu.min;sig.min     # Gibt die maximale Likelihood und die Parameterwerte aus, die zu dieser maximalen Likelihood führen

#d

lik = function(x) {     # Definiert eine neue Funktion `lik` für die Optimierung, die -Log-Likelihood zurückgibt
  return (-l(xs, mu=x[1], sigma=x[2]))
}
optim(c(0.5,0.5), lik)  # Verwendet die `optim`-Funktion, um die Parameter zu finden, die die Log-Likelihood minimieren (entspricht der Maximierung der Likelihood)



#e)
# mit diesem Regler von manipulate

mean(xs)                    # Mittelwert der Daten
sqrt(var(xs))               # Standardabweichung der Daten
sqrt(mean((xs-mean(xs))^2)) # Standardabweichung der Daten



#Aufgabe2
w_x = function(x, N, m, k){ # Definition einer Funktion zur Berechnung der hypergeometrischen Wahrscheinlichkeit
  return(dhyper(x, m, N-m, k)) # Rückgabe der hypergeometrischen Wahrscheinlichkeit
  #Weisse Kugeln entsprechen den markierten Fischen.
  #Schwarz Kugel den nicht markierten Fischen, diese sind N-m
  # dhyper berechnet die Wahrscheinlichkeit, x markierte Objekte zu ziehen,
  # gegeben eine Gesamtpopulation N, von der m markiert und N-m nicht markiert sind.
  
}

#a
N = 200       # Definition der Gesamtpopulation (N), 
m=k=30        #Anzahl der markierten (m) und Stichprobengröße (k)

plot(0:30, w_x(0:30, N,m,k), type="h", xlab = "Anzahl der markierten im zweiten Fang", ylab="m/k") # Erstellt ein Plot der hypergeometrischen Wahrscheinlichkeiten für das Ziehen von 0 bis 30 markierten Objekten
# type="h" erzeugt ein Histogramm-ähnliches Diagramm mit vertikalen Linien für jede Wahrscheinlichkeit

#b

m=k=30
ps = w_x(x=7, N=1:400, m,k)    # Berechnet die Wahrscheinlichkeiten für das Ziehen von genau 7 markierten Objekten über eine Spanne von N-Werten
p_max = max(ps, na.rm = TRUE)  # Findet die maximale Wahrscheinlichkeit in den berechneten Wahrscheinlichkeiten
which(ps == p_max)             # Identifiziert den Wert von N, der die maximale Wahrscheinlichkeit für das Ziehen von genau 7 markierten Objekten liefert
