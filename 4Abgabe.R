# Übungsblatt 4 / Uebungsblatt 4

#Aufgabe1
#a
x <- c(-10:10)                # Erstellt einen Vektor x mit Werten von -10 bis 10
z <- x^2                      # Quadriert jeden Wert in x, um z zu erzeugen
par(mar= c(4,4,1,1))          # Setzt die Ränder für den Plot
plot(x, z)                    # Erstellt einen Plot von x gegen z

cor(x,z)                      # Berechnet die Pearson-Korrelation zwischen x und z
cor(x,z, method="spearman")   # Berechnet die Spearman-Korrelation zwischen x und z


#b
x <- c(-10:10)                # Erstellt einen Vektor x mit Werten von -10 bis 10
z <- x^2                      # Quadriert jeden Wert in x, um z zu erzeugen
par(mar= c(4,4,1,1))          # Setzt die Ränder für den Plot
plot(x, z)                    # Erstellt einen Plot von x gegen z

cor(x,z)                      # Berechnet die Pearson-Korrelation zwischen x und z
cor(x,z, method="spearman")   # Berechnet die Spearman-Korrelation zwischen x und z


#normale methode besser, da sie genauer ist

#c
x <- log(c(1:10))             # Wendet den Logarithmus auf die Werte 1 bis 10 an
z <- exp(x^2)                 # Berechnet z als die Exponentialfunktion von x^2
par(mar= c(4,4,1,1))          # Setzt die Ränder für den Plot
plot(x,z)                     # Plottet x gegen z

cor(x,z)                      # Berechnet die Pearson-Korrelation zwischen x und z
cor(x,z, method = "spearman") # Berechnet die Spearman-Korrelation zwischen x und z

#d
res = rep(NA, 1000)           # Initialisiert ein Ergebnisarray mit NA-Werten
for (i in 1:length(res)) {
  x <- rnorm(5)               # Generiert 5 normalverteilte Zufallszahlen für x
  y <- rnorm(5)               # Generiert 5 normalverteilte Zufallszahlen für y
  res[i] = cor(x,y)           # Berechnet die Korrelation zwischen x und y und speichert das Ergebnis
}
max(res)                      # Gibt den maximalen Korrelationswert aus
hist(res,30)                  # Erstellt ein Histogramm der Korrelationswerte mit 30 Bins


#e
res = rep(NA, 1000)           # Initialisiert ein Ergebnisarray mit NA-Werten
for (i in 1:length(res)) {
  x <- rnorm(50)              # Generiert 50 normalverteilte Zufallszahlen für x
  y <- rnorm(50)              # Generiert 50 normalverteilte Zufallszahlen für y
  res[i] = cor(x,y)           # Berechnet die Korrelation zwischen x und y und speichert das Ergebnis
}
max(res)                      # Gibt den maximalen Korrelationswert aus
hist(res,30)                  # Erstellt ein Histogramm der Korrelationswerte mit 30 Bins

#schlechtere korrelation weil weniger daten und dadurch größere abstände

#Aufgabe2
#a

get_data = function(n = 100){                # Diese Zeile definiert eine Funktion namens get_data, die einen optionalen Parameter n hat, der standardmäßig auf 100 gesetzt ist. n repräsentiert die Anzahl der Datenpunkte, die generiert werden sollen.
  gender = sample(c('F', 'M'), n, replace = TRUE) #Hier wird ein Vektor gender erstellt, der n Elemente enthält. Jedes Element ist entweder ‘F’ (für weiblich) oder ‘M’ (für männlich), zufällig ausgewählt.
  shoesize = rnorm(n, 45, 3)                 # Ein Vektor shoesize wird erstellt, der n normalverteilte Zufallszahlen enthält. Der Mittelwert dieser Verteilung ist 45 und die Standardabweichung ist 3.
  n_f = sum(gender == 'F')                   # Die Schuhgröße für Frauen wird neu generiert. n_f ist die Anzahl der Frauen in den Daten.
  shoesize[gender == 'F'] = rnorm(n_f, 40,3) #  Für diese Frauen wird die Schuhgröße als normalverteilte Zufallszahl mit einem Mittelwert von 40 und einer Standardabweichung von 3 generiert.
  
  salary = rnorm(n,50,10)                    # Das Gehalt ist normalverteilt, mit einem Mittelwert von 50 und einer Standardabweichung von 10 für alle,
  salary[gender == 'F'] = rnorm(n_f, 40,10)  # und einem Mittelwert von 40 und einer Standardabweichung von 10 für Frauen.
  return ( 
    data.frame(salary=salary, shoesize=shoesize, gender = as.factor(gender))
  )                                          # Die Funktion gibt einen Datenrahmen zurück, der die Gehälter, Schuhgrößen und das Geschlecht aller Individuen enthält.
}
df = get_data()                              # Hier wird die Funktion get_data aufgerufen und das Ergebnis in df gespeichert.
plot(df$salary, df$shoesize, col=df$gender)  # Schließlich wird ein Streudiagramm erstellt, das die Schuhgröße gegen das Gehalt aufträgt. Die Punkte sind nach Geschlecht gefärbt.

#b
res = rep(NA, 10)
for (i in 1:length(res)) {                   # Dies ist eine Schleife, die zehnmal durchläuft (da res zehn Elemente hat). 
  df = get_data()                            # In jedem Durchlauf der Schleife wird ein neuer Datensatz mit der Funktion get_data() generiert und die Korrelation zwischen salary und shoesize berechnet. 
  res[i] = cor(df$salary, df$shoesize)       # Diese Korrelation wird dann im i-ten Element von res gespeichert.     
}
round(res,2) # rundet die korrelationen auf 2 dezimalstellen

# Korrelation ist näher an der null als an der eins

#c
res = rep(NA, 1000)                          # Initialisiert ein Array `res` für 1000 Korrelationen.
for (i in 1:length(res)) {
  df = get_data()                            # Generiert in jedem Durchlauf neue Daten.
  res[i] = cor(df$salary[df$gender == 'F'], df$shoesize[df$gender == 'F']) # Berechnet die Korrelation zwischen `salary` und `shoesize` für Frauen.
}
plot(res)                                    # Erstellt ein Plot der Korrelationswerte.
round(res, 2) # Rundet die Korrelationswerte auf zwei Dezimalstellen.


# sehr nah an der Null, diesmal auch im minusbereich

