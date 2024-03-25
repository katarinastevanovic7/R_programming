# Übungsblatt 8 / Uebungsblatt 8

#Aufgabe1
#a
plot(0:300, dpois(0:300, lambda = 100), type="h", xlab="Anzahl Anfragen an einem Tag")
# Poisson-verteilung, Wahrscheinlichkeiten für das Auftreten von 0 bis 300 Anfragen 
# an einem Tag zu berechnen, wobei die durchschnittliche Rate (λ) 100 Anfragen pro Tag beträgt. 

#b
simu = rpois(1:86400, lambda =100/86400)
plot(1:(84600/12), simu[1:(84600/12)], type = "h",  main ="Servernanfragen an einem Tag (Simulation)")

#c
times = (1:84600)[simu==1]
diffs = diff(times)
hist(diffs, 20)

#d optional
library("car")
qqPlot(diffs, distribution = "exp")



#Aufgabe 2 im Heft
