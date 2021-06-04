## Lernziele:

## 
## 
## - Wissen, wobei R pingelig ist

## - Die Varianten von "x=y" in R auseinanderhalten können

## - Wissen, was ein Argument einer Funktion ist

## - Verstehen, was man mit vektoriellem Rechnen meint, und wissen, wie man es anwendet

## 

## ----variable, echo = FALSE, fig.cap = "Sinnbild: Eine Variable definieren", out.width = "30%", cache = FALSE----
stopifnot(file.exists("images/Rahmen/Variablen_zuweisen-crop.pdf"))
knitr::include_graphics("images/Rahmen/Variablen_zuweisen-crop.pdf")


## -----------------------------------------
temperatur <- 9


## -----------------------------------------
temperatur


## An den meisten Stellen darf man Leerzeichen setzen, wenn man mit R spricht (Befehle erteilt). Der Zuweisungspfeil ist eine Ausnahme: es ist "verboten", `< -` zu schreiben, also ein Leerzeichen zwischen dem Kleinerzeichen und dem Minuszeichen einzufügen. R würde diesen Versuch mit einer Fehlermeldung quittieren, da R dann etwas von einem Kleinerzeichen und einem Minuszeichen liest und nicht ahnt, dass Sie ein Zuweisungszeichen schreiben wollten.

## 

## ----hallo, eval = FALSE------------------
## y <- 'Hallo R!'


## ----eval = FALSE-------------------------
## y <- x


## ----eval = FALSE-------------------------
## ein_mittelwert <- mean(temperatur)


## ----read-csv-example, eval = FALSE-------
## #ok:
## ein_mittelwert <- mean(x = temperatur, na.rm = FALSE)
## ein_mittelwert <- mean(temperatur, FALSE)
## 
## # ohno:
## ein_mittelwert <- mean(FALSE, temperatur)
## 


## Einen R-Befehl\index{R-Befehl, Funktion} (synonym: Funktion) erkennt man in R daran, dass er einen Namen hat, der von runden Klammern gefolgt ist, z.B. `mean(x)`, `play_it_again() oder find_question(42)`. Es ist bei machen Funktionen auch möglich, dass nichts (kein Parameter) in den Klammern steht. So, wie Menschen in Häusern wohnen, "wohnen" R-Funktionen in R-Paketen. Da es viele R-Pakete gibt, passiert es immer wieder, dass ein Funktionsname in mehreren Paketen existiert. Ein Beispiel ist die Funktion `filter()`, die es im Standard-R gibt (Paket `base`) oder z.B. im Paket `dplyr`. Da sind Verwechslungen vorprogrammiert: "Nein, ich meinte den anderen Schorsch Müller!" Um zu spezifizieren, aus welchem Paket man eine Funktion beziehen will, benutzt man diese Schreibweise: `dplyr::filter()`. Damit weiß R, dass Sie `filter()` aus dem Paket `dplyr` meinen. Falls es zu merkwürdigen Fehlermeldungen kommt, kann so eine Funktionen-Verwechslung vorliegen. Geben Sie dann den Paketnamen mit an, mit Hilfe des doppelten Doppelpunkts\index{::, doppelter Doppelpunkt}. Abschnitt \@ref(funs-pckgs) erläutert, wie man herausfindet, in welchem Paket (oder welchen Paketen) eine Funktion beheimatet ist. Übrigens: Gibt man den Paketnamen mit an, so muss das Paket *nicht* geladen sein, ansonsten schon. Probieren Sie einmal aus: `fortunes::fortune(50)` -- aber vergessen Sie nicht, das Paket `fortunes` zu installieren (s. Abschnitt \@ref(cran)).

## 

## -----------------------------------------
2 < 3


## -----------------------------------------
temperatur < 10
temperatur == 10


## "X gleich Y" hat in R drei Gesichter:

## 
## 1. `x <- y` (oder `x = y`) weist `x` den Wert von y zu.

## 2. `x == y` prüft, ob `x` und `y` identisch sind.

## 3. `fun(x = y)` weist innerhalb der Funktion `fun()` dem Argument `x` den Wert der Variablen `y` zu.

## 

## -----------------------------------------
# Temperatur zwischen 0 und 10 Grad?
temperatur < 10 & temperatur > 0


## ----logik, echo = FALSE------------------
logik_tab <- readr::read_csv("includes/logik.csv")

knitr::kable(logik_tab, caption = "Logische Operatoren in R",
booktabs = T)


## -----------------------------------------
temperaturen <- c(32, 54, 0)  # Temperaturen in Fahrenheit


## -----------------------------------------
temperaturen_celcius <- (temperaturen - 32) * 5/9
temperaturen_celcius


## ----vek-rechnen-tab, echo = FALSE--------
d <- data.frame(
          id = c(1L, 2L, 3L, 4L, 5L),
           x = c(2L, 3L, 1L, 2L, 3L),
           y = c(1L, 5L, 2L, 2L, 3L),
           z = c(3L, 8L, 3L, 4L, 6L)
)

knitr::kable(d, caption = "Sinnbild für x+y=z, vektoriell gerechnet")



## Unter vektoriellem Rechnen bzw. vektoriellem Verarbeiten wird die Tatsache verstanden, dass R eine Operation auf alle Elemente eines Vektors ausführt.

## 

## 1. Datenanalyse wird in diesem Buch in folgende fünf Schritte eingeteilt: Daten einlesen, Daten aufbereiten, Daten visualisieren und modellieren, schließlich die Ergebnisse kommunizieren.

## 2. Eine Variable ist ein Platzhalter für einen oder mehrere Werte.

## 3. Eine Variable ist etwas anderes als ein Objekt.

## 4. Mit `temp` = 9 wird der Variablen `9` der Wert `temp` zugewiesen.

## 5. Durch Eingabe des Namens eines Objekts wird dieses ausgelesen.

## 6. Der Zuweisungspfeil `<-` darf auch (d.h. korrekte R-Schreibweise) so geschrieben werden `< -`.

## 7. Das Argument oder die Argumente einer R-Funktion werden stets in eckigen Klammern geschrieben.

## 8. Um auf Gleichheit zu prüfen, ist diese R-Schreibweise korrekt: `temperatur = 9`.

## 9. `fun(x = y)` weist innerhalb der Funktion `fun()` dem Argument `x` den Wert der globalen Variablen `y` zu.

## 10. Unter vektoriellem Rechnen bzw. vektoriellem Verarbeiten wird die Tatsache verstanden, dass R eine Operation auf alle Vektoren in der Arbeitsumgebung  ausführt.

## 
## 
