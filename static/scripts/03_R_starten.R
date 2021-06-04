## Lernziele:

## 
## 
## - In der Lage sein, einige häufige technische Probleme zu lösen

## - R-Pakete installieren können

## - Einige grundlegende R-Funktionalitäten verstehen

## 

## ----libs-hidden-r-r-starten, echo = FALSE----
library(okcupiddata)
library(nycflights13)
library(MASS)
library(AER)
library(titanic)


## ----rstudio-screenshot, echo = FALSE, fig.cap = "RStudio"----
knitr::include_graphics("images/Rahmen/RStudio-Screenshot.png")


## Wenn Sie RStudio starten, startet R automatisch auch. Starten Sie daher, wenn

## Sie RStudio gestartet haben, *nicht* noch extra R. Damit hätten Sie sonst zwei

## Instanzen von R laufen, was zu Verwirrung (bei R und beim Nutzer) führen kann.

## Wer Shortcuts mag, wird in RStudio überschwänglich beschenkt; der Shortcut für die Shortcuts ist Shift-Alt-K.

## 

## ----fig-install-packages, echo = FALSE, fig.cap = "So installiert man Pakete in RStudio", out.width = "30%"----

knitr::include_graphics("images/Rahmen/install_packages.png")



## ----eval = FALSE-------------------------
## install.packages("tidyverse")


## Wann benutzt man bei R Anführungszeichen? Im Detail ist es Kraut und Rüben, aber die Grundregel

## lautet: Wenn man Text anspricht, braucht man Anführungsstriche. Sprechen Sie existierende Daten oder Befehle an, brauchen Sie hingegen keine Anführungsstriche. Aber es gibt Ausnahmen: Im Beispiel `library(tidyverse)` ist `tidyverse`

## hier erst mal für R nichts Bekanntes, weil noch nicht geladen. Demnach müssten

## *eigentlich* Anführungsstriche (einfache oder doppelte) stehen. Allerdings meinte ein Programmierer, dass

## es doch bequemer sei, so ohne Anführungsstriche -- spart man doch zwei Anschläge auf der Tastatur.

## Recht hat er. Aber bedenken Sie, dass es sich um die

## Ausnahme einer Regel handelt. Sie können also auch schreiben: `library("tidyverse")`, das entspricht der normalen R-Rechtschreibung.

## 

## Wenn Sie R bzw. RStudio schließen, werden alle gestarteten Pakete ebenfalls geschlossen. Sie müssen die benötigten Pakete beim erneuten Öffnen von RStudio wieder starten.

## 

## ----eval = FALSE-------------------------
## install.packages("devtools", dependencies = TRUE)
## library(devtools)
## install_github("sebastiansauer/pradadata")


## OH NO:

## 
## - `install.packages(teidyvörse)`

## - `install.packages("Tidyverse")`

## - `install.packages("tidy verse")`

## - Keine Internet-Verbindung beim Installieren von Paketen

## - `library(tidyverse)`  # ohne tidyverse vorher zu installieren

## 

## Verwenden Sie möglichst die neueste Version von R, RStudio und Ihres Betriebssystems. Um R oder R-Studio zu aktualisieren, laden Sie einfach die neueste Version herunter und installieren Sie sie. Ältere Versionen führen u.U. zu Problemen; je älter, desto Problem... Updaten Sie Ihre Packages regelmäßig z.B. mit `update.packages()`

## oder dem Button *Update* bei RStudio (Reiter `Packages`).

## 

## ----pakete-hilfe, echo = FALSE, fig.cap = "Hier werden Sie geholfen: Die Dokumentation der R-Pakete", out.width = "100%"----
imgs <- c("images/Rahmen/hilfe_pakete.png",
          "images/Rahmen/hilfe_funs.png")

#prada::comb2pngs(imgs)
knitr::include_graphics("images/Rahmen/werden_sie_geholfen-crop.pdf")


## 
## Manche Befehle haben Allerweltsnamen (z.B. `filter()`). Manchmal gibt es Befehle mit gleichem Namen in verschiedenen Paketen. Befehle mit Allerweltsnamen (wie `filter()`) sind Kandidaten für solcherlei Verwechslung (`mosaic::filter()` vs. `dplyr::filter()`). Falls Ihnen eine vertraute Funktion wirre Ausgaben  oder eine diffuse Fehlermeldung liefert, kann es daran liegen, dass R einen Befehl mit dem richtigen Namen, aber aus dem "falschen" Paket zieht. Geben Sie im Zweifel lieber den Namen des Pakets vor dem Paketnamen an, z.B. so `dplyr::filter()`. Der doppelte Doppelpunkt trennt den Paketnamen vom Namen der Funktion.

## 

## ----Arbeitsverzeichnis, echo = FALSE, fig.cap = "Das Arbeitsverzeichnis mit RStudio auswählen", out.width = "50%"----

knitr::include_graphics("images/tidy/Arbeitsverzeichnis.png")


## Spezifizieren Sie ein Arbeitsverzeichnis, wird dort der Inhalt Ihrer Umgebung (d.h. alle geladenen Daten/Objekte) gespeichert, wenn Sie RStudio schließen und die Option *Save Workspace on Exit* aktiviert haben (*Tools > Global Options > Save Workspace on Exit*). Den *Workspace* (synonym: Umgebung oder *global environment*) kann man sich als Sammelbecken für alle aktuell in R geladenen bzw. erzeugten Daten vorstellen. Allerdings sucht RStudio beim Starten *nicht* zwangsläufig in diesem Ordner. Per Doppelklick auf die Workspace-Datei `.Rdata` oder über *File > Open File ...* können Sie Ihren Workspace wieder laden. Es kann praktisch sein, die Objekte Ihrer letzten Arbeitssitzung wieder zur Verfügung zu haben. Ist man aber darauf aus, "schönen" R-Code zu schreiben, ist es teilweise sinnvoller, eine Skript-Datei erneut ablaufen zu lassen, um sich die Datenobjekte neu berechnen zu lassen; Hadley Wickham empfiehlt, nie den Workspace zu sichern bzw. wiederherzustellen [@rfuerds].

## 

## Wenn Sie jemanden um R-Hilfe bitten, dann sollten Sie Ihr Problem prägnant beschreiben, das nennen wir ein ERBie. Ein ERBie besteht aus vier Teilen: Syntax, Daten, Paketen und Infos zum laufenden System (R Version etc.)

## 

## 1. RStudio ist eine Entwicklungsumgebung für R.

## 2. Das Skriptfenster von RStudio stellt R-Ausgaben dar.

## 3. Die Konsole von RStudio gleicht der Konsole von R.

## 4. Nur Objekte, die im Fenster 'Umgebung' dargestellt sind, existieren in der aktuellen Sitzung.

## 5. R-Pakete dürfen keine Daten enthalten.

## 6. CRAN stellt R-Pakete gegen Entgelt zur Verfügung.

## 7. Beim Befehl `install.packages()` bedeutet das Argument `dependencies = TRUE`, dass frühere Versionen des Pakets mitinstalliert werden sollen.

## 8. Pakete müssen nach jedem Start von R neu installiert werden.

## 9. Startet man RStudio, so wird R automatisch gestartet.

## 10. Die Schreibweise `mosaic::filter()` meint, dass aus dem Paket `mosaic` die Funktion `filter()` verwendet werden soll.

## 
## 
