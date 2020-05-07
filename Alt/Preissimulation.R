#Installatioon und Laden der benötigten Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readxl")
install.packages("stringr")
library("dplyr") 
library("ggplot2")
library("tidyr")
library("readxl")
library("stringr")

#Einlesen der Rohdaten
setwd("~/R/wd/Tag 1")
df = read.delim("Rel_Jan18-Dez18_BOA.txt", sep = ";")

#Spaltenvektor erstellen
Spalten = c("Kalenderjahr / Monat","Artikelnr","TextArtikelgeneration","Artikelklasse","TextArtikelklasse","Ermässigung","TextErmässigung","TextFahrart","Klasse","Abgangsbahnhof","TextAbgangsbahnhof","Bestimmungsbahnhof", "TextBestimmungsbahnhof", "RelationsstringmitVIA","AnzVerkauf", "UmsatzVerkauf")

#Datensatz kopieren
df2 = df

#Spalten umbenennen
names(df2)[1:16] = Spalten

#Filterung
#Entfernen der nicht benötigten Artikelklassen
df2$Artikelklasse = as.character(df2$Artikelklasse)
Artikelfilter = c("000059","000061","000062","000064","0000065","0000105","000106","000107","000120","000162","000188","000190")
df3 = filter(df2, !(df2$Artikelklasse %in%  Artikelfilter))

#Entfernen der speziellen, nicht benötigten Artikelnummern
Ausnahmen = c(4004, 1880, 4433, 6991, 1672, 4106, 2669, 6957, 983, 1151, 1788, 199, 88, 871, 208, 891, 5445, 80, 2024, 2042, 2030, 2032, 3500, 1745, 1744, 1738, 114, 115, 616, 1227, 5689, 616, 1198, 186, 782, 293, 232, 376, 372)
df4 = filter(df3, !(df3$Artikelnr %in%  Ausnahmen ))

#Entfernen der nicht benötigten Fahrarten
FalscheFahrarten = c("Keine Fahrart","Rundfahrt", "")
df5 = filter(df4, !(df4$`TextFahrart` %in%  FalscheFahrarten))

#Entfernen der nicht benötigten Klassen
FalscheKlassen = c("0", "", "Keine Fahrart")
df6 = filter(df5, !(df5$Klasse %in%  FalscheKlassen))

#Entfernen der nicht benötigten Ermässigungen
FalscheErmässigungen = c("","041","046","050","056","062","081","099","100","101","102","105","120","122","126","127","160","216","218","219","220","221","222","223","229","230","236","237","238","240","242","RailAway")
df7 = filter(df6, !(df6$Ermässigung %in%  FalscheErmässigungen))

#Entfernen der leeren Relationenstrings und der leeren Umsätze und Absätze
df8 = filter(df7, !(df7$`RelationsstringmitVIA` %in%  ""))
df8 = filter(df8, !is.na(df8$RelationsstringmitVIA))
df8$AnzVerkauf = as.character(df8$AnzVerkauf)
df8$UmsatzVerkauf = as.character(df8$UmsatzVerkauf)
df8$AnzVerkauf = as.numeric(df8$AnzVerkauf)
df8$UmsatzVerkauf = as.numeric(df8$UmsatzVerkauf)
df8 = filter(df8, df8$AnzVerkauf > 0)
df8 = filter(df8, df8$UmsatzVerkauf > 0) 

#Aggregation
#Retourfahrten duplizieren
Retourfahrten =  filter(df8, (df8$`TextFahrart` == "Retour"))
df9 = rbind(df8, Retourfahrten)
df9$`TextFahrart` = "Einfach Simple"

#Vollpreise in Reduziert 1/2  duplizieren
Vollpreisfahrten =  filter(df9, (df9$`TextErmässigung` == "1/2"))
df10 = rbind(df9, Vollpreisfahrten)
df10$Ermässigung = 0.5

#Umwandlung der Factors in numerics
df10$Artikelklasse = as.character(df10$Artikelklasse)
df10$AnzVerkauf = as.character(df10$AnzVerkauf)
df10$UmsatzVerkauf = as.character(df10$UmsatzVerkauf)
df10$Artikelklasse = as.integer(df10$Artikelklasse)
df10$AnzVerkauf = as.numeric(df10$AnzVerkauf)
df10$UmsatzVerkauf = as.numeric(df10$UmsatzVerkauf)

#Daten aggregieren
df10_agg = df10  %>% group_by(Klasse, Ermässigung, TextFahrart,  RelationsstringmitVIA) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE))
df10_agg_sortZeit = df10  %>% group_by(`Kalenderjahr / Monat`) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE))


#Analyse
#Umwandlung der Factors in numerics
df2$Artikelklasse = as.character(df2$Artikelklasse)
df2$AnzVerkauf = as.character(df2$AnzVerkauf)
df2$UmsatzVerkauf = as.character(df2$UmsatzVerkauf)
df2$Artikelklasse = as.integer(df2$Artikelklasse)
df2$AnzVerkauf = as.numeric(df2$AnzVerkauf)
df2$UmsatzVerkauf = as.numeric(df2$UmsatzVerkauf)

#Analysewerte vor Filterung
#Verteilung Absatz und Umsatz über verschiedene Artikelklassen
df2 %>% group_by(Artikelklasse) %>% summarize(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE) / 1e6, UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE) / 1e6) %>% arrange(desc(UmsatzVerkauf.sum))

#Umformen der aggregierten Daten für Plots
df10_agg_sortABS =arrange(df10_agg, desc(AnzVerkauf.sum))
df10_agg_sortABS$AnzVerkauf.sum.cum = cumsum(df10_agg_sortABS$AnzVerkauf.sum)
df10_agg_sortABS$AnzVerkauf.sum.rel = df10_agg_sortABS$AnzVerkauf.sum.cum / sum(df10_agg_sortABS$AnzVerkauf.sum)
df10_agg_sortABS$UmsatzVerkauf.sum.cum = cumsum(df10_agg_sortABS$UmsatzVerkauf.sum)
df10_agg_sortABS$UmsatzVerkauf.sum.rel = df10_agg_sortABS$UmsatzVerkauf.sum.cum / sum(df10_agg_sortABS$UmsatzVerkauf.sum)

df10_agg_sortUMS =arrange(df10_agg, desc(UmsatzVerkauf.sum))
df10_agg_sortUMS$UmsatzVerkauf.sum.cum = cumsum(df10_agg_sortUMS$UmsatzVerkauf.sum)
df10_agg_sortUMS$UmsatzVerkauf.sum.rel = df10_agg_sortUMS$UmsatzVerkauf.sum.cum / sum(df10_agg_sortUMS$UmsatzVerkauf.sum)
df10_agg_sortUMS$AnzVerkauf.sum.cum = cumsum(df10_agg_sortUMS$AnzVerkauf.sum)
df10_agg_sortUMS$AnzVerkauf.sum.rel = df10_agg_sortUMS$AnzVerkauf.sum.cum / sum(df10_agg_sortUMS$AnzVerkauf.sum)

#Umsatz im Zeitverlauf plot
df10_agg_sortZeit$UmsatzVerkauf.sum = df10_agg_sortZeit$UmsatzVerkauf.sum / 1e6
par(las=1)
par(mar=c(5,8,4,1)+.1)
plot(df10_agg_sortZeit$UmsatzVerkauf.sum, type = "l", main = "Umsatz nach Zeit", ylab = "", xlab = "Datum")
title(ylab="Umsatz (in Mio.)", line=4, cex.lab=1.2)

#Verteilung Relationen nach Umsatz
plot(head(df10_agg_sortABS$UmsatzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Umsatz", ylab = "Prozentualer Anteil am Gesamtumsatz", xlab = "Relationen (absteigend sortiert nach Absatz)")
points(head(df10_agg_sortABS$UmsatzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 50000, col = 3)

plot(head(df10_agg_sortUMS$UmsatzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Umsatz", ylab = "Prozentualer Anteil am Gesamtumsatz", xlab = "Relationen (absteigend sortiert nach Umsatz)")
points(head(df10_agg_sortUMS$UmsatzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 50000, col = 3)

plot(head(df10_agg_sortABS$AnzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Absatz", ylab = "Prozentualer Anteil am Gesamtabsatz", xlab = "Relationen (absteigend sortiert nach Absatz)")
points(head(df10_agg_sortABS$AnzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 50000, col = 3)

plot(head(df10_agg_sortUMS$AnzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Absatz", ylab = "Prozentualer Anteil am Gesamtabsatz", xlab = "Relationen (absteigend sortiert nach Umsatz)")
points(head(df10_agg_sortUMS$AnzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 50000, col = 3)

#Preis pro Relation berechnen
df10_agg_sortUMS = arrange(df10_agg, desc(UmsatzVerkauf.sum))
df10_agg_sortUMS$Preis <- df10_agg_sortUMS$UmsatzVerkauf.sum / df10_agg_sortUMS$AnzVerkauf.sum

#Sample erstellen
Nsamp1 <- 50000
exportTop5000Umsatz <- df10_agg_sortUMS[1:Nsamp1,]

#ReferenzID erstellen
exportTop5000Umsatz$ReferenzID = c(1000:50999)

#Abgang und Bestimmung abtrennen
exportTop5000Umsatz = separate(exportTop5000Umsatz, RelationsstringmitVIA, into = c("Abgang", "Bestimmung"), sep = " ", remove = FALSE)

#VIAs aus Relationsstring löschen
OhneVIAs = grepl(" VIA ", exportTop5000Umsatz$RelationsstringmitVIA )
exportTop5000Umsatz$RelationsstringohneVIA = gsub( ".*VIA ",  exportTop5000Umsatz$RelationsstringmitVIA, replacement = " " )
i= 1
while (i < length(exportTop5000Umsatz$RelationsstringohneVIA)) {
  if ( OhneVIAs[i]== FALSE) {
    exportTop5000Umsatz$RelationsstringohneVIA[i] = ""
  } 
  i =i+1 
  print(i)
} 
rm(i)

#laden des UIC Files und anpassen der Strings
UICCodes = read_xlsx("Mutationsregeln_UIC-Code.xlsx")
Löschen = filter(UICCodes, (UICCodes$Befehl == "LÖSCHEN"))
Löschen$BHOF_KURZBEZ =Löschen$BHOF_KURZBEZ[order(nchar(Löschen$BHOF_KURZBEZ), Löschen$BHOF_KURZBEZ, decreasing = TRUE)]
Ersetzen = filter(UICCodes, (UICCodes$Befehl == "ERSETZEN"))
exportTop5000Umsatz$RelationsstringohneVIA = str_trim(exportTop5000Umsatz$RelationsstringohneVIA , side = "both")
i= 1
while (i < length(exportTop5000Umsatz$RelationsstringohneVIA)+1) {
  exportTop5000Umsatz$RelationsstringohneVIA[i] = str_pad(exportTop5000Umsatz$RelationsstringohneVIA[i] , width= nchar(exportTop5000Umsatz$RelationsstringohneVIA[i])+2, side = "both")
  i=i+1
} 
rm(i)
exportTop5000Umsatz$RelationsstringohneVIA = str_pad(exportTop5000Umsatz$RelationsstringohneVIA , width= 2, side = "both")
Löschen$BHOF_KURZBEZ = str_trim(Löschen$BHOF_KURZBEZ , side = "both")

i= 1
while (i < length(Löschen$BHOF_KURZBEZ)+1) {
  Löschen$BHOF_KURZBEZ[i] = str_pad(Löschen$BHOF_KURZBEZ[i] , width= nchar(Löschen$BHOF_KURZBEZ[i])+2, side = "both")
  i=i+1
} 
rm(i)


#Unerwünschte Strings aus Relationsstring löschen 
#Eventuell erst unerwünschte bei Abgang und Bestimmung Relationen löschen, danach die unerwüschten aus dem Relationsstring löschen
i= 1
while (i< length(Löschen$BHOF_KURZBEZ)) {
  x = Löschen$BHOF_KURZBEZ[i]
 exportTop5000Umsatz$RelationsstringohneVIA= gsub( x,"", exportTop5000Umsatz$RelationsstringohneVIA)
i =i+1 
print(i)
} 
rm(i)
exportTop5000Umsatz$RelationsstringohneVIA = str_trim(exportTop5000Umsatz$RelationsstringohneVIA , side = "both")

##Nur zum Rausfinden der Fehler
i= 1
while (i< length(Löschen$BHOF_KURZBEZ)) {
  x = Löschen$BHOF_KURZBEZ[i]
  exportTop5000Umsatz$Abgang= gsub( x,"", exportTop5000Umsatz$Abgang)
  i =i+1 
  print(i)
} 
rm(i)
#Unerwünschte Strings im Relationsstring ersetzen
# for(i in lenght(Ersetzen)) {
#  gsub( Ersetzen[i], exportTop5000Umsatz$RelationsstringohneVIA, replacement = ErsetzenMit[i] )
#  }

#Relationsstring auftrennen in mehrere Spalten und UIC Code Spalten einfügen
exportTop5000Umsatz = separate(exportTop5000Umsatz, RelationsstringohneVIA, into = c("Via1", "Via2", "Via3", "Via4", "Via5", "Via6", "Via7", "Via8", "Via9", "Via10", "Via11", "Via12", "Via13", "Via14"), sep = " ", remove = TRUE)
exportTop5000Umsatz[is.na(exportTop5000Umsatz)] <- ""
exportTop5000Umsatz$UICCodeAbgang = ""
exportTop5000Umsatz$UICCodeBestimmung = ""
exportTop5000Umsatz$UICCodeVia1 = ""
exportTop5000Umsatz$UICCodeVia2 = ""
exportTop5000Umsatz$UICCodeVia3 = ""
exportTop5000Umsatz$UICCodeVia4 = ""
exportTop5000Umsatz$UICCodeVia5 = ""
exportTop5000Umsatz$UICCodeVia6 = ""
exportTop5000Umsatz$UICCodeVia7 = ""
exportTop5000Umsatz$UICCodeVia8 = ""
exportTop5000Umsatz$UICCodeVia9 = ""
exportTop5000Umsatz$UICCodeVia10 = ""
exportTop5000Umsatz$UICCodeVia11 = ""
exportTop5000Umsatz$UICCodeVia12 = ""
exportTop5000Umsatz$UICCodeVia13 = ""
exportTop5000Umsatz$UICCodeVia14 = ""
exportTop5000Umsatz$Absatz.w = ""
exportTop5000Umsatz$Umsatz.w = ""

#UIC Codes joinen (left)
UICCodes= UICCodes[1:2]
Test  =merge(x=exportTop5000Umsatz,y=UICCodes, by.x="Abgang", by.y="BHOF_KURZBEZ", all.x=TRUE)


#Spalten neu anordnen und umbennenen für Excel Zwischenfile
exportTop5000Umsatz = exportTop5000Umsatz %>% select(ReferenzID, Klasse, Ermässigung, TextFahrart, RelationsstringmitVIA, Abgang, UICCodeAbgang, Bestimmung, UICCodeBestimmung, Via1, UICCodeVia1, Via2, UICCodeVia2, Via3, UICCodeVia3, Via4, UICCodeVia4, Via5, UICCodeVia5, Via6, UICCodeVia6, Via7, UICCodeVia7, Via8, UICCodeVia8, Via9, UICCodeVia9, Via10, UICCodeVia10, Via11,UICCodeVia11, Via12, UICCodeVia12, Via13, UICCodeVia13, Via14, UICCodeVia14, AnzVerkauf.sum, UmsatzVerkauf.sum, Preis,  Absatz.w, Umsatz.w) 
Spaltennamen = c( "ReferenzID" , "Klasse", "Ermässigung", "Fahrart", "RelationsstringmitVIA", "Abgang", "UIC-Code Abgang", "Bestimmung", "UIC-Code Bestimmung", "1. Via", "UIC-Code 1. Via", "2. Via", "UIC-Code 2. Via", "3. Via", "UIC-Code 3. Via", "4. Via", "UIC-Code 4. Via", "5. Via", "UIC-Code 5. Via", "6. Via", "UIC-Code 6. Via", "7. Via", "UIC-Code 7. Via", "8. Via", "UIC-Code 8. Via", "9. Via", "UIC-Code 9. Via", "10. Via", "UIC-Code 10. Via", "11. Via", "UIC-Code 11. Via", "12. Via", "UIC-Code 12. Via", "13. Via", "UIC-Code 13. Via", "14. Via", "UIC-Code 14. Via",  "Absatz.sum", "Umsatz.sum", "Preis", "Absatz.w", "Umsatz.w")
names(exportTop5000Umsatz)[1:42] = Spaltennamen

#Sample in .csv exportieren
write.table(exportTop5000Umsatz, 
            file = "Export_fuer_Preissimulation_top5000nachUmsatz.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)

#Gibt es NA?
apply(df10_agg, 2, function(x) any(is.na(x) | is.infinite(x)))
