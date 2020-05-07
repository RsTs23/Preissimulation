#Installation und Laden der benötigten Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readxl")
install.packages("stringr")
install.packages("sjmisc")
library("dplyr") 
library("ggplot2")
library("tidyr")
library("readxl")
library("stringr")
library("sjmisc")

#Einlesen und verbinden der Rohdaten und Garbage collection, Optionen anpassen
gc()
options(scipen = 999)
setwd("C:/Users/DataCareer Guest/Desktop/Basistarif 2")
df = read.delim("CH_direct_BT2_NOV18_OKT19.txt", sep = ";")

#Spaltenvektor der Spaltenbezeichnungen erstellen
Spalten = c("Kalenderjahr / Monat","Produktnr","ProduktnrText","Kundensegment","TextKundensegment","TextFahrart","Klasse","Fahrart", "Abgangsbahnhof","TextAbgangsbahnhof","Bestimmungsbahnhof", "TextBestimmungsbahnhof","Wegangabe","Wegangabecode","VIA1","VIA2","VIA3","VIA4","VIA5","AnzVerkauf","UmsatzVerkauf")

#Datensatz kopieren und nicht benötigte VIAS entfernen
df2 = subset(df, select = -c(20:34))

#Spalten umbenennen
names(df2)[1:21] = Spalten

#Filterung
#Entfernen der speziellen, nicht benötigten Produktnummern
Ausnahmen = c(50166, 80175)
df3 = filter(df2, !(df2$Produktnr %in%  Ausnahmen ))
df3$TextKundensegment = as.character(df3$TextKundensegment)
df3$AnzVerkauf = as.numeric(df3$AnzVerkauf)
df3$UmsatzVerkauf = as.numeric(df3$UmsatzVerkauf)

#Entfernen der leeren Abgangsbahnhöfe, Bestimmungsbahnhöfe und der leeren Umsätze und Absätze und Kundensegmente
df3$Wegangabe = as.character(df3$Wegangabe) 
df3$TextAbgangsbahnhof = as.character(df3$TextAbgangsbahnhof) 
df3$TextBestimmungsbahnhof = as.character(df3$TextBestimmungsbahnhof) 
df5 = filter(df3, !(df3$TextAbgangsbahnhof %in%  ""))
df5 = filter(df5, !(df5$TextBestimmungsbahnhof %in%  ""))
df5$AnzVerkauf = as.character(df5$AnzVerkauf)
df5$AnzVerkauf = as.numeric(df5$AnzVerkauf)
df5 = filter(df5, df5$AnzVerkauf > 0)
df5 = filter(df5, df5$UmsatzVerkauf > 0) 
df8 = df5

#Aggregation
#Daten aggregieren 
df8$TextAbgangsbahnhof = as.character(df8$TextAbgangsbahnhof)
df8$TextBestimmungsbahnhof = as.character(df8$TextBestimmungsbahnhof)
df8$Abgangsbahnhof = as.numeric(df8$Abgangsbahnhof)
df8$Bestimmungsbahnhof = as.numeric(df8$Bestimmungsbahnhof)

# Wegangabecode kürzen auf 5 Vias
df8$Wegangabecode = strtrim(df8$Wegangabecode, 59)

df8_agg = df8  %>% group_by(Klasse, Produktnr, Kundensegment, TextFahrart, Abgangsbahnhof, TextAbgangsbahnhof, Bestimmungsbahnhof, TextBestimmungsbahnhof, Wegangabe, Wegangabecode,VIA1,VIA2,VIA3,VIA4,VIA5) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE))

#Nicht benötigte Daten entfernen.  
rm(df)
rm(df5)
rm(Ausnahmen)
rm(Spalten)
gc()

#Reverse String zusammensetzen
df9 = df8_agg
df9$Reverse = paste(df9$Bestimmungsbahnhof, df9$Abgangsbahnhof, df9$VIA5, df9$VIA4, df9$VIA3, df9$VIA2, df9$VIA1)
df9$Reverse = gsub(" 0", "", df9$Reverse)
df9$Wegangabecode = gsub("VIA", "", df9$Wegangabecode)

# Wahlwege anpassen
Mutationsregeln = read_xlsx("Mutationsregeln.xlsx")
Wahlstrecken = filter(Mutationsregeln, (Mutationsregeln$Befehl == "WAHLWEG"))

#Ersetzen der Wahlstrecken
i = 1
while (i < length(Wahlstrecken$Wegangabe)+1) {
  df9$Wegangabecode = ifelse(grepl(Wahlstrecken$Wegangabe[i], df9$Wegangabe), gsub(Wahlstrecken$`UIC-Codes`[i], "", df9$Wegangabecode), df9$Wegangabecode)
  df9$Reverse = ifelse(grepl(Wahlstrecken$Wegangabe[i], df9$Wegangabe), gsub(Wahlstrecken$`UIC-Codes`[i], "", df9$Reverse), df9$Reverse)
  i = i+1
  print(i)
}
rm(i)
df9$Wegangabecode = gsub("  ", " ", df9$Wegangabecode)
df9$Reverse = gsub("  ", " ", df9$Reverse)

# Löschen nicht benötigter Spalten und Vorbereiten der Strings
df9 = subset(df9, select = -c(11:15))
df9$Wegangabecode = str_trim(df9$Wegangabecode , side = "both")
df9$Reverse = str_trim(df9$Reverse , side = "both")
rm(Mutationsregeln)
rm(Wahlstrecken)
rm(df8)
rm(df8_agg)

# Spalten umkehren, wenn eine identische Angabe in Reverse existiert
i = 1
while (i < length(df9$Wegangabecode)+1) {
df9$Wegangabecode[i] = ifelse(df9$Wegangabecode[i] %in% df9$Reverse, df9$Wegangabecode[df9$Wegangabecode==df9$Wegangabecode[i]] <-df9$Reverse[i], df9$Wegangabecode[i] <-df9$Wegangabecode[i])
print(i)
i = i+1
}
rm(i)

x = df9$TextBestimmungsbahnhof[df9$Wegangabecode == df9$Reverse]

df9$TextBestimmungsbahnhof[df9$Wegangabecode == df9$Reverse] = df9$TextAbgangsbahnhof[df9$Wegangabecode == df9$Reverse]
df9$TextAbgangsbahnhof[df9$Wegangabecode == df9$Reverse] = x
rm(x)
#Export vor Aggregierung
write.table(df9, 
            file = "1Umgekehrte Relationen.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)

df9_agg = df9  %>% group_by(Klasse, Produktnr, Kundensegment, TextAbgangsbahnhof, TextBestimmungsbahnhof, TextFahrart, Wegangabecode) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf.sum, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf.sum, na.rm = TRUE))

#Zwischenspeicherung der Daten
write.table(df9_agg, 
            file = "Umgekehrte Relationen.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
df9_agg = read.delim("Umgekehrte Relationen.csv", sep= ";")

#Umformen der aggregierten Daten
df9_agg_sortABS =arrange(df9_agg, desc(AnzVerkauf.sum))
df9_agg_sortABS$AnzVerkauf.sum.cum = cumsum(df9_agg_sortABS$AnzVerkauf.sum)
df9_agg_sortABS$AnzVerkauf.sum.rel = df9_agg_sortABS$AnzVerkauf.sum.cum / sum(df9_agg_sortABS$AnzVerkauf.sum)
df9_agg_sortABS$UmsatzVerkauf.sum.cum = cumsum(df9_agg_sortABS$UmsatzVerkauf.sum)
df9_agg_sortABS$UmsatzVerkauf.sum.rel = df9_agg_sortABS$UmsatzVerkauf.sum.cum / sum(df9_agg_sortABS$UmsatzVerkauf.sum)

df9_agg_sortUMS =arrange(df9_agg, desc(UmsatzVerkauf.sum))
df9_agg_sortUMS$UmsatzVerkauf.sum.cum = cumsum(df9_agg_sortUMS$UmsatzVerkauf.sum)
df9_agg_sortUMS$UmsatzVerkauf.sum.rel = df9_agg_sortUMS$UmsatzVerkauf.sum.cum / sum(df9_agg_sortUMS$UmsatzVerkauf.sum)
df9_agg_sortUMS$AnzVerkauf.sum.cum = cumsum(df9_agg_sortUMS$AnzVerkauf.sum)
df9_agg_sortUMS$AnzVerkauf.sum.rel = df9_agg_sortUMS$AnzVerkauf.sum.cum / sum(df9_agg_sortUMS$AnzVerkauf.sum)

#Preis pro Relation berechnen
df9_agg_sortUMS = arrange(df9_agg, desc(UmsatzVerkauf.sum))
df9_agg_sortUMS$Preis <- df9_agg_sortUMS$UmsatzVerkauf.sum / df9_agg_sortUMS$AnzVerkauf.sum

#Einlesen der Mutationsregeln
Mutationsregeln = read_xlsx("Mutationsregeln.xlsx")
Loeschen = filter(Mutationsregeln, (Mutationsregeln$Befehl == "LÖSCHEN"))
Ersetzen = filter(Mutationsregeln, (Mutationsregeln$Befehl == "ERSETZEN"))

#Ausführen der Mutationsregeln
i = 1
while (i < length(Ersetzen$`UIC-Codes`)+1) {
  df9_agg_sortUMS$Wegangabecode = ifelse(grepl(Ersetzen$`UIC-Codes`[i], df9_agg_sortUMS$Wegangabecode), df9_agg_sortUMS$Wegangabecode, gsub(Ersetzen$`UIC-Codes`[i], Ersetzen$`Ersetzen durch UIC-Codes`[i], df9_agg_sortUMS$Wegangabecode))
  i = i+1
  print(i)
}
rm(i)

i = 1
while (i < length(Loeschen$`UIC-Codes`)+1) {
  df9_agg_sortUMS$Wegangabecode = gsub(Loeschen$`UIC-Codes`[i], "", df9_agg_sortUMS$Wegangabecode)
  i = i+1
  print(i)
}
rm(i)

df9_agg_sortUMS$Wegangabecode = gsub("  ", " ", df9_agg_sortUMS$Wegangabecode)

#Sample1 erstellen
exportTopUmsatz <- df9_agg_sortUMS

#ReferenzID erstellen
exportTopUmsatz$ReferenzID = c(1000:(length(exportTopUmsatz$Klasse)+999))

#Wegangabe auftrennen in mehrere Spalten und Gewichtete Preis Spalten einfügen
exportTopUmsatz = separate(exportTopUmsatz, Wegangabecode, into = c( "Abgangsbahnhof","Bestimmungsbahnhof", "Via1", "Via2", "Via3", "Via4", "Via5"), sep = " ", remove = FALSE)
exportTopUmsatz[is.na(exportTopUmsatz)] <- 0
exportTopUmsatz$Absatz.w = ""
exportTopUmsatz$Umsatz.w = ""

# Gewichteten Preis berechnen
exportTopUmsatz$Absatz.w <- exportTopUmsatz$AnzVerkauf.sum / sum(df3$AnzVerkauf, na.rm = TRUE)
exportTopUmsatz$Umsatz.w <- exportTopUmsatz$UmsatzVerkauf.sum / sum(df3$UmsatzVerkauf, na.rm = TRUE)

#Spalten neu anordnen und umbenennen für Excel Zwischenfile
exportTopUmsatz = exportTopUmsatz %>% select(ReferenzID, Produktnr, Klasse, Kundensegment, TextFahrart,TextAbgangsbahnhof, Abgangsbahnhof, TextBestimmungsbahnhof, Bestimmungsbahnhof,  Via1, Via2, Via3, Via4,Via5,  AnzVerkauf.sum, UmsatzVerkauf.sum,  Absatz.w, Umsatz.w) 
Spaltennamen = c( "ReferenzID", "Produktnr", "Klasse", "Kundensegment", "TextFahrart", "Abgangsbahnhof", "UIC-Code Abgangsbahnhof", "Bestimmungsbahnhof", "UIC-Code Bestimmungsbahnhof", "UIC-Code 1. Via", "UIC-Code 2. Via", "UIC-Code 3. Via", "UIC-Code 4. Via", "UIC-Code 5. Via",  "Absatz.sum", "Umsatz.sum", "Absatz.w", "Umsatz.w")
names(exportTopUmsatz) = Spaltennamen

#Anpassungen Variablentypen
exportTopUmsatz$`UIC-Code Abgangsbahnhof` = str_trim(exportTopUmsatz$`UIC-Code Abgangsbahnhof` , side = "both")
exportTopUmsatz$`UIC-Code Bestimmungsbahnhof` = str_trim(exportTopUmsatz$`UIC-Code Bestimmungsbahnhof` , side = "both")
exportTopUmsatz$`UIC-Code 1. Via` = str_trim(exportTopUmsatz$`UIC-Code 1. Via` , side = "both")
exportTopUmsatz$`UIC-Code 2. Via` = str_trim(exportTopUmsatz$`UIC-Code 2. Via` , side = "both")
exportTopUmsatz$`UIC-Code 3. Via` = str_trim(exportTopUmsatz$`UIC-Code 3. Via` , side = "both")
exportTopUmsatz$`UIC-Code 4. Via` = str_trim(exportTopUmsatz$`UIC-Code 4. Via` , side = "both")
exportTopUmsatz$`UIC-Code 5. Via` = str_trim(exportTopUmsatz$`UIC-Code 5. Via` , side = "both")

exportTopUmsatz$`UIC-Code Abgangsbahnhof` = as.numeric(exportTopUmsatz$`UIC-Code Abgangsbahnhof`)
exportTopUmsatz$`UIC-Code Bestimmungsbahnhof` = as.numeric(exportTopUmsatz$`UIC-Code Bestimmungsbahnhof`)
exportTopUmsatz$`UIC-Code 1. Via` = as.numeric(exportTopUmsatz$`UIC-Code 1. Via`)
exportTopUmsatz$`UIC-Code 2. Via` = as.numeric(exportTopUmsatz$`UIC-Code 2. Via`)
exportTopUmsatz$`UIC-Code 3. Via` = as.numeric(exportTopUmsatz$`UIC-Code 3. Via`)
exportTopUmsatz$`UIC-Code 4. Via` = as.numeric(exportTopUmsatz$`UIC-Code 4. Via`)
exportTopUmsatz$`UIC-Code 5. Via` = as.numeric(exportTopUmsatz$`UIC-Code 5. Via`)

#Sample in .csv exportieren
write.table(format(exportTopUmsatz, scientific=FALSE), 
            file = "Export_fuer_Preissimulation_nachUmsatzBasistarif2.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)

