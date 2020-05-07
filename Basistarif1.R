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
setwd("C:/Users/DataCareer Guest/Desktop/Basistarif 1")
df = read.delim("ch_direct_201811_201812.txt", sep = ";")
df = rbind(df, read.delim("ch_direct_201901_201904.txt", sep = ";"))
df = rbind(df, read.delim("ch_direct_201905_201908.txt", sep = ";"))
df = rbind(df, read.delim("ch_direct_201909_201910.txt", sep = ";"))

#Spaltenvektor der Spaltenbezeichnungen erstellen
Spalten = c("Kalenderjahr / Monat","Produktnr","ProduktnrText","Kundensegment","TextKundensegment","TextFahrart","Klasse","Fahrart", "Abgangsbahnhof","TextAbgangsbahnhof","Bestimmungsbahnhof", "TextBestimmungsbahnhof","Wegangabe","Wegangabecode","VIA1","VIA2","VIA3","VIA4","VIA5","AnzVerkauf","UmsatzVerkauf")

#Datensatz kopieren und nicht benötigte VIAS entfernen
df2 = subset(df, select = -c(20:34))

#Spalten umbenennen
names(df2)[1:21] = Spalten

#Filterung
#Entfernen der speziellen, nicht benötigten Produktnummern
Ausnahmen = c(980125, 4004)
df3 = filter(df2, !(df2$Produktnr %in%  Ausnahmen ))

#Entfernen der nicht benötigten Kundensegmenten
df3$TextKundensegment = as.character(df3$TextKundensegment)
df3$AnzVerkauf = as.numeric(df3$AnzVerkauf)
df3$UmsatzVerkauf = as.numeric(df3$UmsatzVerkauf)
FalscheKundensegmenten = c("", "Juniorkarte", "Kinder-Mitfahrkarte")
df4 = filter(df3, !(df3$TextKundensegment %in%  FalscheKundensegmenten))
FalscheKundensegmente = c( 3, 26, 89,90, 91,92)
df4 = filter(df4, !(df4$Kundensegment %in%  FalscheKundensegmente))

#Entfernen der leeren Abgangsbahnhöfe, Bestimmungsbahnhöfe und der leeren Umsätze und Absätze und Kundensegmente
df4$Wegangabe = as.character(df4$Wegangabe) 
df4$TextAbgangsbahnhof = as.character(df4$TextAbgangsbahnhof) 
df4$TextBestimmungsbahnhof = as.character(df4$TextBestimmungsbahnhof) 
df5 = filter(df4, !(df4$TextAbgangsbahnhof %in%  ""))
df5 = filter(df5, !(df5$TextBestimmungsbahnhof %in%  ""))
df5$AnzVerkauf = as.character(df5$AnzVerkauf)
df5$AnzVerkauf = as.numeric(df5$AnzVerkauf)
df5 = filter(df5, df5$AnzVerkauf > 0)
df5 = filter(df5, df5$UmsatzVerkauf > 0) 

#Artikelnummer 5361 umwandeln
df5361 = filter(df5, (df5$Produktnr == 5361 ))
df5361$UmsatzVerkauf = df5361$UmsatzVerkauf / 6 
df5361$AnzVerkauf = df5361$AnzVerkauf/ 6
df6 = rbind(df5, df5361)
df6 = rbind(df6, df5361)

#Retourfahrten duplizieren
df6$TextFahrart = as.character(df6$TextFahrart)
df6$UmsatzVerkauf[df6$`TextFahrart` == "RETOUR"] = df6$UmsatzVerkauf[df6$`TextFahrart` == "RETOUR"] * 0.5
Retourfahrten =  filter(df6, (df6$`TextFahrart` == "RETOUR")) 
df7 = rbind(df6, Retourfahrten)
df7$`TextFahrart` = "EINFACH"

#Preis pro Relation berechnen
df7$Preis <- df7$UmsatzVerkauf / df7$AnzVerkauf
df7$Kundensegment[df7$TextKundensegment == "Reduziert 1/2"] = "Reduziert 1/2"

#Vollpreise in Reduziert 1/2  duplizieren (Ausnahme für Billette zu 3.- und 3.60)
Vollpreisfahrten =  filter(df7, (df7$Kundensegment == 19))
Vollpreisfahrten = rbind(Vollpreisfahrten, filter(df7, (df7$Kundensegment == 55)))
df7 = df7[!(df7$Kundensegment == 19),]
df7 = df7[!(df7$Kundensegment == 55),]
Dreierfahrten =  filter(Vollpreisfahrten, (Vollpreisfahrten$Preis == 3))
Dreisechzigfahrten =  filter(Vollpreisfahrten, (Vollpreisfahrten$Preis == 3.6))
Vollpreisfahrten = filter(Vollpreisfahrten, !(Vollpreisfahrten$Preis == 3))
Vollpreisfahrten = filter(Vollpreisfahrten, !(Vollpreisfahrten$Preis == 3.6))
Vollpreisfahrten$Preis = Vollpreisfahrten$Preis /2
Vollpreisfahrten$UmsatzVerkauf = Vollpreisfahrten$UmsatzVerkauf /2
Vollpreisfahrten$Kundensegment = "HTA"
Vollpreisfahrten$TextKundensegment = "HTA"
df8 = rbind(df7, Vollpreisfahrten)
df8 = rbind(df8, Vollpreisfahrten)
df8 = rbind(df8, Dreierfahrten)
df8 = rbind(df8, Dreisechzigfahrten)

#Aggregation
#Daten aggregieren 
df8$TextAbgangsbahnhof = as.character(df8$TextAbgangsbahnhof)
df8$TextBestimmungsbahnhof = as.character(df8$TextBestimmungsbahnhof)
df8$Abgangsbahnhof = as.numeric(df8$Abgangsbahnhof)
df8$Bestimmungsbahnhof = as.numeric(df8$Bestimmungsbahnhof)

# Wegangabecode kürzen auf 5 Vias
df8$Wegangabecode = strtrim(df8$Wegangabecode, 59)

df8_agg = df8  %>% group_by(Klasse, Kundensegment, TextFahrart, Abgangsbahnhof, TextAbgangsbahnhof, Bestimmungsbahnhof, TextBestimmungsbahnhof, Wegangabe, Wegangabecode,VIA1,VIA2,VIA3,VIA4,VIA5) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE))

#Nicht benötigte Daten entfernen.  
rm(df)
rm(df4)
rm(df5)
rm(df6)
rm(df7)
rm(df5361)
rm(Vollpreisfahrten)
rm(Retourfahrten)
rm(Ausnahmen)
rm(FalscheKundensegmenten)
rm(FalscheKundensegmente)
rm(Dreierfahrten)
rm(Dreisechzigfahrten)
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
df9 = subset(df9, select = -c(10:14))
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

df9_agg = df9  %>% group_by(Klasse, Kundensegment, TextAbgangsbahnhof, TextBestimmungsbahnhof, TextFahrart, Wegangabecode) %>% summarise(AnzVerkauf.sum = sum(AnzVerkauf.sum, na.rm = TRUE), UmsatzVerkauf.sum = sum(UmsatzVerkauf.sum, na.rm = TRUE))

#Zwischenspeicherung der Daten
write.table(df9_agg, 
            file = "Umgekehrte Relationen.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
df9_agg = read.delim("Umgekehrte Relationen.csv", sep= ";")
#Analyse
#Umwandlung der Factors in numerics
df2$Produktnr = as.character(df2$Produktnr)
df2$AnzVerkauf = as.character(df2$AnzVerkauf)
df2$UmsatzVerkauf = as.character(df2$UmsatzVerkauf)
df2$Produktnr = as.integer(df2$Produktnr)
df2$AnzVerkauf = as.numeric(df2$AnzVerkauf)
df2$UmsatzVerkauf = as.numeric(df2$UmsatzVerkauf)

#Analysewerte vor Filterung
#Verteilung Absatz und Umsatz über verschiedene Produktnrn
#Neu nach Artikelnummer
df2 %>% group_by(Produktnr) %>% summarize(AnzVerkauf.sum = sum(AnzVerkauf, na.rm = TRUE) / 1e6, UmsatzVerkauf.sum = sum(UmsatzVerkauf, na.rm = TRUE) / 1e6) %>% arrange(desc(UmsatzVerkauf.sum))

#Umformen der aggregierten Daten für Plots
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

#Verteilung Relationen nach Umsatz
options(scipen = 999)  
plot(head(df9_agg_sortABS$UmsatzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Umsatz", ylab = "Prozentualer Anteil am Gesamtumsatz", xlab = "Relationen (absteigend sortiert nach Absatz)")
points(head(df9_agg_sortABS$UmsatzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 100000, col = 3)

plot(head(df9_agg_sortUMS$UmsatzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Umsatz", ylab = "Prozentualer Anteil am Gesamtumsatz", xlab = "Relationen (absteigend sortiert nach Umsatz)")
points(head(df9_agg_sortUMS$UmsatzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 100000, col = 3)

plot(head(df9_agg_sortABS$AnzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Absatz", ylab = "Prozentualer Anteil am Gesamtabsatz", xlab = "Relationen (absteigend sortiert nach Absatz)")
points(head(df9_agg_sortABS$AnzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 100000, col = 3)

plot(head(df9_agg_sortUMS$AnzVerkauf.sum.rel, 200000), type = "l", ylim = c(0,1), main = "Absatz", ylab = "Prozentualer Anteil am Gesamtabsatz", xlab = "Relationen (absteigend sortiert nach Umsatz)")
points(head(df9_agg_sortUMS$AnzVerkauf.sum.rel, 200000), type = "l", col = 2)
abline(v = 100000, col = 3)

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
Nsamp1 <- 100000
exportTop100000Umsatz <- df9_agg_sortUMS[1:Nsamp1,]

#Sample2 erstellen
set.seed(1234)  # make sample reproducible
Nsamp2 <- 100000 # sample size
samp2 <- sample(setdiff(1:nrow(df9_agg_sortUMS), 1:Nsamp1), Nsamp2)
exportSampleBottomUmsatz <- df9_agg_sortUMS[samp2,]
exportSampleBottomUmsatz$Absatz.w <- exportSampleBottomUmsatz$AnzVerkauf.sum / sum(df3$AnzVerkauf, na.rm = TRUE)
exportSampleBottomUmsatz$Umsatz.w <- exportSampleBottomUmsatz$UmsatzVerkauf.sum / sum(df3$UmsatzVerkauf, na.rm = TRUE)
exportSampleBottomUmsatz$ReferenzID = c(1000:(Nsamp2+999))
exportSampleBottomUmsatz = separate(exportSampleBottomUmsatz, Wegangabecode, into = c( "Abgangsbahnhof","Bestimmungsbahnhof", "Via1", "Via2", "Via3", "Via4", "Via5"), sep = " ", remove = FALSE)
exportSampleBottomUmsatz[is.na(exportSampleBottomUmsatz)] <- 0
exportSampleBottomUmsatz = exportSampleBottomUmsatz %>% select(ReferenzID, Klasse, Kundensegment, TextFahrart,TextAbgangsbahnhof, Abgangsbahnhof, TextBestimmungsbahnhof, Bestimmungsbahnhof,  Via1, Via2, Via3, Via4,Via5,  AnzVerkauf.sum, UmsatzVerkauf.sum,  Absatz.w, Umsatz.w) 
Spaltennamen = c( "ReferenzID" , "Klasse", "Kundensegment", "TextFahrart", "Abgangsbahnhof", "UIC-Code Abgangsbahnhof", "Bestimmungsbahnhof", "UIC-Code Bestimmungsbahnhof", "UIC-Code 1. Via", "UIC-Code 2. Via", "UIC-Code 3. Via", "UIC-Code 4. Via", "UIC-Code 5. Via",  "Absatz.sum", "Umsatz.sum", "Absatz.w", "Umsatz.w")
names(exportSampleBottomUmsatz) = Spaltennamen

#Analyse Sample2
sd(exportSampleBottomUmsatz$Umsatz.w)
sd(exportSampleBottomUmsatz$Umsatz.sum)

#Anpassungen Variablentypen
exportSampleBottomUmsatz$`UIC-Code Abgangsbahnhof` = str_trim(exportSampleBottomUmsatz$`UIC-Code Abgangsbahnhof` , side = "both")
exportSampleBottomUmsatz$`UIC-Code Bestimmungsbahnhof` = str_trim(exportSampleBottomUmsatz$`UIC-Code Bestimmungsbahnhof` , side = "both")
exportSampleBottomUmsatz$`UIC-Code 1. Via` = str_trim(exportSampleBottomUmsatz$`UIC-Code 1. Via` , side = "both")
exportSampleBottomUmsatz$`UIC-Code 2. Via` = str_trim(exportSampleBottomUmsatz$`UIC-Code 2. Via` , side = "both")
exportSampleBottomUmsatz$`UIC-Code 3. Via` = str_trim(exportSampleBottomUmsatz$`UIC-Code 3. Via` , side = "both")
exportSampleBottomUmsatz$`UIC-Code 4. Via` = str_trim(exportSampleBottomUmsatz$`UIC-Code 4. Via` , side = "both")
exportSampleBottomUmsatz$`UIC-Code 5. Via` = str_trim(exportSampleBottomUmsatz$`UIC-Code 5. Via` , side = "both")

exportSampleBottomUmsatz$`UIC-Code Abgangsbahnhof` = as.numeric(exportSampleBottomUmsatz$`UIC-Code Abgangsbahnhof`)
exportSampleBottomUmsatz$`UIC-Code Bestimmungsbahnhof` = as.numeric(exportSampleBottomUmsatz$`UIC-Code Bestimmungsbahnhof`)
exportSampleBottomUmsatz$`UIC-Code 1. Via` = as.numeric(exportSampleBottomUmsatz$`UIC-Code 1. Via`)
exportSampleBottomUmsatz$`UIC-Code 2. Via` = as.numeric(exportSampleBottomUmsatz$`UIC-Code 2. Via`)
exportSampleBottomUmsatz$`UIC-Code 3. Via` = as.numeric(exportSampleBottomUmsatz$`UIC-Code 3. Via`)
exportSampleBottomUmsatz$`UIC-Code 4. Via` = as.numeric(exportSampleBottomUmsatz$`UIC-Code 4. Via`)
exportSampleBottomUmsatz$`UIC-Code 5. Via` = as.numeric(exportSampleBottomUmsatz$`UIC-Code 5. Via`)

## save exportSampleBottomUmsatz as csv file
write.table(format(exportSampleBottomUmsatz, scientific=FALSE), 
            file = "Export_fuer_Preissimulation_sampleAusBottomUmsatz.csv",
            sep = ";", dec = ".", row.names = FALSE)

#ReferenzID erstellen
exportTop100000Umsatz$ReferenzID = c(1000:(Nsamp1 + 999))

#Wegangabe auftrennen in mehrere Spalten und Gewichtete Preis Spalten einfügen
exportTop100000Umsatz = separate(exportTop100000Umsatz, Wegangabecode, into = c( "Abgangsbahnhof","Bestimmungsbahnhof", "Via1", "Via2", "Via3", "Via4", "Via5"), sep = " ", remove = FALSE)
exportTop100000Umsatz[is.na(exportTop100000Umsatz)] <- 0
exportTop100000Umsatz$Absatz.w = ""
exportTop100000Umsatz$Umsatz.w = ""

# Gewichteten Preis berechnen
exportTop100000Umsatz$Absatz.w <- exportTop100000Umsatz$AnzVerkauf.sum / sum(df3$AnzVerkauf, na.rm = TRUE)
exportTop100000Umsatz$Umsatz.w <- exportTop100000Umsatz$UmsatzVerkauf.sum / sum(df3$UmsatzVerkauf, na.rm = TRUE)

#Spalten neu anordnen und umbenennen für Excel Zwischenfile
exportTop100000Umsatz = exportTop100000Umsatz %>% select(ReferenzID, Klasse, Kundensegment, TextFahrart,TextAbgangsbahnhof, Abgangsbahnhof, TextBestimmungsbahnhof, Bestimmungsbahnhof,  Via1, Via2, Via3, Via4,Via5,  AnzVerkauf.sum, UmsatzVerkauf.sum,  Absatz.w, Umsatz.w) 
Spaltennamen = c( "ReferenzID" , "Klasse", "Kundensegment", "TextFahrart", "Abgangsbahnhof", "UIC-Code Abgangsbahnhof", "Bestimmungsbahnhof", "UIC-Code Bestimmungsbahnhof", "UIC-Code 1. Via", "UIC-Code 2. Via", "UIC-Code 3. Via", "UIC-Code 4. Via", "UIC-Code 5. Via",  "Absatz.sum", "Umsatz.sum", "Absatz.w", "Umsatz.w")
names(exportTop100000Umsatz) = Spaltennamen

#Anpassungen Variablentypen
exportTop100000Umsatz$`UIC-Code Abgangsbahnhof` = str_trim(exportTop100000Umsatz$`UIC-Code Abgangsbahnhof` , side = "both")
exportTop100000Umsatz$`UIC-Code Bestimmungsbahnhof` = str_trim(exportTop100000Umsatz$`UIC-Code Bestimmungsbahnhof` , side = "both")
exportTop100000Umsatz$`UIC-Code 1. Via` = str_trim(exportTop100000Umsatz$`UIC-Code 1. Via` , side = "both")
exportTop100000Umsatz$`UIC-Code 2. Via` = str_trim(exportTop100000Umsatz$`UIC-Code 2. Via` , side = "both")
exportTop100000Umsatz$`UIC-Code 3. Via` = str_trim(exportTop100000Umsatz$`UIC-Code 3. Via` , side = "both")
exportTop100000Umsatz$`UIC-Code 4. Via` = str_trim(exportTop100000Umsatz$`UIC-Code 4. Via` , side = "both")
exportTop100000Umsatz$`UIC-Code 5. Via` = str_trim(exportTop100000Umsatz$`UIC-Code 5. Via` , side = "both")

exportTop100000Umsatz$`UIC-Code Abgangsbahnhof` = as.numeric(exportTop100000Umsatz$`UIC-Code Abgangsbahnhof`)
exportTop100000Umsatz$`UIC-Code Bestimmungsbahnhof` = as.numeric(exportTop100000Umsatz$`UIC-Code Bestimmungsbahnhof`)
exportTop100000Umsatz$`UIC-Code 1. Via` = as.numeric(exportTop100000Umsatz$`UIC-Code 1. Via`)
exportTop100000Umsatz$`UIC-Code 2. Via` = as.numeric(exportTop100000Umsatz$`UIC-Code 2. Via`)
exportTop100000Umsatz$`UIC-Code 3. Via` = as.numeric(exportTop100000Umsatz$`UIC-Code 3. Via`)
exportTop100000Umsatz$`UIC-Code 4. Via` = as.numeric(exportTop100000Umsatz$`UIC-Code 4. Via`)
exportTop100000Umsatz$`UIC-Code 5. Via` = as.numeric(exportTop100000Umsatz$`UIC-Code 5. Via`)

#Sample in .csv exportieren
write.table(format(exportTop100000Umsatz, scientific=FALSE), 
            file = "Export_fuer_Preissimulation_top100000nachUmsatz.csv",
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)

