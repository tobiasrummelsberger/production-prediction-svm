# WICHTIG: Library discretization, Library binr und die unten aufgeführten Funktionen laden!
library(discretization)
library(binr)

setwd("C:/Users/Administrator/Documents/Hauptseminar SVM")

# Equal-Frequency-Binning mit Package binr: gibt einen Vektor mit den Gruppenwerten zurück
discretize <- function(matrix_column, target.bins, exact.groups) {
  matrix_column_bins <- bins(matrix_column, target.bins = target.bins, exact.groups = exact.groups, minpts = 5000)
  matrix_column_bins_interval <- bins.getvals(matrix_column_bins)
  matrix_column_disc <- cut(matrix_column, breaks = matrix_column_bins_interval, labels = FALSE, right = FALSE)
  return (matrix_column_disc)
}

# Normieren (/Skalieren) der Stellgrößen (Min-Max-Normalisierung)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

########################################################
# Laden des Datensatzes und Speichern als RDS-Datei (Data.frame) // Korrektur des Spaltennamens
Produktionsdaten <- read.csv2(paste("./Data/", "Produktionsdaten2_80_400000.csv", sep = ""))
Produktionsdaten$CarrierSpeedEmpty <- Produktionsdaten$CarrierSpeedEmtpy
Produktionsdaten$CarrierSpeedEmtpy <- NULL
saveRDS(Produktionsdaten, "./Data/Produktionsdaten.rds")

# Lesen des Datensatzes
Produktionsdaten <- readRDS("./Data/Produktionsdaten.rds")

########################################################
# Leistungskennzahlen
# Erstellen einer Matrix mit den Leistungskennzahlen
Leistungskennzahlen <- data.frame(cbind("Throughput" = Produktionsdaten$Throughput, "CycleTime" = Produktionsdaten$CycleTime, "Buffer1Mean" = Produktionsdaten$Buffer1Mean, "Buffer2Mean" = Produktionsdaten$Buffer2Mean, "Buffer1Max" = Produktionsdaten$Buffer1Max, "Buffer2Max" = Produktionsdaten$Buffer2Max, "Machine1Utilization" = Produktionsdaten$Machine1Utilization, "Machine2Utilization" = Produktionsdaten$Machine2Utilization, "Machine3Utilization" = Produktionsdaten$Machine3Utilization, "Machine4Utilization" = Produktionsdaten$Machine4Utilization))
saveRDS(Leistungskennzahlen, "./Data/Leistungskennzahlen.rds")

# Verschieben des Kommas um 2 Stellen:
Leistungskennzahlen$Buffer1Mean <- Leistungskennzahlen$Buffer1Mean*100
Leistungskennzahlen$Buffer2Mean <- Leistungskennzahlen$Buffer2Mean*100
saveRDS(Leistungskennzahlen, "./Data/Leistungskennzahlen.rds")

Leistungskennzahlen <- readRDS("./Data/Leistungskennzahlen.rds")

# Diskretisieren
# Initialisierung eines leeren Dataframes
Leistungskennzahlen_diskretisiert <- data.frame(1:400000)
# Diskretisieren der Leistungskennzahlen in 5 Klassen
for(i in 1:length(Leistungskennzahlen)){
  print(paste("Diskretisieren von Leistungskennzahl", names(Leistungskennzahlen[i])))
  diskretisiert <- discretize(Leistungskennzahlen[[i]], 5, 5)
  diskretisiert_faktor <- factor(diskretisiert)
  Leistungskennzahlen_diskretisiert <- cbind(Leistungskennzahlen_diskretisiert, diskretisiert_faktor)
}
Leistungskennzahlen_diskretisiert[1] <- NULL

# Namen der Spalten übernehmen
Leistungskennzahlen_diskretisiert <- setNames(Leistungskennzahlen_diskretisiert, c(names(Leistungskennzahlen)))

# Buffer1Max und Buffer2Max bestanden bereits aus diskreten Werten
Leistungskennzahlen_diskretisiert$Buffer1Max <- factor(Leistungskennzahlen$Buffer1Max)
Leistungskennzahlen_diskretisiert$Buffer2Max <- factor(Leistungskennzahlen$Buffer2Max)
saveRDS(Leistungskennzahlen_diskretisiert, "./Data/Leistungskennzahlen_diskretisiert.rds")

########################################################
# Stellgrößen
# Erstellen einer Matrix mit den Stellgrößen
Stellgroessen <- data.frame(cbind("LoadingVolume" = Produktionsdaten$LoadingVolume, "Buffer1" = Produktionsdaten$Buffer1, "Buffer2" = Produktionsdaten$Buffer2, "NumberOfCarriers" = Produktionsdaten$NumberOfCarriers, "CarrierSpeedLoaded" = Produktionsdaten$CarrierSpeedLoaded, "CarrierSpeedEmpty" = Produktionsdaten$CarrierSpeedEmpty, "Product1" = Produktionsdaten$Product1, "Product2" = Produktionsdaten$Product2, "Product3" = Produktionsdaten$Product3, "Product4" = Produktionsdaten$Product4, "Product5" = Produktionsdaten$Product5, "Product6" = Produktionsdaten$Product6, "Product7" = Produktionsdaten$Product7, "Product8" = Produktionsdaten$Product8, "Product9" = Produktionsdaten$Product9, "Product10" = Produktionsdaten$Product10, "Product11" = Produktionsdaten$Product11, "Product12" = Produktionsdaten$Product12, "Product13" = Produktionsdaten$Product13, "Product14" = Produktionsdaten$Product14, "Product15" = Produktionsdaten$Product15, "Product16" = Produktionsdaten$Product16, "Product17" = Produktionsdaten$Product17, "Product18" = Produktionsdaten$Product18, "Product19" = Produktionsdaten$Product19, "Product20" = Produktionsdaten$Product20, "Product21" = Produktionsdaten$Product21, "Product22" = Produktionsdaten$Product22, "Product23" = Produktionsdaten$Product23, "Product24" = Produktionsdaten$Product24, "Product25" = Produktionsdaten$Product25, "Product26" = Produktionsdaten$Product26, "Product27" = Produktionsdaten$Product27, "Machine1Speed" = Produktionsdaten$Machine1Speed, "Machine2Speed" = Produktionsdaten$Machine2Speed, "Machine3Speed" = Produktionsdaten$Machine3Speed, "Machine4Speed" = Produktionsdaten$Machine4Speed))
saveRDS(Stellgroessen, "./Data/Stellgroessen.rds")

# Normalisieren
Stellgroessen_normalisiert <- as.data.frame(lapply(Stellgroessen, normalize))
saveRDS(Stellgroessen_normalisiert, "./Data/Stellgroessen_normalisiert.rds")

########################################################
# Zusammenführen der Stellgrößen und der Leistungskennzahlen

# Daten für die Klassifikation
# Zusammenführen der normierten Stellgroessen und der diskretisierten Leistungskennzahlen
Produktionsdaten_vorbearbeitet_Classification <- data.frame(Stellgroessen_normalisiert, Leistungskennzahlen_diskretisiert)
summary(Produktionsdaten_vorbearbeitet_Classification)
saveRDS(Produktionsdaten_vorbearbeitet_Classification, "./Data/Produktionsdaten_vorbearbeitet_Classification.rds")

# Daten für die Regression
# Zusammenführen der normierten Stellgroessen und der Leistungskennzahlen
Stellgroessen_normalisiert <- readRDS("./Data/Stellgroessen_normalisiert.rds")
Leistungskennzahlen <- readRDS("./Data/Leistungskennzahlen.rds")
Produktionsdaten_vorbearbeitet_Regression <- data.frame(Stellgroessen_normalisiert, Leistungskennzahlen)
Produktionsdaten_vorbearbeitet_Regression$Buffer1Max <- NULL
Produktionsdaten_vorbearbeitet_Regression$Buffer2Max <- NULL
summary(Produktionsdaten_vorbearbeitet_Regression)
saveRDS(Produktionsdaten_vorbearbeitet_Regression, "./Data/Produktionsdaten_vorbearbeitet_Regression.rds")