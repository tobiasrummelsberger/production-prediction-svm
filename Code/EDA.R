# Exploratory Data Analysis
# Zusammenfassung der Daten und Visualisierung mittels Boxplot-Diagrammen
# Erfassen der Intervalle und Datentypen der Attribute
# Grundlage für die Vorverarbeitung der Daten (insb. Normalisierung, Diskretisierung)

# Lesen des Datensatzes
Produktionsdaten <- readRDS("./Data/Produktionsdaten.rds")

# alle Daten
summary(Produktionsdaten)
str(Produktionsdaten)

# Stellgrößen
# LoadingVolume
boxplot(Produktionsdaten$LoadingVolume)
range(Produktionsdaten$LoadingVolume)
summary(Produktionsdaten$LoadingVolume)

# Buffer1
boxplot(Produktionsdaten$Buffer1)
range(Produktionsdaten$Buffer1)
summary(Produktionsdaten$Buffer1)

# Buffer2
boxplot(Produktionsdaten$Buffer2)
range(Produktionsdaten$Buffer2)
summary(Produktionsdaten$Buffer2)

# NumberOfCarriers
boxplot(Produktionsdaten$NumberOfCarriers)
range(Produktionsdaten$NumberOfCarriers)
summary(Produktionsdaten$NumberOfCarriers)

# CarrierSpeedLoaded
boxplot(Produktionsdaten$CarrierSpeedLoaded)
range(Produktionsdaten$CarrierSpeedLoaded)
summary(Produktionsdaten$CarrierSpeedLoaded)

# CarrierSpeedEmpty
boxplot(Produktionsdaten$CarrierSpeedEmpty)
range(Produktionsdaten$CarrierSpeedEmpty)
summary(Produktionsdaten$CarrierSpeedEmpty)

# Produktmix

# MachineXSpeed 
# Machine1
boxplot(Produktionsdaten$Machine1Speed)
range(Produktionsdaten$Machine1Speed)
summary(Produktionsdaten$Machine1Speed)

# Machine2
boxplot(Produktionsdaten$Machine2Speed)
range(Produktionsdaten$Machine2Speed)
summary(Produktionsdaten$Machine2Speed)

# Machine3
boxplot(Produktionsdaten$Machine3Speed)
range(Produktionsdaten$Machine3Speed)
summary(Produktionsdaten$Machine3Speed)

# Machine4
boxplot(Produktionsdaten$Machine4Speed)
range(Produktionsdaten$Machine4Speed)
summary(Produktionsdaten$Machine4Speed)

# Leistungskennzahlen
# Throughput
hist(Produktionsdaten$Throughput)

# Buffer1Mean
hist(Produktionsdaten$Buffer1Mean)

# Bufer2Mean
hist(Produktionsdaten$Buffer2Mean)

# Buffer1Max

# Buffer2Max

# CycleTimne
hist(Produktionsdaten$CycleTime)

# MachineXUtilization
# Machine1
hist(Produktionsdaten$Machine1Utilization)
# Machine2
hist(Produktionsdaten$Machine2Utilization)
# Machine3
hist(Produktionsdaten$Machine3Utilization)
# Machine4
hist(Produktionsdaten$Machine4Utilization)
