# WICHTIG: Library e1071 und die unten aufgeführten Funktionen laden!
library(e1071)

# Preprocessing
preprocessing <- function(path, name){
  # Einlesen der csv-Datei und Abspeichern als rds-Datei
  Testdaten <- read.csv2(paste(path, name, sep = ""))
  saveRDS(Testdaten, paste(path, name, ".rds", sep = ""))
  
  # Normalisieren (/Skalieren) der Stellgrößen (Min-Max-Normalisierung)
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  Testdaten_normalisiert <- as.data.frame(lapply(Testdaten, normalize))
  return(Testdaten_normalisiert)
}

# Regression
svm.regression <- function(features, path, dataset){
  for(i in features){
    print(paste("Regression des Features", i))
    model <- readRDS(paste(path, "SVR_", i, ".rds", sep = ""))
    prediction <- predict(model, dataset)
    dataset <- data.frame(cbind(dataset, prediction))
    colnames(dataset)[length(colnames(dataset))] <- paste("Prediction", i)
  }
  return(dataset)
}

# Klassifikation
svm.classification <- function(features, path, dataset){
  for(i in features){
    print(paste("Klassifikation des Features", i))
    model <- readRDS(paste(path, "SVC_", i, ".rds", sep = ""))
    prediction <- predict(model, dataset)
    dataset <- data.frame(cbind(dataset, name = prediction))
    colnames(dataset)[length(colnames(dataset))] <- paste("Prediction", i)
  }
  return(dataset)
}

########################################################
# Dateinamen und Verzeichnisse eingeben

# Datensatz, dessen Leistungskennzahlen vorhergesagt werden sollen
dataset <- "Produktionsdaten2_80_400000.csv"

# Verzeichnisse des Datensatzes und der Modelle
datapath <- "./Data/"
modelpath_regression <- "./Model/Regression/"
modelpath_classification <- "./Model/Classification/"

# Vorherzusagende Leistungskennzahlen
features_regression = c("Throughput", "CycleTime", "Buffer1Mean", "Buffer2Mean", "Machine1Utilization", "Machine2Utilization", "Machine3Utilization", "Machine4Utilization")
features_classification = c("Throughput", "CycleTime", "Buffer1Mean", "Buffer1Max", "Buffer2Mean", "Buffer2Max", "Machine1Utilization", "Machine2Utilization", "Machine3Utilization", "Machine4Utilization")

########################################################
# Vorbereitung der Stellgrößen
Testdaten_preprocessed <- preprocessing(datapath, dataset)
Testdaten_preprocessed$CarrierSpeedEmpty <- Testdaten_preprocessed$CarrierSpeedEmtpy
Testdaten_preprocessed$CarrierSpeedEmtpy <- NULL

########################################################
# Anwendung der Modelle auf den Datensatz
# Auführen der Regression
result_regression <- svm.regression(features = features_regression, path = modelpath_regression, dataset = Testdaten_preprocessed)
# Abspeichern als R-Datei und csv
saveRDS(result_regression, "./Data/Ergebnis_Regression.rds")
write.csv2(result_regression, "./Data/Ergebnis_Regression.csv")

# Ausführen der Klassifikation
result_classification <- svm.classification(features = features_classification, path = modelpath_classification, dataset = Testdaten_preprocessed)
# Abspeichern als R-Datei und csv
write.csv2(result_classification, "./Data/Ergebnis_Classification.csv")
saveRDS(result_classification, "./Data/Ergebnis_Classification.rds")