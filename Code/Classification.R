# WICHTIG: Library dplyr, Library e1071 und die unten aufgeführten Funktionen laden!
library(dplyr)
library(e1071)

# Funktion zur Erstellung einer zufälliger Teilmenge
randomSample = function(df,n) {
  return (df[sample(nrow(df),n),])
}

# Validierung der SVC
validate.classification = function(model, testset, feature) {
  prediction <- predict(model, testset)
  cm = as.matrix(table(Actual = feature, Predicted = prediction))
  accuracy = (sum(diag(cm))/sum(cm))*100
  print(cm)
  print(paste("Accuracy: ", accuracy, "%"))
}

########################################################

# Einlesen der Daten
Produktionsdaten_vorbearbeitet_Classification <- readRDS("./Data/Produktionsdaten_vorbearbeitet_Classification.rds")

# Erstellen eines Trainings- und Testdatensatzes
trainset <- randomSample(Produktionsdaten_vorbearbeitet_Classification, 2500)
testset <- randomSample(Produktionsdaten_vorbearbeitet_Classification, 50000)

# Listen für Tuning der SVC
cost_list <- c(10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5, 10^7) 
gamma_list <- c(10^-7, 10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5)

########################################################
# Erstellen der SVC - sortiert nach Leistungskennzahl

# SVC für Buffer1Max
#SVC_Buffer1Max_tune <- tune(svm, Buffer1Max ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = gamma_list, cost = cost_list), scale = T)
#print(SVC_Buffer1Max_tune)
#plot(SVC_Buffer1Max_tune)
SVC_Buffer1Max <- svm(Buffer1Max ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 100, gamma = 0.01, scale = T)
validate.classification(SVC_Buffer1Max, testset, testset$Buffer1Max)
saveRDS(SVC_Buffer1Max, "./Model/Classification/SVC_Buffer1Max.rds")

# SVC für Buffer2Max
#SVC_Buffer2Max_tune <- tune(svm, Buffer2Max ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = gamma_list, cost = cost_list), scale = T)
#print(SVC_Buffer2Max_tune)
#plot(SVC_Buffer2Max_tune)
SVC_Buffer2Max <- svm(Buffer2Max ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1000, gamma = 0.01, scale = T)
validate.classification(SVC_Buffer2Max, testset, testset$Buffer2Max)
saveRDS(SVC_Buffer2Max, "./Model/Classification/SVC_Buffer2Max.rds")

# SVC für Buffer1Mean
#SVC_Buffer1Mean_tune <- tune(svm, Buffer1Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = gamma_list, cost = cost_list), scale = T)
#print(SVC_Buffer1Mean_tune)
#plot(SVC_Buffer1Mean_tune)
SVC_Buffer1Mean <- svm(Buffer1Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1000, gamma = 0.001, scale = T)
validate.classification(SVC_Buffer1Mean, testset, testset$Buffer1Mean)
saveRDS(SVC_Buffer1Mean, "./Model/Classification/SVC_Buffer1Mean.rds")

# SVC für Buffer2Mean
#SVC_Buffer2Mean_tune <- tune(svm, Buffer2Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = gamma_list, cost = cost_list), scale = T)
#print(SVC_Buffer2Mean_tune)
SVC_Buffer2Mean <- svm(Buffer2Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1e+05, gamma = 0.001, scale = T)
validate.classification(SVC_Buffer2Mean, testset, testset$Buffer2Mean)
saveRDS(SVC_Buffer2Mean, "./Model/Classification/SVC_Buffer2Mean.rds")

# SVC für Throughput
#SVC_Throughput_tune <- tune(svm, Throughput ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_Throughput_tune)
#print(SVC_Throughput_tune)
SVC_Throughput <- svm(Throughput ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 100, gamma = 0.01, scale = T)
validate.classification(SVC_Throughput, testset, testset$Throughput)
saveRDS(SVC_Throughput, "./Model/Classification/SVC_Throughput.rds")

# SVC für CycleTime
#SVC_CycleTime_tune <- tune(svm, CycleTime ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_CycleTime_tune)
#print(SVC_CycleTime_tune)
#saveRDS(SVC_CycleTime_tune, "./CycleTime_tune.rds")
SVC_CycleTime <- svm(CycleTime ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 10000, gamma = 0.001, scale = T)
validate.classification(SVC_CycleTime, testset, testset$CycleTime)
saveRDS(SVC_CycleTime, "./Model/Classification/SVC_CycleTime.rds")

# SVC für Machine1Utilization
#SVC_Machine1Utilization_tune <- tune(svm, Machine1Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_Machine1Utilization_tune)
#print(SVC_Machine1Utilization_tune)
SVC_Machine1Utilization <- svm(Machine1Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 2, gamma = 0.1, scale = T)
validate.classification(SVC_Machine1Utilization, testset, testset$Machine1Utilization)
saveRDS(SVC_Machine1Utilization, "./Model/Classification/SVC_Machine1Utilization.rds")

# SVC für Machine2Utilization
#SVC_Machine2Utilization_tune <- tune(svm, Machine2Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_Machine2Utilization_tune)
#print(SVC_Machine2Utilization_tune)
SVC_Machine2Utilization <- svm(Machine2Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1000, gamma = 0.01, scale = T)
validate.classification(SVC_Machine2Utilization, testset, testset$Machine2Utilization)
saveRDS(SVC_Machine2Utilization, "./Model/Classification/SVC_Machine2Utilization.rds")

# SVM für Machine3Utilization
#SVC_Machine3Utilization_tune <- tune(svm, Machine3Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_Machine3Utilization_tune)
#print(SVC_Machine3Utilization_tune)
SVC_Machine3Utilization <- svm(Machine3Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1000, gamma = 0.01, scale = T)
validate.classification(SVC_Machine3Utilization, testset, testset$Machine3Utilization)
saveRDS(SVC_Machine3Utilization, "./Model/Classification/SVC_Machine3Utilization.rds")

# SVM für Machine4Utilization
#SVC_Machine4Utilization_tune <- tune(svm, Machine1Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", ranges = list(gamma = c(0.0001, 0.001, 0.01, 0.1, 0.3, 0.5, 1), cost = c(1, 5, 10, 100, 500, 1000, 3000, 10000)), scale = T)
#plot(SVC_Machine4Utilization_tune)
#print(SVC_Machine4Utilization_tune)
SVC_Machine4Utilization <- svm(Machine4Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "C-classification", kernel = "radial", cost = 1000, gamma = 0.01, scale = T)
validate.classification(SVC_Machine4Utilization, testset, testset$Machine4Utilization)
saveRDS(SVC_Machine4Utilization, "./Model/Classification/SVC_Machine4Utilization.rds")