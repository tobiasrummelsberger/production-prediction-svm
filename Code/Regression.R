# WICHTIG: Library e1071 und die unten aufgeführten Funktionen laden!
library(e1071)

# Funktion zur Erstellung einer zufälliger Teilmenge
randomSample = function(df,n) {
  return (df[sample(nrow(df),n),])
}

# Validierung der SVR
validate.regression = function(model, testset, feature) {
  prediction <- predict(model, testset)
  error_norm = abs(feature-prediction)/feature
  error_norm_max = max(error_norm)
  error_norm_min = min(error_norm)
  error_norm_mean = mean(error_norm)
  print(paste("Normierte absolute Standardabweichung: ", error_norm_mean))
  print(paste("Maximale normierte Abweichung: ", error_norm_max, " Minimale Abweichung: ", error_norm_min))
  return(list("error_norm" = error_norm, "error_norm_mean" = error_norm_mean, "error_max" = error_norm_max, "error_min" = error_norm_min))
}

########################################################

Produktionsdaten_vorbearbeitet_Regression <- readRDS("./Data/Produktionsdaten_vorbearbeitet_Regression.rds")

trainset <- randomSample(Produktionsdaten_vorbearbeitet_Regression, 10000)
testset <- randomSample(Produktionsdaten_vorbearbeitet_Regression, 400000)

# Listen für Tuning
cost_list <- c(10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5, 10^7) 
gamma_list <- c(10^-7, 10^-5, 10^-3, 10^-1, 10^1, 10^3, 10^5)

########################################################
# Erstellen der SVR - sortiert nach Leistungskennzahl

# SVR für Throughput
#SVR_Throughput_tune <- tune(svm, Throughput ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed, data = trainset, type = "eps-regression", kernel = "radial", ranges = list(gamma = gamma_list, cost = cost_list), scale = T)
#print(SVR_Throughput_tune)
#plot(SVR_Throughput_tune)
SVR_Throughput <- svm(Throughput ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Throughput_validation <- validate.regression(SVR_Throughput, testset, testset$Throughput)
hist(SVR_Throughput_validation$error_norm, breaks = seq(0, max(SVR_Throughput_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Throughput", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Throughput, "./Model/Regression/SVR_Throughput.rds")

# SVR für Cycletime
SVR_CycleTime <- svm(CycleTime ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_CycleTime_validation <- validate.regression(SVR_CycleTime, testset, testset$CycleTime)
hist(SVR_CycleTime_validation$error_norm, breaks = seq(0, max(SVR_CycleTime_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - CycleTime", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_CycleTime, "./Model/Regression/SVR_CycleTime.rds")

# SVR für Buffer1Mean
SVR_Buffer1Mean <- svm(Buffer1Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Buffer1Mean_validation <- validate.regression(SVR_Buffer1Mean, testset, testset$Buffer1Mean)
hist(SVR_Buffer1Mean_validation$error_norm, breaks = seq(0, max(SVR_Buffer1Mean_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Buffer1Mean", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Buffer1Mean, "./Model/Regression/SVR_Buffer1Mean.rds")

# SVR für Buffer2Mean
SVR_Buffer2Mean <- svm(Buffer2Mean ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Buffer2Mean_validation <- validate.regression(SVR_Buffer2Mean, testset, testset$Buffer2Mean)
hist(SVR_Buffer2Mean_validation$error_norm, breaks = seq(0, max(SVR_Buffer2Mean_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Buffer2Mean", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Buffer2Mean, "./Model/Regression/SVR_Buffer2Mean.rds")

# SVR für Machine1Utilization
SVR_Machine1Utilization <- svm(Machine1Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Machine1Utilization_validation <- validate.regression(SVR_Machine1Utilization, testset, testset$Machine1Utilization)
hist(SVR_Machine1Utilization_validation$error_norm, breaks = seq(0, max(SVR_Machine1Utilization_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Machine1Utilization", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Machine1Utilization, "./Model/Regression/SVR_Machine1Utilization.rds")

# SVR für Machine2Utilization
SVR_Machine2Utilization <- svm(Machine2Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Machine2Utilization_validation <- validate.regression(SVR_Machine2Utilization, testset, testset$Machine2Utilization)
hist(SVR_Machine2Utilization_validation$error_norm, breaks = seq(0, max(SVR_Machine2Utilization_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Machine2Utilization", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Machine2Utilization, "./Model/Regression/SVR_Machine2Utilization.rds")

# SVR für Machine3Utilization
SVR_Machine3Utilization <- svm(Machine3Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Machine3Utilization_validation <- validate.regression(SVR_Machine3Utilization, testset, testset$Machine3Utilization)
hist(SVR_Machine3Utilization_validation$error_norm, breaks = seq(0, max(SVR_Machine3Utilization_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Machine3Utilization", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Machine3Utilization, "./Model/Regression/SVR_Machine3Utilization.rds")

# SVR für Machine4Utilization
SVR_Machine4Utilization <- svm(Machine4Utilization ~ LoadingVolume + Buffer1 + Buffer2 + NumberOfCarriers + CarrierSpeedLoaded + CarrierSpeedEmpty + Product1 + Product2 + Product3 + Product4 + Product5 + Product6 + Product7 + Product8 + Product9 + Product10 + Product11 + Product12 + Product13 + Product14 + Product15 + Product16 + Product17 + Product18 + Product19 + Product20 + Product21 + Product22 + Product23 + Product24 + Product25 + Product26 + Product27 + Machine1Speed + Machine2Speed + Machine3Speed + Machine4Speed , data = trainset)
SVR_Machine4Utilization_validation <- validate.regression(SVR_Machine4Utilization, testset, testset$Machine4Utilization)
hist(SVR_Machine4Utilization_validation$error_norm, breaks = seq(0, max(SVR_Machine4Utilization_validation$error_norm)+0.05, 0.05), main = "Histogramm der normierten Abweichung - Machine4Utilization", xlab = "normierte Abweichung", ylab = "Häufigkeit")
saveRDS(SVR_Machine4Utilization, "./Model/Regression/SVR_Machine4Utilization.rds")
