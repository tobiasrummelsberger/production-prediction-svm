# WICHTIG: Library e1071, Library grDevices und die unten aufgeführten Funktionen laden!

library(e1071)
library(grDevices)
library(binr)

evaluation.regression = function(feature){
  dataset = Produktionsdaten_vorbearbeitet_Regression
  feature_name = colnames(dataset)[feature]
  model <- get(paste("SVR_", feature_name, sep = ""))
  kernel <- model$kernel
  cost <- model$cost
  gamma <- model$gamma
  prediction <- predict(model, dataset)
  actual <- dataset[[feature]]
  error_norm = abs(actual-prediction)/actual
  error_norm_max = max(error_norm)
  error_norm_min = min(error_norm)
  error_norm_mean = mean(error_norm)
  
  # Histogramm
  png(filename= paste("./Graphics/Histogram/", feature_name, ".png", sep = ""), width = 647, height = 496, units = "px")
  plot(hist(error_norm, breaks = seq(0, max(error_norm)+0.05, 0.05)), main = "Histogramm der normierten Abweichung", sub = feature_name, xlab = "normierte Abweichung", ylab = "Häufigkeit")
  dev.off()
  return(list("Leistungskennzahl" = feature_name, "Mittlere normierte Abweichung" = error_norm_mean, "Maximale normierte Abweichung" = error_norm_max, "Minimale normierte Abweichung" = error_norm_min, "Kernel" = kernel, "Kostenbudget" = cost, "Gamma" = gamma))
}

evaluation.classification = function(feature){
  dataset = Produktionsdaten_vorbearbeitet_Classification
  feature_name = colnames(dataset)[feature]
  model <- get(paste("SVC_", feature_name, sep = ""))
  kernel <- model$kernel
  cost <- model$cost
  gamma <- model$gamma
  prediction <- predict(model, dataset)
  actual <- dataset[feature]
  cm = as.matrix(table(Actual = actual[[1]], Predicted = prediction))
  accuracy = (sum(diag(cm))/sum(cm))*100
  return(list("Leistungskennzahl" = as.character(feature_name), "Genauigkeit" = accuracy, "Kernel" = kernel, "Kostenbudget" = cost, "Gamma" = gamma))
}

########################################################
# Evaluation der Regressionsmodelle
# Einlesen der Daten
Produktionsdaten_vorbearbeitet_Regression <- readRDS("./Data/Produktionsdaten_vorbearbeitet_Regression.rds")

# Einlesen der SVR-Modelle
SVR_Throughput <- readRDS("./Model/Regression/SVR_Throughput.rds")
SVR_CycleTime <- readRDS("./Model/Regression/SVR_CycleTime.rds")
SVR_Buffer1Mean <- readRDS("./Model/Regression/SVR_Buffer1Mean.rds")
SVR_Buffer2Mean <- readRDS("./Model/Regression/SVR_Buffer2Mean.rds")
SVR_Machine1Utilization <- readRDS("./Model/Regression/SVR_Machine1Utilization.rds")
SVR_Machine2Utilization <- readRDS("./Model/Regression/SVR_Machine2Utilization.rds")
SVR_Machine3Utilization <- readRDS("./Model/Regression/SVR_Machine3Utilization.rds")
SVR_Machine4Utilization <- readRDS("./Model/Regression/SVR_Machine4Utilization.rds")

# Anwenden des Evaluationsalogrithmus auf alle Modelle, die Ergebnisse werden als Spalten angehangen
evaluation_regression <- data.frame(cbind("Leistungskennzahl" = 0, "Mittlere normierte Abweichung" = 0, "Maximale normierte Abweichung" = 0, "Minimale normierte Abweichung" = 0, "Kernel" = 0, "Kostenbudget" = 0, "Gamma" = 0))
for(i in 38:45){
  evaluation_regression <- data.frame(rbind(evaluation_regression, evaluation.regression(i)))
}

# Speichern der Evaluation als rds-Datei
saveRDS(evaluation_regression, "./Model/Evaluation/evaluation_regression.rds")

##############################################################
# Evaluation der Klassifikationsmodelle
# Einlesen der Daten
Produktionsdaten_vorbearbeitet_Classification <- readRDS("./Data/Produktionsdaten_vorbearbeitet_Classification.rds")

# Einlesen der SVC-Modelle
SVC_Throughput <- readRDS("./Model/Classification/SVC_Throughput.rds")
SVC_CycleTime <- readRDS("./Model/Classification/SVC_CycleTime.rds")
SVC_Buffer1Mean <- readRDS("./Model/Classification/SVC_Buffer1Mean.rds")
SVC_Buffer2Mean <- readRDS("./Model/Classification/SVC_Buffer2Mean.rds")
SVC_Buffer1Max <- readRDS("./Model/Classification/SVC_Buffer1Max.rds")
SVC_Buffer2Max <- readRDS("./Model/Classification/SVC_Buffer2Max.rds")
SVC_Machine1Utilization <- readRDS("./Model/Classification/SVC_Machine1Utilization.rds")
SVC_Machine2Utilization <- readRDS("./Model/Classification/SVC_Machine2Utilization.rds")
SVC_Machine3Utilization <- readRDS("./Model/Classification/SVC_Machine3Utilization.rds")
SVC_Machine4Utilization <- readRDS("./Model/Classification/SVC_Machine4Utilization.rds")

# Anwenden des Evaluationsalogrithmus auf alle Modelle, die Ergebnisse werden als Spalten angehangen
evaluation_classification <- data.frame(cbind("Leistungskennzahl" = 0, "Genauigkeit" = 0, "Kernel" = 0, "Kostenbudget" = 0, "Gamma" = 0))
for(i in 38:47){
  evaluation_classification <- data.frame(rbind(evaluation_classification, evaluation.classification(i)))
}

# Speichern der Evaluation als rds-Datei
saveRDS(evaluation_classification, "./Model/Evaluation/evaluation_classifcation.rds")

##############################################################
# Erstellen eines Balkendiagrammes zur Visualisierung von normierter mittlerer absoluter Abweichung und normierter maximaler absoluter Abweichung

evaluation_regression <- readRDS("./Model/Evaluation/evaluation_regression.rds")
evaluation_classification <- readRDS("./Model/Evaluation/evaluation_classifcation.rds")

# Erstellen eines Balkendiagramms
bartable <- data.frame(NULL)
feature <- rapply(evaluation_regression$feature, c)
error_norm_mean <- rapply((evaluation_regression$error_norm_mean), c)
error_norm_max <- rapply((evaluation_regression$error_max), c)

bartable <- data.frame("feature" = feature, "error_mean" = error_norm_mean, "error_max" = error_norm_max)

funProp <- function(testCol) {
  bartable[, testCol]/max(bartable[, testCol])
}

bartable$var.a.prop <- funProp(2)
bartable$var.b.prop <- funProp(3)
par(cex = 1.5, cex.main = 0.8) 
barplot(t(as.matrix(bartable[, c("var.a.prop", "var.b.prop")])), beside = TRUE,
        yaxt = "n", names.arg = test$feature, cex.names = 0.5, las = 2, col = c("grey", "grey20"), main = "Evaluation der SVR")
par(cex = 0.6)
axis(2, line = 0, at = seq(0, max(bartable$var.a.prop), length.out = 11),
     labels = round(seq(0, max(bartable$error_mean), length.out = 11), digits = 2), las =2)

axis(4, line = 0, at = seq(0, max(bartable$var.b.prop), length.out = 11),
     labels = round(seq(0, 4, length.out = 11), digits = 2), las = 2)

legend("topright", 
       legend = c("mittlere Abweichung", "maximale Abweichung"), 
       fill = c("grey", "grey20"), bty = "n")
mtext("mittlere normierte Abweichung",side=2,line=1.7, cex = 0.7)
mtext("maximale normierte Abweichung",side=4,line=1.2, cex = 0.7)

##############################################################
validate.regression.cm = function(actual, prediction) {
  cm = as.matrix(table(Actual = actual, Predicted = prediction))
  accuracy = (sum(diag(cm))/sum(cm))*100
  print(cm)
  print(paste("Accuracy: ", accuracy, "%"))
}

# Vergleichen von Regression und Klassifikation
Leistungskennzahlen_diskretisiert <- readRDS("./Data/Leistungskennzahlen_diskretisiert.rds")
Ergebnis_Regression <- readRDS("./Data/Ergebnis_Regression.rds")
Leistungskennzahlen <- readRDS("./Data/Leistungskennzahlen.rds")
# bei folgenden Leistungskennzahlen wurde keine Regression durchgeführt
Leistungskennzahlen$Buffer1Max <- NULL
Leistungskennzahlen$Buffer2Max <- NULL

for(i in 49:56){
  # Breakpoints laden
  matrix_column_bins <- bins(Leistungskennzahlen[[(i-48)]], target.bins = 5, exact.groups = 5, minpts = 5000)
  matrix_column_bins_interval <- bins.getvals(matrix_column_bins)
  x <- cut(Ergebnis_Regression[[i]], breaks = matrix_column_bins_interval, labels = FALSE, right = FALSE)
  validate.regression.cm(Leistungskennzahlen_diskretisiert[[i-47]], x)
}