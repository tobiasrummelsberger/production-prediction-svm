# WICHTIG: Library e1071 laden!
library(e1071)

########################################################
# Laden des Beispieldatensatzes
example.df <- read.csv2(paste("./Data/", "Beispieldaten.csv", sep = ""))
example.df$Y <- factor(example.df$Y)
saveRDS(example.df, "./Data/Beispieldatensatz.rds")


example.df <- readRDS("./Data/Beispieldatensatz.rds")

# Visualisieren des Beispieldatensatzes
plot(x = example.df$X1, y = example.df$X2, type = "p", pch = 4, xlab = "X1", ylab = "X2", col = (as.numeric(example.df$Y)))

# Erstellen einer linearen Hard Margin SVM
example.svm <- svm(Y ~ X1+X2, data = example.df, kernel = "linear", cost = 10000, gamma = 0.5)
plot(example.svm, data = example.df, formula = X2~X1, grid = 250, svSymbol = 2, dataSymbol = 4, fill = T)

# Erstellen einer linearen Soft Margin SVM
example.svm <- svm(Y ~ X1+X2, data = example.df, kernel = "radial", cost = 0.3, gamma = 0.01)
plot(example.svm, data = example.df, formula = X2~X1, grid = 250, svSymbol = 2, dataSymbol = 4, fill = T, sub = "SVM linear Soft Margin")

# Tuning der SVM
example.svm.tune <- tune(svm, Y~X1+X2, data = example.df, kernel = "radial", ranges = list(cost = c(1:1000), gamma = c(0,1,0.1)))
print(example.svm.tune)
plot(example.svm.tune)