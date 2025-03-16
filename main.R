source("lib.R")

# Chargement des données (base de données Mario Wutrich)
data <- read.csv2(file="https://people.math.ethz.ch/%7Ewmario/Lecture/MTPL_data.csv", sep = ";")
str(data)
summary(data)


# Définition du répertoire de travail et réglage de la graine pour la reproductibilité
set.seed(123)


# Conversion des types de données
data$claims <- as.numeric(data$claims)
data$expo <- as.numeric(data$expo)
data$age <- as.numeric(data$age)
data$ac <- as.numeric(data$ac)
data$power <- as.numeric(data$power)
data$gas <- as.factor(data$gas)
data$brand <- as.factor(data$brand)
data$area <- as.factor(data$area)
data$dens <- as.numeric(data$dens)
data$ct <- as.factor(data$ct)
data$truefreq <- as.numeric(data$truefreq)
data$truefreqexpo <- data$truefreq * data$expo   #vrai modèle


# Séparation des données en ensembles : training - testing - validation
inTraining <- createDataPartition(data$claims, p=0.6, list=FALSE)
TrainingSet <- data[inTraining, ] 
Set <- data[-inTraining, ]
inTestSet <- createDataPartition(Set$claims, p=0.5, list=FALSE)
TestSet <- Set[inTestSet, ]
ValidationSet <- Set[-inTestSet, ]


GAMAO <- gam(claims ~ offset(log(expo)) + s(age), family = poisson, data=TrainingSet)

TrainingSet$GAMAO <- predict(GAMAO,newdata = TrainingSet, type = "response")
TestSet$GAMAO <- predict(GAMAO,newdata = TestSet, type = "response")
ValidationSet$GAMAO <- predict(GAMAO,newdata = ValidationSet, type = "response")


GAMAV <- gam(claims ~ offset(log(expo)) + s(age) + s(ac) + power + gas + brand+  area + s(dens) +ct,family = poisson, data=TrainingSet)

TrainingSet$GAMAV <- predict(GAMAV,newdata = TrainingSet, type = "response")
TestSet$GAMAV <- predict(GAMAV,newdata = TestSet, type = "response")
ValidationSet$GAMAV <- predict(GAMAV,newdata = ValidationSet, type = "response")




PlotConcentrationLorenzCurve <- function(data, model, Y, step = 0.01, legend = "Lorenz & Concentration Curves") {
  # Extraire les prédictions et la variable cible
  predictions <- data[[model]]
  n <- nrow(data)
  
  # Vecteurs pour stocker les points des courbes de Lorenz et de concentration
  alpha <- seq(0, 1, by = step)
  y_lorenz <- numeric(length(alpha))
  y_concentration <- numeric(length(alpha))
  
  # Trier les données en fonction des prédictions croissantes
  sorted_data <- data[order(predictions), ]
  sorted_predictions <- sorted_data[[model]]
  sorted_Y <- sorted_data[[Y]]
  
  # Calcul des courbes de Lorenz et de concentration
  total_predictions <- sum(sorted_predictions)
  total_Y <- sum(sorted_Y)
  
  for (i in seq_along(alpha)) {
    threshold <- round(alpha[i] * n) # Définir le seuil d'observation
    
    if (threshold > 0) {
      y_lorenz[i] <- sum(sorted_predictions[1:threshold]) / total_predictions
      y_concentration[i] <- sum(sorted_Y[1:threshold]) / total_Y
    } else {
      y_lorenz[i] <- 0
      y_concentration[i] <- 0
    }
  }
  
  # Tracé des courbes
  plot(alpha, y_lorenz, type = "l", col = "blue", lwd = 2, xlab = "alpha", ylab = legend,
       main = "Lorenz & Concentration Curves")
  lines(alpha, y_concentration, col = "red", lwd = 2)
  lines(alpha, alpha, col = "black", lty = 2)  # Courbe de référence y = x
  
  # Ajout de la légende
  legend("bottomright", legend = c("Lorenz Curve", "Concentration Curve", "Equality Line"),
         col = c("blue", "red", "black"), lty = c(1, 1, 2), lwd = 2)
}


ABC <- function(data, model, Y, step = 0.01) {
  # Extraire les prédictions et la variable cible
  predictions <- data[[model]]
  n <- nrow(data)
  
  # Initialisation des vecteurs pour les courbes de Lorenz et de concentration
  alpha <- seq(0, 1, by = step)
  y_lorenz <- numeric(length(alpha))
  y_concentration <- numeric(length(alpha))
  ABC_values <- numeric(length(alpha))
  
  # Trier les données en fonction des prédictions croissantes
  sorted_data <- data[order(predictions), ]
  sorted_predictions <- sorted_data[[model]]
  sorted_Y <- sorted_data[[Y]]
  
  # Calcul des courbes de Lorenz et de concentration
  total_predictions <- sum(sorted_predictions)
  total_Y <- sum(sorted_Y)
  
  for (i in seq_along(alpha)) {
    threshold <- round(alpha[i] * n)  # Définir le seuil d'observation
    
    if (threshold > 0) {
      y_lorenz[i] <- sum(sorted_predictions[1:threshold]) / total_predictions
      y_concentration[i] <- sum(sorted_Y[1:threshold]) / total_Y
      ABC_values[i] <- step * abs(y_concentration[i] - y_lorenz[i]) # Différence absolue pondérée par step
    } else {
      y_lorenz[i] <- 0
      y_concentration[i] <- 0
      ABC_values[i] <- 0
    }
  }
  
  # Retourne l'aire sous la courbe de la différence entre la courbe de concentration et celle de Lorenz
  return(sum(ABC_values))
}



############################################################################################

ABC(ValidationSet,"truefreqexpo","claims", 0.001)

PlotConcentrationLorenzCurve(ValidationSet,"truefreqexpo","claims", 0.001, "CC/LC on Validation Set")


ABC(ValidationSet,"GAMAO","claims", 0.001)

PlotConcentrationLorenzCurve(ValidationSet,"GAMAO","claims", 0.001, "CC/LC on Validation Set")


ABC(ValidationSet,"GAMAV","claims", 0.001)

PlotConcentrationLorenzCurve(ValidationSet,"GAMAV","claims", 0.001, "CC/LC on Validation Set")

############################################################################################


alpha <- seq(0.03,0.035,0.0005)
lcvplot(x=TestSet$GAMAO, y=TestSet$claims, kern="rect",deg=0,alpha=alpha)

fitgam <- locfit.raw(x=TestSet$GAMAO, y=TestSet$claims, kern="rect",deg=0,alpha=0.031)
TestSet$GAMAOBC <- predict(fitgam, TestSet$GAMAO)
ValidationSet$GAMAOBC <- predict(fitgam, ValidationSet$GAMAO)
PlotConcentrationLorenzCurve(TestSet,"GAMAOBC","claims", 0.001, "CC/LC on Test Set")
PlotConcentrationLorenzCurve(ValidationSet,"GAMAOBC","claims", 0.001, "CC/LC on Validation Set")

alpha <- seq(0.03,0.04,0.0005)
# lcvplot(x=TestSet$GAMAV1, y=TestSet$claims, kern="rect",deg=0,alpha=alpha)

fitgam <- locfit.raw(x=TestSet$GAMAV, y=TestSet$claims, kern="rect",deg=0,alpha=0.0345)
TestSet$GAMAVBC <- predict(fitgam, TestSet$GAMAV)
ValidationSet$GBMAOBC <- predict(fitgam, ValidationSet$GAMAV)
PlotConcentrationLorenzCurve(TestSet,"GAMAVBC","claims", 0.001, "CC/LC on Test Set")
# PlotConcentrationLorenzCurve(ValidationSet,"GAMAVBC","claims", 0.001, "CC/LC on Validation Set")


