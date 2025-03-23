### Charger les packages nécessaires
library(ggplot2)
library(lmtest)
library(car)
library(forecast)

### Charger les données (Assurez-vous d'avoir le fichier "wa-wheat.dat" dans le bon répertoire)
data <- read.table("wa-wheat.dat", header = TRUE)

# Extraction des données pour Northampton (supposons que la colonne YIELD est celle du rendement)
northampton <- data[data$Shire == "Northampton", ]

# Définir TIME comme une séquence de 1 à 48
northampton$TIME <- 1:nrow(northampton)

# Transformation logarithmique si nécessaire
northampton$ln_YIELD <- log(northampton$YIELD)

definir_modeles <- function(df) {
  mod1 <- lm(YIELD ~ TIME, data = df)
  mod2 <- lm(YIELD ~ log(TIME), data = df)
  mod3 <- lm(YIELD ~ I(TIME^2), data = df)
  mod4 <- lm(log(YIELD) ~ TIME, data = df)
  return(list(mod1, mod2, mod3, mod4))
}

modeles <- definir_modeles(northampton)
noms_modeles <- c("LINEAIRE", "LOGARITHMIQUE", "QUADRATIQUE", "LOG-LINEAIRE")

### Évaluation des modèles
evaluer_modeles <- function(modeles, noms) {
  for (i in 1:length(modeles)) {
    cat("\nModèle :", noms[i], "\n")
    print(summary(modeles[[i]]))
    cat("\nTest de normalité des résidus (Shapiro-Wilk) :", shapiro.test(resid(modeles[[i]]))$p.value, "\n")
  }
}

evaluer_modeles(modeles, noms_modeles)

### Analyse graphique des résidus
plot_residus <- function(modele, titre) {
  par(mfrow = c(2, 2))
  plot(modele, main = titre)
  par(mfrow = c(1, 1))
}

lapply(1:4, function(i) plot_residus(modeles[[i]], noms_modeles[i]))

### Diagnostic des observations influentes
modele_choisi <- modeles[[4]]  # Supposons que le modèle log-linéaire soit le meilleur
influence_measures <- influence.measures(modele_choisi)
print(influence_measures)

# Détection des valeurs atypiques
northampton$studentized_residuals <- rstudent(modele_choisi)
northampton$leverage <- hatvalues(modele_choisi)
northampton$dfbetas <- dfbeta(modele_choisi)
northampton$dffits <- dffits(modele_choisi)

print(northampton[northampton$studentized_residuals > 2, ])

### Prédiction pour 1997
modele_train <- lm(log(YIELD) ~ TIME, data = northampton[1:47, ])
prevision_1997 <- predict(modele_train, newdata = data.frame(TIME = 48), interval = "prediction", level = 0.95)
prevision_1997 <- exp(prevision_1997)

print(prevision_1997)

# Comparer avec la valeur réelle
print(paste("Valeur réelle en 1997 :", northampton$YIELD[48]))
