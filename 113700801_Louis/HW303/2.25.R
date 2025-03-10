# Installation des packages nécessaires
# install.packages("devtools")  # Installer devtools
# library(devtools)             # Charger devtools
# devtools::install_github("ccolonescu/POE5Rdata") # Télécharger les données
# install.packages("dplyr")
# install.packages("ggplot2")

# Nettoyer l'environnement (optionnel)
rm(list=ls())

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(POE5Rdata)

# Charger le jeu de données cex5_small
data("cex5_small")

# Calculer la fréquence relative (en pourcentage)
hist_data <- hist(cex5_small$foodaway, breaks = 20, plot = FALSE)  # Ne pas afficher l'histogramme, juste calculer les données
relative_freq <- hist_data$counts / sum(hist_data$counts) * 100  # Convertir en pourcentage

# Tracer l'histogramme
barplot(
  relative_freq, 
  names.arg = hist_data$mids,  # Utiliser les milieux des classes comme étiquettes
  main = "Figure xr2-25a Histogramme des dépenses alimentaires hors domicile",  # Titre
  xlab = "Dépenses mensuelles pour la nourriture hors domicile par personne, $",  # Axe X
  ylab = "Pourcentage",  # Axe Y
  ylim = c(0, 60)    # Ajuster l'échelle de l'axe Y
)

# Calcul des statistiques descriptives
summary(cex5_small$foodaway)

#----------------------------------------------------------(a)

# Calculer la moyenne et la médiane des dépenses alimentaires par niveau d'éducation
summary_foodaway <- cex5_small %>%
  mutate(categorie_education = case_when(
    advanced == 1 ~ "Études avancées",
    college == 1 ~ "Diplôme universitaire",
    advanced == 0 & college == 0 ~ "Aucun diplôme"
  )) %>%
  group_by(categorie_education) %>%
  summarise(
    N = n(),  # Nombre d'observations
    MOYENNE = mean(foodaway, na.rm = TRUE),  # Moyenne des dépenses alimentaires
    MEDIANE = median(foodaway, na.rm = TRUE)  # Médiane des dépenses alimentaires
  )

# Afficher les résultats
print(summary_foodaway)

#------------------------------------------------------------------(b)

# Créer la variable ln_foodaway (éviter le log de 0 ou valeurs négatives)
cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, 
                                 log(cex5_small$foodaway), 
                                 NA)

# Calcul du nombre d'observations valides
n_foodaway <- sum(!is.na(cex5_small$foodaway))
n_ln_foodaway <- sum(!is.na(cex5_small$ln_foodaway))
n_zeros <- sum(cex5_small$foodaway == 0, na.rm = TRUE)

# Affichage des résultats
cat("Nombre d'observations de foodaway:", n_foodaway, "\n")
cat("Nombre d'observations de ln_foodaway:", n_ln_foodaway, "\n")
cat("Nombre d'observations foodaway = 0:", n_zeros, "\n")
cat("Nombre d'observations supprimées à cause du log:", n_foodaway - n_ln_foodaway, "\n\n")

#-------------------------------------------------------------------(c)

# Estimation d'un modèle de régression linéaire
modele <- lm(ln_foodaway ~ income, data = cex5_small)
summary(modele)

#-------------------------------------------------------------------(d)

# Tracer le nuage de points de ln(FOODAWAY) vs. ln(INCOME) avec la ligne de tendance
ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Figure xr2.25(d) Nuage de points de ln(FOODAWAY) vs. ln(INCOME)",
       x = "ln(INCOME)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

# Calcul des résidus
residus <- modele$residuals

# Nettoyer les valeurs manquantes
donnees_nettoyees <- cex5_small %>%
  filter(!is.na(ln_foodaway), !is.na(income))

# Tracer le graphe des résidus
ggplot(donnees_nettoyees, aes(x = income, y = residus)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Résidus vs. Revenu",
       x = "Revenu",
       y = "Résidus") +
  theme_minimal()

#----------------------------------------------------------------(e)

