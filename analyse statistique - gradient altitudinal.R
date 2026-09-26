# Charger les bibliothèques nécessaires
library(readxl)
library(dplyr)
library(ggplot2)

# Lire les données depuis la feuille "data"
# Remplacez "chemin/vers/votre/fichier/taxon - caracteriser - v3.xlsx" par le chemin réel
donnees <- read_excel(file.choose(), sheet = "data")

# Vérifier la structure et s'assurer que l'altitude est numérique
str(donnees)
donnees$altitude <- as.numeric(donnees$altitude)

# Filtrer les données pour ne garder que les amphibiens et reptiles
donnees_herp <- donnees %>%
  filter(type %in% c("amphibien", "reptile"))

# Test de Mann-Whitney (Wilcoxon rank sum test)
test_mann_whitney <- wilcox.test(altitude ~ type, data = donnees_herp)

# Afficher le résultat
print(test_mann_whitney)

# Calculer les médianes pour vérifier les valeurs du tableau
medianes <- donnees_herp %>%
  group_by(type) %>%
  summarise(median_altitude = median(altitude, na.rm = TRUE))

print(medianes)