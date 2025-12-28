##################################################
# Partie 1 : Préparation de l'environnement
##################################################

# 1.1 Nettoyage de la session
rm(list = ls())          # vider l'environnement
graphics.off()           # fermer les graphiques ouverts

# 1.2 Chargement des packages 
# Installer les packages une seule fois si nécessaire :
# install.packages(c("tidyverse", "sf", "tmap"))

library(tidyverse)       # dplyr, ggplot2, readr, etc.
library(sf)              # données spatiales (cartographie)
library(tmap)            # visualisation cartographique 

##################################################
# Partie 2 : Import et nettoyage des données
##################################################

#setwd("~/Desktop/R-stat/TP2")

# 2.1 Import brut du fichier 
# Adapter le chemin vers ton fichier
raw_pauv <- read.csv2("pauvrete.csv",
                      sep = ";",
                      dec = ".",
                      header = FALSE,
                      stringsAsFactors = FALSE)

# Vérifier le début du fichier
head(raw_pauv, 10)
dim(raw_pauv)

# 2.2 Construction d'un data.frame propre 
# Les 3 premières lignes contiennent l'en-tête complexe, on les ignore
pauv <- raw_pauv[-c(1:3), ]

# Donner des noms de colonnes explicites
colnames(pauv) <- c("Gouvernorat",
                    "Taux_enquete",
                    "Ecart_type_enquete",
                    "IC95_bas",
                    "IC95_haut",
                    "Taux_recensement",
                    "Ecart_type_recensement",
                    "Diff_absolue")

# 2.3 Conversion des types 
# Gouvernorat en facteur, le reste en numérique
pauv <- pauv %>%
  mutate(
    Gouvernorat = factor(Gouvernorat),
    Taux_enquete = as.numeric(Taux_enquete),
    Ecart_type_enquete = as.numeric(Ecart_type_enquete),
    IC95_bas = as.numeric(IC95_bas),
    IC95_haut = as.numeric(IC95_haut),
    Taux_recensement = as.numeric(Taux_recensement),
    Ecart_type_recensement = as.numeric(Ecart_type_recensement),
    Diff_absolue = as.numeric(Diff_absolue)
  )

# 2.4 Vérifications de base 
str(pauv)
summary(pauv)
any(is.na(pauv))          # vérifier s'il y a des NA
pauv %>% head()

##################################################
# Partie 3 : Création de variables dérivées
##################################################

# 3.1 Largeur de l'intervalle de confiance 
pauv <- pauv %>%
  mutate(
    Largeur_IC = IC95_haut - IC95_bas
  )

# 3.2 Différences absolue et relative 
pauv <- pauv %>%
  mutate(
    Diff = Taux_recensement - Taux_enquete,              # recensement - enquête
    Diff_relatif = Diff / Taux_recensement              # différence en proportion
  )

# 3.3 Indicateur : recensement dans l'IC 95 % ? 
pauv <- pauv %>%
  mutate(
    Recensement_dans_IC = Taux_recensement >= IC95_bas &
      Taux_recensement <= IC95_haut
  )

# 3.4 Vérifications rapides 
pauv %>%
  select(Gouvernorat,
         Taux_enquete,
         Taux_recensement,
         Diff_absolue,
         Diff,
         Diff_relatif,
         Largeur_IC,
         Recensement_dans_IC) %>%
  head()
summary(pauv$Largeur_IC)
table(pauv$Recensement_dans_IC)

##################################################
# Partie 4 : Statistiques descriptives
##################################################

# 4.1 Résumé global des principales variables 
summary_select <- pauv %>%
  select(Taux_enquete,
         Taux_recensement,
         Diff_absolue,
         Diff,
         Diff_relatif,
         Largeur_IC)

summary(summary_select)

# 4.2 Mesures détaillées (moyenne, médiane, etc.) 
desc_stats <- summary_select %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(. , na.rm = TRUE),
        median = ~ median(. , na.rm = TRUE),
        sd = ~ sd(. , na.rm = TRUE),
        min = ~ min(. , na.rm = TRUE),
        max = ~ max(. , na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

desc_stats

# 4.3 Statistiques par indicateur sous forme longue 
desc_long <- summary_select %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valeur") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(valeur, na.rm = TRUE),
    median = median(valeur, na.rm = TRUE),
    sd = sd(valeur, na.rm = TRUE),
    q1 = quantile(valeur, 0.25, na.rm = TRUE),
    q3 = quantile(valeur, 0.75, na.rm = TRUE),
    min = min(valeur, na.rm = TRUE),
    max = max(valeur, na.rm = TRUE)
  )

desc_long

##################################################
# Partie 5 : Visualisations classiques
##################################################

# Pour l'ordre des barplots, on trie par taux de pauvreté de l'enquête
pauv <- pauv %>%
  mutate(Gouvernorat = fct_reorder(Gouvernorat, Taux_enquete))

# 5.1 Barplot des taux enquête et recensement 
ggplot(pauv, aes(x = Gouvernorat)) +
  geom_col(aes(y = Taux_enquete, fill = "Enquête"),
           position = "dodge") +
  geom_col(aes(y = Taux_recensement, fill = "Recensement"),
           position = "dodge") +
  coord_flip() +
  labs(title = "Taux de pauvreté par gouvernorat",
       x = "Gouvernorat",
       y = "Taux de pauvreté (%)",
       fill = "Source") +
  theme_minimal()

# 5.2 Boxplots des taux de pauvreté 
pauv_long_taux <- pauv %>%
  select(Gouvernorat, Taux_enquete, Taux_recensement) %>%
  pivot_longer(cols = c(Taux_enquete, Taux_recensement),
               names_to = "Source",
               values_to = "Taux")

ggplot(pauv_long_taux, aes(x = Source, y = Taux, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution des taux de pauvreté",
       x = "Source",
       y = "Taux de pauvreté (%)") +
  theme_minimal()

# 5.3 Histogrammes des taux et des différences 
ggplot(pauv_long_taux, aes(x = Taux, fill = Source)) +
  geom_histogram(alpha = 0.6, bins = 8, position = "identity") +
  labs(title = "Histogrammes des taux de pauvreté",
       x = "Taux de pauvreté (%)",
       y = "Fréquence") +
  theme_minimal()

ggplot(pauv, aes(x = Diff)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 8) +
  labs(title = "Histogramme de la différence Recensement - Enquête",
       x = "Différence de taux (points de %)",
       y = "Fréquence") +
  theme_minimal()

# 5.4 Nuage de points enquête vs recensement 
ggplot(pauv, aes(x = Taux_enquete, y = Taux_recensement)) +
  geom_point(color = "darkred", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(label = Gouvernorat),
            hjust = -0.1, vjust = 0.5, size = 3) +
  labs(title = "Taux de pauvreté : enquête vs recensement",
       x = "Taux de pauvreté (enquête)",
       y = "Taux de pauvreté (recensement)") +
  theme_minimal()

##################################################
# Partie 5 : Visualisations classiques
##################################################

# Pour l'ordre des barplots, on trie par taux de pauvreté de l'enquête
pauv <- pauv %>%
  mutate(Gouvernorat = fct_reorder(Gouvernorat, Taux_enquete))

# 5.1 Barplot des taux enquête et recensement 
ggplot(pauv, aes(x = Gouvernorat)) +
  geom_col(aes(y = Taux_enquete, fill = "Enquête"),
           position = "dodge") +
  geom_col(aes(y = Taux_recensement, fill = "Recensement"),
           position = "dodge") +
  coord_flip() +
  labs(title = "Taux de pauvreté par gouvernorat",
       x = "Gouvernorat",
       y = "Taux de pauvreté (%)",
       fill = "Source") +
  theme_minimal()

# 5.2 Boxplots des taux de pauvreté 
pauv_long_taux <- pauv %>%
  select(Gouvernorat, Taux_enquete, Taux_recensement) %>%
  pivot_longer(cols = c(Taux_enquete, Taux_recensement),
               names_to = "Source",
               values_to = "Taux")

ggplot(pauv_long_taux, aes(x = Source, y = Taux, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution des taux de pauvreté",
       x = "Source",
       y = "Taux de pauvreté (%)") +
  theme_minimal()

# 5.3 Histogrammes des taux et des différences 
ggplot(pauv_long_taux, aes(x = Taux, fill = Source)) +
  geom_histogram(alpha = 0.6, bins = 8, position = "identity") +
  labs(title = "Histogrammes des taux de pauvreté",
       x = "Taux de pauvreté (%)",
       y = "Fréquence") +
  theme_minimal()

ggplot(pauv, aes(x = Diff)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 8) +
  labs(title = "Histogramme de la différence Recensement - Enquête",
       x = "Différence de taux (points de %)",
       y = "Fréquence") +
  theme_minimal()

# 5.4 Nuage de points enquête vs recensement 
ggplot(pauv, aes(x = Taux_enquete, y = Taux_recensement)) +
  geom_point(color = "darkred", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(label = Gouvernorat),
            hjust = -0.1, vjust = 0.5, size = 3) +
  labs(title = "Taux de pauvreté : enquête vs recensement",
       x = "Taux de pauvreté (enquête)",
       y = "Taux de pauvreté (recensement)") +
  theme_minimal()
##################################################
# Partie 5 : Visualisations classiques
##################################################

# Pour l'ordre des barplots, on trie par taux de pauvreté de l'enquête
pauv <- pauv %>%
  mutate(Gouvernorat = fct_reorder(Gouvernorat, Taux_enquete))

# 5.1 Barplot des taux enquête et recensement 
ggplot(pauv, aes(x = Gouvernorat)) +
  geom_col(aes(y = Taux_enquete, fill = "Enquête"),
           position = "dodge") +
  geom_col(aes(y = Taux_recensement, fill = "Recensement"),
           position = "dodge") +
  coord_flip() +
  labs(title = "Taux de pauvreté par gouvernorat",
       x = "Gouvernorat",
       y = "Taux de pauvreté (%)",
       fill = "Source") +
  theme_minimal()

# 5.2 Boxplots des taux de pauvreté 
pauv_long_taux <- pauv %>%
  select(Gouvernorat, Taux_enquete, Taux_recensement) %>%
  pivot_longer(cols = c(Taux_enquete, Taux_recensement),
               names_to = "Source",
               values_to = "Taux")

ggplot(pauv_long_taux, aes(x = Source, y = Taux, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution des taux de pauvreté",
       x = "Source",
       y = "Taux de pauvreté (%)") +
  theme_minimal()

# 5.3 Histogrammes des taux et des différences 
ggplot(pauv_long_taux, aes(x = Taux, fill = Source)) +
  geom_histogram(alpha = 0.6, bins = 8, position = "identity") +
  labs(title = "Histogrammes des taux de pauvreté",
       x = "Taux de pauvreté (%)",
       y = "Fréquence") +
  theme_minimal()

ggplot(pauv, aes(x = Diff)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 8) +
  labs(title = "Histogramme de la différence Recensement - Enquête",
       x = "Différence de taux (points de %)",
       y = "Fréquence") +
  theme_minimal()

# 5.4 Nuage de points enquête vs recensement 
ggplot(pauv, aes(x = Taux_enquete, y = Taux_recensement)) +
  geom_point(color = "darkred", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(label = Gouvernorat),
            hjust = -0.1, vjust = 0.5, size = 3) +
  labs(title = "Taux de pauvreté : enquête vs recensement",
       x = "Taux de pauvreté (enquête)",
       y = "Taux de pauvreté (recensement)") +
  theme_minimal()

##################################################
# Partie 6 : Cartographie des taux de pauvreté
##################################################

# 6.1 Import du fichier géographique des gouvernorats 
# Adapter le chemin vers ton GeoJSON / shapefile
# Exemple avec le GeoJSON de riatelab/tunisie : TN-gouvernorats.geojson
gouv_sf <- st_read("decoupage.geojson.json")  # chemin à adapter

# Vérifier les colonnes de noms de gouvernorats
names(gouv_sf)
head(gouv_sf)

# Supposons qu'on dispose d'un champ 'nom_gouv' en français ou 'gov_name'
# À adapter en fonction du fichier
gouv_sf <- gouv_sf %>%
  rename(Gouvernorat_geo = gov_name_f)   

# 6.2 Harmonisation des noms de gouvernorat 
# 6.2.1. Harmonisation Shapefile (suppression accents et tirets)
# Correction ultime des noms
gouv_sf <- gouv_sf %>%
  mutate(Gouv_clean = toupper(Gouvernorat_geo)) %>%
  mutate(Gouv_clean = case_when(
    Gouv_clean == "SILIANA"       ~ "SELIANA",   
    Gouv_clean == "KEF"           ~ "LE KEF",    
    Gouv_clean == "MANUBAH"       ~ "MANOUBA",
    Gouv_clean == "BÉJA"          ~ "BEJA",
    Gouv_clean == "JENDOUBA"      ~ "JENDOUBA",
    Gouv_clean == "KASSÉRINE"     ~ "KASSERINE",
    Gouv_clean == "SIDI BOU ZID"  ~ "SIDI BOUZID",
    Gouv_clean == "GABÈS"         ~ "GABES",
    Gouv_clean == "MÉDENINE"      ~ "MEDNINE",
    Gouv_clean == "TATAOUINE"     ~ "TATAOUINE",
    TRUE ~ Gouv_clean
  ))

# Vérification immédiate
print(setdiff(pauv$Gouv_clean, gouv_sf$Gouv_clean))

# 6.2.2. Harmonisation CSV (déjà sans accents, mais on vérifie)
pauv <- pauv %>%
  mutate(Gouv_clean = toupper(as.character(Gouvernorat))) %>%
  mutate(Gouv_clean = case_when(
    Gouv_clean == "LE KEF"  ~ "KEF",       # Vérifie si Shapefile a "KEF" ou "LE KEF"
    TRUE ~ Gouv_clean
  ))

# 3. Vérification immédiate
# Si Shapefile a "KEF" et CSV "LE KEF", il faut corriger l'un ou l'autre.
# D'après tes logs précédents, Shapefile semble avoir "KEF" (à confirmer via unique(gouv_sf$Gouv_clean))
# Si Shapefile a "LE KEF", enlève la ligne 20 ci-dessus.

# Relance la jointure SEULEMENT si setdiff est vide
print(setdiff(pauv$Gouv_clean, gouv_sf$Gouv_clean))

# 6.3 Jointure des données de pauvreté aux géométries 
carte_pauv <- gouv_sf %>%
  left_join(pauv, by = "Gouv_clean")

# Forcer manuellement Le Kef dans les deux fichiers
# On remplace tout ce qui contient "KEF" par "LE KEF"
gouv_sf <- gouv_sf %>% 
  mutate(Gouv_clean = case_when(
    grepl("KEF", Gouv_clean) ~ "LE KEF",  # Si ça contient KEF -> LE KEF
    TRUE ~ Gouv_clean
  ))

pauv <- pauv %>% 
  mutate(Gouv_clean = case_when(
    grepl("KEF", Gouv_clean) ~ "LE KEF",  # Idem dans le CSV
    TRUE ~ Gouv_clean
  ))

# Relancer la jointure
carte_pauv <- gouv_sf %>% left_join(pauv, by = "Gouv_clean")

# Vérifier le résultat
carte_pauv %>%
  select(Gouv_clean, Taux_enquete, Taux_recensement) %>%
  head()

# 6.4 Carte choroplèthe : taux de pauvreté enquête 
tm_shape(carte_pauv) +
  tm_polygons("Taux_enquete",
              palette = "Reds",
              style = "quantile",
              title = "Taux de pauvreté (%)\nEnquête") +
  tm_layout(title = "Taux de pauvreté par gouvernorat (Enquête)",
            legend.outside = TRUE)

# 6.5 Carte choroplèthe : taux de pauvreté recensement 
tm_shape(carte_pauv) +
  tm_polygons("Taux_recensement",
              palette = "Blues",
              style = "quantile",
              title = "Taux de pauvreté (%)\nRecensement") +
  tm_layout(title = "Taux de pauvreté par gouvernorat (Recensement)",
            legend.outside = TRUE)

# 6.6 Carte choroplèthe : différence absolue ----
tm_shape(carte_pauv) +
  tm_polygons("Diff",
              palette = "PuOr",
              style = "pretty",
              title = "Différence (Rec - Enq)\npoints de %") +
  tm_layout(title = "Écart entre recensement et enquête\npar gouvernorat",
            legend.outside = TRUE)

##################################################
# Partie 7 : Analyse par les 5 Districts 
##################################################

sf_use_s2(FALSE)  # <<< clé : désactiver s2 pour éviter les erreurs "duplicate vertex"

# 7.1 Table de correspondance officielle (5 Districts)
# Basé sur le décret présidentiel de sept 2023
districts_5 <- data.frame(
  Gouvernorat = c(
    # District 1 (Nord-Ouest)
    "BIZERTE", "BEJA", "JENDOUBA", "LE KEF",
    # District 2 (Nord-Est)
    "TUNIS", "ARIANA", "BEN AROUS", "MANOUBA", "ZAGHOUAN", "NABEUL",
    # District 3 (Centre)
    "SELIANA", "SOUSSE", "KASSERINE", "KAIROUAN", "MONASTIR", "MAHDIA",
    # District 4 (Sud-Ouest & Centre-Est)
    "TOZEUR", "SIDI BOUZID", "SFAX", "GAFSA",
    # District 5 (Sud-Est)
    "TATAOUINE", "GABES", "MEDNINE", "KEBILI"
  ),
  District = c(
    rep("District 1 (Nord-Ouest)", 4),
    rep("District 2 (Nord-Est)", 6),
    rep("District 3 (Centre)", 6),
    rep("District 4 (Sud-Ouest/Centre-Est)", 4),
    rep("District 5 (Sud-Est)", 4)
  )
)

# Harmonisation des noms (MAJUSCULES)
districts_5$Gouv_clean <- toupper(districts_5$Gouvernorat)

# 7.2 Calcul des taux moyens (Enquête ET Recensement)
pauv_dist_data <- pauv %>%
  left_join(districts_5, by = "Gouv_clean") %>%
  group_by(District) %>%
  summarise(
    Moyenne_Enquete = mean(Taux_enquete, na.rm = TRUE),       # Moyenne Enquête
    Moyenne_Recensement = mean(Taux_recensement, na.rm = TRUE), # Moyenne Recensement
    Diff_Moyenne = mean(Diff, na.rm = TRUE)
  )

# 7.3 Refaire la jointure spatiale avec ces nouvelles données
sf_districts <- gouv_sf %>%
  left_join(districts_5, by = "Gouv_clean") %>%
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  group_by(District) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid() %>%
  left_join(pauv_dist_data, by = "District")

# 7.4 Affichage de la carte des Districts selon l'enquête
tm_shape(sf_districts) +
  tm_polygons("Moyenne_Enquete",
              palette = "Reds",
              title = "Taux de Pauvreté Moyen\npar District (2023)",
              textNA = "Pas de données") +
  tm_text("District", size = 0.7, fontface = "bold") +
  tm_layout(title = "Disparités Régionales (5 Districts)",
            legend.position = c("left", "bottom"))

# 7.5 Affichage de la Carte des Districts selon le RECENSEMENT
tm_shape(sf_districts) +
  tm_polygons("Moyenne_Recensement",    
              palette = "Blues",        
              title = "Pauvreté Moyenne\n(Recensement 2023)") +
  tm_text("District", size = 0.6, fontface = "bold") +
  tm_layout(title = "Districts (Données Recensement)")

##################################################
# Partie 8 : Analyse des intervalles de confiance (IC)
##################################################

# 8.1 Création de la variable logique (déjà fait en partie 3, on vérifie)
# TRUE si le taux recensement est compris entre IC_bas et IC_haut
pauv <- pauv %>%
  mutate(
    Recensement_dans_IC = (Taux_recensement >= IC95_bas) & (Taux_recensement <= IC95_haut)
  )

# 8.2 Tableau de comptage
# Combien de gouvernorats ont un recensement cohérent avec l'enquête ?
table_IC <- table(pauv$Recensement_dans_IC)
print(table_IC)

# Calcul du pourcentage de cohérence
prop_coherence <- prop.table(table_IC) * 100
print(round(prop_coherence, 1))

# 8.3 Visualisation Graphique : Barplot coloré selon l'IC
# Ce graphique montre pour chaque gouv le taux recensement et l'IC de l'enquête
ggplot(pauv, aes(x = reorder(Gouvernorat, Taux_recensement), y = Taux_recensement)) +
  # Barre d'erreur représentant l'IC de l'Enquête
  geom_errorbar(aes(ymin = IC95_bas, ymax = IC95_haut), 
                width = 0.4, color = "gray40", size = 0.8) +
  # Point représentant le Taux du Recensement
  geom_point(aes(color = Recensement_dans_IC), size = 3) +
  # Esthétique
  coord_flip() +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "forestgreen"),
                     labels = c("Hors IC", "Dans IC"),
                     name = "Cohérence") +
  labs(title = "Comparaison Recensement vs IC Enquête (95%)",
       subtitle = "Les barres grises représentent l'IC de l'enquête.\nLes points rouges indiquent une divergence significative.",
       x = "Gouvernorat",
       y = "Taux de Pauvreté (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 8.4 Cartographie de la divergence 
# On affiche en rouge les régions où le recensement "contredit" l'enquête
carte_IC <- gouv_sf %>%
  left_join(pauv, by = "Gouv_clean")

tm_shape(carte_IC) +
  tm_polygons("Recensement_dans_IC",
              palette = c("red", "forestgreen"),
              title = "Cohérence Recensement/Enquête",
              labels = c("Divergence (Hors IC)", "Cohérent (Dans IC)")) +
  tm_text("Gouv_clean", size = 0.5) +
  tm_layout(title = "Validité Statistique par Gouvernorat")

##################################################
# Partie 9 : Corrélations et modèle linéaire
##################################################

# 9.1 Calcul de la corrélation
cor_pearson <- cor(pauv$Taux_enquete, pauv$Taux_recensement, 
                   method = "pearson", use = "complete.obs")
cor_spearman <- cor(pauv$Taux_enquete, pauv$Taux_recensement, 
                    method = "spearman", use = "complete.obs")

print(paste("Corrélation de Pearson :", round(cor_pearson, 4)))
print(paste("Corrélation de Spearman :", round(cor_spearman, 4)))

# 9.2 Ajustement du modèle linéaire
# Y (Recensement) ~ X (Enquête)
modele_lin <- lm(Taux_recensement ~ Taux_enquete, data = pauv)

# Résumé du modèle
print(summary(modele_lin))

# 9.3 Extraction des paramètres clés
coef_intercept <- coef(modele_lin)[1]
coef_pente <- coef(modele_lin)[2]
r_squared <- summary(modele_lin)$r.squared
r_squared_adj <- summary(modele_lin)$adj.r.squared
p_value <- summary(modele_lin)$coefficients[2, 4]  # p-value de la pente

# Affichage formaté
cat("\n=== RÉSULTATS DU MODÈLE LINÉAIRE ===\n")
cat("Équation : Taux_Recensement = ", round(coef_intercept, 3), 
    " + ", round(coef_pente, 3), " * Taux_Enquête\n")
cat("R² :", round(r_squared, 4), "\n")
cat("R² ajusté :", round(r_squared_adj, 4), "\n")
cat("p-value de la pente :", round(p_value, 4), "\n")

# 9.4 Tableau de synthèse pour les résultats de régression
tidy_modele <- broom::tidy(modele_lin)
print(tidy_modele)

# 9.5 Graphique : Nuage de points + Droite de régression + Intervalles de confiance
ggplot(pauv, aes(x = Taux_enquete, y = Taux_recensement)) +
  # Nuage de points
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  # Droite de régression
  geom_smooth(method = "lm", color = "red", fill = "red", alpha = 0.2,
              formula = y ~ x, se = TRUE) +
  # Ligne y = x (pour référence : si coefficients étaient parfaits)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "gray50", size = 1) +
  # Étiquettes pour gouvernorats extrêmes
  geom_text(aes(label = Gouvernorat),
            hjust = -0.1, vjust = 0.5, size = 2.5,
            data = pauv %>% slice_max(abs(Diff), n = 5)) +
  # Esthétique
  labs(title = "Relation Enquête vs Recensement",
       subtitle = paste("Corrélation Pearson :", round(cor_pearson, 3),
                        " | R² :", round(r_squared, 3)),
       x = "Taux de Pauvreté Enquête (%)",
       y = "Taux de Pauvreté Recensement (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# 9.6 Analyse des résidus (optionnel mais recommandé)
# Graphique diagnostique
plot(modele_lin, which = 1)  # Résidus vs valeurs ajustées
plot(modele_lin, which = 2)  # Q-Q plot pour la normalité

# 9.7 Interprétation commentée
cat("\n=== INTERPRÉTATION ===\n")
if (cor_pearson > 0.8) {
  cat("✓ Très forte corrélation : les deux sources mesurent essentiellement la même réalité\n")
} else if (cor_pearson > 0.6) {
  cat("✓ Corrélation forte : bonne concordance entre enquête et recensement\n")
} else if (cor_pearson > 0.4) {
  cat("⚠ Corrélation modérée : divergences notables mais structure commune\n")
} else {
  cat("✗ Faible corrélation : sources peu en accord\n")
}

if (abs(coef_pente - 1) < 0.1) {
  cat("✓ Pente proche de 1 : les taux de pauvreté sont à l'échelle similaire\n")
} else if (coef_pente > 1) {
  cat("⚠ Pente > 1 : le recensement amplifie les taux mesurés par l'enquête\n")
} else {
  cat("⚠ Pente < 1 : le recensement diminue les taux de l'enquête\n")
}

if (abs(coef_intercept) < 2) {
  cat("✓ Intercept proche de 0 : bonne superposition des deux sources\n")
} else {
  cat("⚠ Intercept éloigné de 0 : décalage systématique entre enquête et recensement\n")
}

##################################################
# Partie 10 : Test de la différence moyenne (Test t apparié)
##################################################

# 10.1 Test de Student apparié (Paired t-test)
# H0 : La différence moyenne est nulle (mu_diff = 0)
# H1 : La différence moyenne n'est pas nulle (mu_diff != 0)
test_t <- t.test(pauv$Taux_recensement, pauv$Taux_enquete, 
                 paired = TRUE, 
                 conf.level = 0.95)

# Affichage des résultats bruts
print(test_t)

# 10.2 Extraction et formatage des résultats
t_statistic <- test_t$statistic
p_value_t <- test_t$p.value
mean_diff <- test_t$estimate
conf_int_diff <- test_t$conf.int

cat("\n=== RÉSULTATS DU TEST T APPARIÉ ===\n")
cat("Différence moyenne (Recensement - Enquête) :", round(mean_diff, 3), "points\n")
cat("Intervalle de confiance à 95% de la différence : [", 
    round(conf_int_diff[1], 3), ";", round(conf_int_diff[2], 3), "]\n")
cat("Statistique t :", round(t_statistic, 3), "\n")
cat("p-value :", format.pval(p_value_t, digits = 4), "\n")

# 10.3 Interprétation automatique
cat("\n=== INTERPRÉTATION ===\n")
if (p_value_t < 0.05) {
  cat("Résultat SIGNIFICATIF (p < 0.05) :\n")
  cat("Il y a une différence statistique réelle entre les mesures du recensement et de l'enquête.\n")
  if (mean_diff > 0) {
    cat("-> Le Recensement tend à donner des taux PLUS ÉLEVÉS que l'Enquête en moyenne.\n")
  } else {
    cat("-> Le Recensement tend à donner des taux PLUS FAIBLES que l'Enquête en moyenne.\n")
  }
} else {
  cat("Résultat NON SIGNIFICATIF (p >= 0.05) :\n")
  cat("La différence moyenne observée n'est pas statistiquement différente de 0.\n")
  cat("Les écarts observés peuvent être attribués aux fluctuations d'échantillonnage de l'enquête.\n")
  cat("Globalement, les deux méthodes ne présentent pas de biais systématique l'une par rapport à l'autre.\n")
}

# 10.4 Visualisation de la distribution des différences (Rappel densité)
# Pour vérifier visuellement la normalité de la différence (hypothèse du test t)
ggplot(pauv, aes(x = Diff)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_diff, color = "darkblue", size = 1) +
  labs(title = "Distribution des Différences (Recensement - Enquête)",
       subtitle = paste("Moyenne des différences =", round(mean_diff, 2), 
                        "| p-value =", format.pval(p_value_t, digits = 3)),
       x = "Différence (Points de %)",
       y = "Densité") +
  theme_minimal()

# 10.5 Test non-paramétrique (Wilcoxon) - Robustesse
# Si la distribution n'est pas normale (taille d'échantillon faible n=24)
test_w <- wilcox.test(pauv$Taux_recensement, pauv$Taux_enquete, paired = TRUE)
cat("\n=== VÉRIFICATION (TEST DE WILCOXON) ===\n")
cat("p-value (Wilcoxon) :", format.pval(test_w$p.value, digits = 4), "\n")

##################################################
# Partie 11 : Export des résultats 
##################################################

# 11.1 Créer un dossier de sortie si inexistant
output_dir <- "resultats_pauvrete"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

cat("✓ Dossier de sortie créé/confirmé :", output_dir, "\n")

# 11.2 Export des tableaux en CSV

# Tableau 1 : Données brutes complètes
write.csv2(pauv, 
           file.path(output_dir, "pauvrete_complete.csv"),
           row.names = FALSE)
cat("✓ Exporté : pauvrete_complete.csv\n")

# Tableau 2 : Statistiques descriptives synthétiques
write.csv2(desc_stats,
           file.path(output_dir, "statistiques_descriptives.csv"),
           row.names = FALSE)
cat("✓ Exporté : statistiques_descriptives.csv\n")

# Tableau 3 : Données des Districts
write.csv2(pauv_dist_data,
           file.path(output_dir, "pauvrete_par_districts.csv"),
           row.names = FALSE)
cat("✓ Exporté : pauvrete_par_districts.csv\n")

# Tableau 4 : Résultats du modèle linéaire (avec broom)
write.csv2(tidy_modele,
           file.path(output_dir, "resultats_regression.csv"),
           row.names = FALSE)
cat("✓ Exporté : resultats_regression.csv\n")

# 11.3 Export des graphiques en PNG (haute résolution)

# Graphique 1 : Barplot des taux
ggsave(file.path(output_dir, "01_barplot_taux_pauvrete.png"),
       width = 12, height = 6, dpi = 300, units = "cm")
cat("✓ Exporté : 01_barplot_taux_pauvrete.png\n")

# Graphique 2 : Boxplot comparatif
p_boxplot <- ggplot(pauv_long_taux, aes(x = Source, y = Taux, fill = Source)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution des taux de pauvreté",
       x = "Source", y = "Taux de pauvreté (%)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(file.path(output_dir, "02_boxplot_distribution.png"),
       plot = p_boxplot, width = 12, height = 8, dpi = 300, units = "cm")
cat("✓ Exporté : 02_boxplot_distribution.png\n")

# Graphique 3 : Scatterplot Enquête vs Recensement
p_scatter <- ggplot(pauv, aes(x = Taux_enquete, y = Taux_recensement)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", fill = "red", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = Gouvernorat), hjust = -0.1, vjust = 0.5, size = 2.5,
            data = pauv %>% slice_max(abs(Diff), n = 5)) +
  labs(title = "Relation Enquête vs Recensement",
       x = "Taux Enquête (%)", y = "Taux Recensement (%)") +
  theme_minimal()
ggsave(file.path(output_dir, "03_scatter_enquete_vs_recensement.png"),
       plot = p_scatter, width = 12, height = 10, dpi = 300, units = "cm")
cat("✓ Exporté : 03_scatter_enquete_vs_recensement.png\n")

# Graphique 4 : Distribution des différences
p_diff_density <- ggplot(pauv, aes(x = Diff)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(pauv$Diff), color = "darkblue", size = 1) +
  labs(title = "Distribution des différences (Recensement - Enquête)",
       x = "Différence (Points %)", y = "Densité") +
  theme_minimal()
ggsave(file.path(output_dir, "04_distribution_differences.png"),
       plot = p_diff_density, width = 12, height = 8, dpi = 300, units = "cm")
cat("✓ Exporté : 04_distribution_differences.png\n")

# Graphique 5 : Barplot IC (Cohérence)
p_ic <- ggplot(pauv, aes(x = reorder(Gouvernorat, Taux_recensement), y = Taux_recensement)) +
  geom_errorbar(aes(ymin = IC95_bas, ymax = IC95_haut), width = 0.4, color = "gray40", size = 0.8) +
  geom_point(aes(color = Recensement_dans_IC), size = 3) +
  coord_flip() +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "forestgreen"),
                     labels = c("Hors IC", "Dans IC")) +
  labs(title = "Comparaison Recensement vs IC Enquête (95%)",
       x = "Gouvernorat", y = "Taux de pauvreté (%)") +
  theme_minimal()
ggsave(file.path(output_dir, "05_ic_coherence.png"),
       plot = p_ic, width = 14, height = 10, dpi = 300, units = "cm")
cat("✓ Exporté : 05_ic_coherence.png\n")

# 11.4 Export des cartes en PNG

# 1. Carte Enquête
png(filename = file.path(output_dir, "06_carte_enquete_gouvernorat.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(carte_pauv) +
    tm_polygons("Taux_enquete", palette = "Reds", title = "Taux Enquête (%)") +
    tm_text("Gouv_clean", size = 0.6) +
    tm_layout(title = "Pauvreté Enquête", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 06_carte_enquete_gouvernorat.png\n")

# 2. Carte Recensement
png(filename = file.path(output_dir, "07_carte_recensement_gouvernorat.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(carte_pauv) +
    tm_polygons("Taux_recensement", palette = "Blues", title = "Taux Recensement (%)") +
    tm_text("Gouv_clean", size = 0.6) +
    tm_layout(title = "Pauvreté Recensement", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 07_carte_recensement_gouvernorat.png\n")

# 3. Carte Différence
png(filename = file.path(output_dir, "08_carte_difference_gouvernorat.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(carte_pauv) +
    tm_polygons("Diff", palette = "PuOr", title = "Différence (pts %)", midpoint = 0) +
    tm_text("Gouv_clean", size = 0.6) +
    tm_layout(title = "Ecart Recensement - Enquête", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 08_carte_difference_gouvernorat.png\n")

# 4. Carte Cohérence IC
png(filename = file.path(output_dir, "09_carte_coherence_gouvernorat.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(carte_pauv) +
    tm_polygons("Recensement_dans_IC", palette = c("#D73027", "#1A9850"), 
                title = "Cohérence", labels = c("Divergence", "Cohérent")) +
    tm_text("Gouv_clean", size = 0.6) +
    tm_layout(title = "Validité Statistique", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 09_carte_coherence_gouvernorat.png\n")

# 5. Carte Districts (Enquête)
png(filename = file.path(output_dir, "10_carte_enquete_districts.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(sf_districts) +
    tm_polygons("Moyenne_Enquete", palette = "Reds", title = "Taux Moyen (%)") +
    tm_text("District", size = 0.7, fontface = "bold") +
    tm_layout(title = "Pauvreté par District (Enquête)", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 10_carte_enquete_districts.png\n")

# 6. Carte Districts (Recensement)
png(filename = file.path(output_dir, "11_carte_recensement_districts.png"),
    width = 2000, height = 2400, res = 300)
print(
  tm_shape(sf_districts) +
    tm_polygons("Moyenne_Recensement", palette = "Blues", title = "Taux Moyen (%)") +
    tm_text("District", size = 0.7, fontface = "bold") +
    tm_layout(title = "Pauvreté par District (Recensement)", frame = TRUE)
)
dev.off()
cat("✓ Exporté : 11_carte_recensement_districts.png\n")

# 11.5 Résumé texte final pour l'utilisateur
cat("\n")
cat("╔════════════════════════════════════════════════════════╗\n")
cat("║          EXPORT COMPLÉTÉ AVEC SUCCÈS                   ║\n")
cat("╚════════════════════════════════════════════════════════╝\n\n")
cat("Tous les résultats ont été sauvegardés dans :", normalizePath(output_dir), "\n\n")
cat("FICHIERS EXPORTÉS :\n")
cat("  TABLEAUX (CSV) :\n")
cat("    • pauvrete_complete.csv\n")
cat("    • statistiques_descriptives.csv\n")
cat("    • pauvrete_par_districts.csv\n")
cat("    • resultats_regression.csv\n\n")
cat("  GRAPHIQUES (PNG) :\n")
cat("    • 01_barplot_taux_pauvrete.png\n")
cat("    • 02_boxplot_distribution.png\n")
cat("    • 03_scatter_enquete_vs_recensement.png\n")
cat("    • 04_distribution_differences.png\n")
cat("    • 05_ic_coherence.png\n\n")
cat("  CARTES (PNG) :\n")
cat("    • 06_carte_enquete_gouvernorat.png\n")
cat("    • 07_carte_recensement_gouvernorat.png\n")
cat("    • 08_carte_difference_gouvernorat.png\n")
cat("    • 09_carte_coherence_gouvernorat.png\n")
cat("    • 10_carte_enquete_districts.png\n")
cat("    • 11_carte_recensement_districts.png\n\n")
