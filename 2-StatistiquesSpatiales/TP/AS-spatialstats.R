

#####
# TP : Statistiques spatiales


#########
# 1 ) Analyse d'un semis de points : statistiques de synthèse
# Aires urbaines françaises (50 plus grandes) - evolution du centre de gravité - population (1831 - 2000)

#' Données: coordonnées et nom des villes (data/FRUrbanAreas/coords.csv), 
#' populations dans le temps (data/FRUrbanAreas/pop50.csv),
#' dates (data/FRUrbanAreas/dates.csv)
#'  * pas de header
#'  * utiliser de préférence dplyr et readr (tidyverse) 

# 1.1) charger les données
#  Question : système de coordonnées?


# 1.2) calculer point moyen, point moyen pondéré, distance-type pour chaque date


# 1.3) cartographier les villes et l'évolution du point moyen
#  Question : quel package pour cartographier "simplement" ?
#  Données supplémentaires: limites des régions: data/regions/regions_2015_metropole_region.shp
#   ! systèmes de coordonnées à gérer


# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:
# library(ICSNP)
# spatial.median(.)



#######
#  2 ) Analyse d'un semis de points: 


# 2.1) Charger les données d'OpenStreetMap:
#  * données gpkg
#  * fichiers disponibles: data/osmdata/
#   c("Architecte.gpkg","Courtier_immobilier.gpkg","Hôtels.gpkg",
#    "Auberge.gpkg","École_élémentaire.gpkg","Lycée.gpkg",
#    "Cabinet_avocats.gpkg","École_maternelle.gpkg","Motel.gpkg",
#    "Chambredhôte.gpkg","Ecole_primaire.gpkg","Notaires.gpkg",
#    "Collège.gpkg","Enseignement_Supérieur.gpkg","Salon_de_coiffure.gpkg",
#     "Comptable.gpkg","Géomètre.gpkg")
#   -> a regrouper par type d'activité: éducation, prof. libérales, logements, coiffeurs
#   (choisir une activité ou ne charger qu'un seul fichier pour l'instant)



# 2.2) Calculer l'indice de plus proche voisin dans le cas d'un faible nombre de points (universités par exemple)




# 2.3) Cartographier la densité des points



# 2.4) Charger le recensement 2017 au niveau départemental (niveau d'agrégation pour lanalyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental



# 2.5) Calculer des indices de concentration


# 2.6) Corréler les effectifs à la population

# 2.7) Calculer l'autocorrélation spatiale
# Pour Moran: package spdep, fonction moran.test



#########
#  3 ) Geographically weighted regression
#
# Analysis from Bergeaud, A., & Raimbault, J. (2020). An empirical analysis of the spatial variability of fuel prices in the United States. Transportation Research Part A: Policy and Practice, 132, 131-143.
# -> Determinants of US fuel prices at the county level
#


# 3.1) Charger les données: data/energyprice


# 3.2) Tester des modèles GWR à bandwidth fixe


# 3.3) Optimmiser la bandwidth







