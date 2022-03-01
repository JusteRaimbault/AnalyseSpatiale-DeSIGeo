

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

library(dplyr)
library(readr)

# 1.1) charger les données
#  Question : système de coordonnées?

coords <- read_csv("data/FRUrbanAreas/coords.csv",col_names = F)
colnames(coords)<-c("name","x","y")
  
populations <- read_csv("data/FRUrbanAreas/pop50.csv",col_names = F)
dates <- read_csv("data/FRUrbanAreas/dates.csv", col_names = F)

colnames(populations)<- as.character(dates$X1)

# coordonnees coherentes ?
plot(coords$x,coords$y)

# population totale par annee?
totalpop = colSums(populations)
plot(dates$X1,totalpop,type='l')
points(dates$X1,populations[1,],type='l',col='red')

# 1.2) calculer point moyen, point moyen pondéré, distance-type pour chaque date

# point moyen
meanx = mean(coords$x)
meany = mean(coords$y)
meanpoint = apply(coords[,c("x","y")],MARGIN = 2, mean)

plot(coords$x,coords$y);points(meanx,meany,col='red')


# point moyen pondere

# pour 1831
wmeanx1831 = sum(populations$`1831`*coords$x)/sum(populations$`1831`)
wmeany1831 = sum(populations$`1831`*coords$y)/sum(populations$`1831`)

# boucle pour chaque annee
wmean = list()
for(date in as.character(dates$X1)){
  show(date)
  wmeanx = sum(populations[,date]*coords$x)/sum(populations[,date])
  wmeany = sum(populations[,date]*coords$y)/sum(populations[,date])
  wmean[[date]] = c(wmeanx,wmeany)
}

# meme chose avec apply
wmean = apply(populations,MARGIN = 2,FUN = function(pop){
  currenttotalpop = sum(pop)
  wmeanx = sum(pop*coords$x)/currenttotalpop
  wmeany = sum(pop*coords$y)/currenttotalpop
  return(c(wmeanx,wmeany))
}
)
wmean=t(wmean)
colnames(wmean)=c("x","y")
wmean=data.frame(wmean)
wmean$date = dates$X1

# 1.3) cartographier les villes et l'évolution du point moyen
#  Question : quel package pour cartographier "simplement" ? -> ggplot (function geom_sf)
#  Données supplémentaires: limites des régions: data/regions/regions_2015_metropole_region.shp
#   ! systèmes de coordonnées à gérer

library(sf)
library(ggplot2)

# voir les drivers disponibles (formats de fichiers)
st_drivers()

regions <- st_read(dsn='data/regions/',layer='regions_2015_metropole_region')

# plot point moyen uniquement
g=ggplot(data=wmean,aes(x=x,y=y,col=date))
g+geom_point()

# "carte" avec point moyen et regions
g=ggplot(data=regions)
g+geom_sf()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))

# meme carte "zoomee"
g=ggplot(data=regions[regions$RégION%in%c("Bourgogne et Franche-Comté","Centre"),])
g+geom_sf()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))



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







