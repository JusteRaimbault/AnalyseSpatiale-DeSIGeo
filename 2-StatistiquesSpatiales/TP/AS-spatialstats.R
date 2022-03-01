setwd('ComplexSystems/Teaching/2022-AnalyseSpatiale/AnalyseSpatiale-DeSIGeo/2-StatistiquesSpatiales/TP/')

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



# distance type ponderee
wsigma = apply(populations,MARGIN = 2,FUN = function(pop){
  currenttotalpop = sum(pop)
  wmeanx = sum(pop*coords$x)/currenttotalpop
  wmeany = sum(pop*coords$y)/currenttotalpop
  sigma = sqrt(sum(pop*((coords$x - wmeanx)^2 + (coords$y - wmeany)^2))/currenttotalpop)
  return(sigma)
}
)

ggplot(data=data.frame(date=dates$X1,sigma=wsigma),aes(x=date,y=sigma))+geom_line()



# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)
spmedian = spatial.median(coords[,c("x","y")])

#g=ggplot(data=regions[regions$RégION%in%c("Bourgogne et Franche-Comté","Centre"),])
ggplot()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))+
  geom_point(data=data.frame(x=spmedian[1]*100,y=spmedian[2]*100),aes(x=x,y=y),color='red',shape=3)

# median "pondere"?
#spmedian1831 = spatial.median(populations$`1831`/sum(populations$`1831`)*coords[,c("x","y")]) # fonctionne pas

wspmedians=data.frame()
for(date in as.character(dates$X1)){
synthpoints=coords[,c("x","y")]
for(i in 1:nrow(coords)){
  addpoints=matrix(data=jitter(rep(unlist(c(coords[i,c("x","y")])), floor(populations[i,date]/100)),amount=100),ncol=2,byrow = T)
  colnames(addpoints)=c("x","y")
  synthpoints=rbind(synthpoints,addpoints)
}
show(dim(synthpoints))
wspmedian = spatial.median(synthpoints)
show(wspmedian)
wspmedians=rbind(wspmedians,wspmedian)
}
wspmedians$date=dates$X1
names(wspmedians)<-c("x","y","date")


ggplot()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))+
  geom_point(data=wspmedians,aes(x=x*100,y=y*100,col=date),shape=3)



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

library(sf)
coiffeurs = st_read("data/osmdata/Salon_de_coiffure.gpkg")
facs = st_read("data/osmdata/Enseignement_Supérieur.gpkg")



# systeme de coordonnees?
st_crs(facs)

# reprojection
coiffeurs = st_transform(coiffeurs,"EPSG:2154")
facs = st_transform(facs,"EPSG:2154")

regions <- st_transform(st_read(dsn = 'data/regions/',layer = 'regions_2015_metropole_region'),"EPSG:2154")


# 2.2) Calculer l'indice de plus proche voisin dans le cas d'un faible nombre de points (universités par exemple)

distancefacs = st_distance(facs)
nearestneighdists = apply(distancefacs,1,function(r){min(r[r>0])})
nndindex = 2*sqrt(nrow(facs)/sum(st_area(regions)))*mean(nearestneighdists)


# 2.3) Cartographier la densité des points

coiffeursmetro = st_filter(coiffeurs,regions)

library(ggplot2)

g=ggplot(regions)
g+geom_sf()+geom_density2d_filled(data=data.frame(st_coordinates(coiffeursmetro)),mapping=aes(x=X,y=Y),alpha=0.5)


# 2.4) Charger le recensement 2017 au niveau départemental (niveau d'agrégation pour lanalyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental

library(readr)
library(dplyr)
library(ggplot2)

popdeps = read_delim('data/insee/Departements.csv', delim=";")
deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

deps = left_join(deps,popdeps[,c("CODDEP","PTOT")],by=c("CODE_DEPT"="CODDEP"))

# 2.5) Corréler les effectifs à la population

aggrcoiffeurs = st_join(coiffeursmetro, deps) %>% group_by(CODE_DEPT) %>%
  summarise(numcoiffeur = n(), population = PTOT[1])

cor.test(aggrcoiffeurs$numcoiffeur,aggrcoiffeurs$population)
cor.test(aggrcoiffeurs$numcoiffeur,aggrcoiffeurs$population,method = "spearman")

ggplot(data.frame(logcoif=log(aggrcoiffeurs$numcoiffeur),logpop=log(aggrcoiffeurs$population)),aes(x=logpop,y=logcoif))+
  geom_point()+geom_smooth()

summary(lm(data=data.frame(logcoif=log(aggrcoiffeurs$numcoiffeur),logpop=log(aggrcoiffeurs$population)),
   formula = logcoif~logpop
   ))
summary(lm(data=data.frame(coif=aggrcoiffeurs$numcoiffeur,pop=aggrcoiffeurs$population),
           formula = coif~pop
))


aggrfacs = st_join(facs, deps) %>% group_by(CODE_DEPT) %>%
  summarise(numfacs = n(), population = PTOT[1])

summary(lm(data=data.frame(logfacs=log(aggrfacs$numfacs),logpop=log(aggrfacs$population)),
           formula = logfacs~logpop
))


# 2.6) Calculer des indices de concentration




# 2.7) Calculer l'autocorrélation spatiale
# Pour Moran: package spdep, fonction moran.test

library(spdep)
moran.test(...)




#########
#  3 ) Geographically weighted regression
#
# Analysis from Bergeaud, A., & Raimbault, J. (2020). An empirical analysis of the spatial variability of fuel prices in the United States. Transportation Research Part A: Policy and Practice, 132, 131-143.
# -> Determinants of US fuel prices at the county level
#

library(GWmodel)

# 3.1) Charger les données: data/energyprice


# 3.2) Tester des modèles GWR à bandwidth fixe


# 3.3) Optimmiser la bandwidth







