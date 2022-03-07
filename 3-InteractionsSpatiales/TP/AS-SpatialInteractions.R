setwd('~/ComplexSystems/Teaching/2022-AnalyseSpatiale/AnalyseSpatiale-DeSIGeo/3-InteractionsSpatiales/TP/')

#####
# TP : Interaction spatiales
#

#########
# 1) Analyse du reseau Europeen de filiales d'entreprises
#   -> Aires urbaines Fonctionelles Europeennes (base GHSL, Joint Research Center Commission Europeenne)
#   -> liens d'appartenance entre entreprises agreges (poids du lien: turnover pondere)
#   Caracs des aires urbaines: turnover des entreprises, parts de differents secteurs d'activite, pays, population, gdp
#   Caracs des liens: origine, destination, poids, turnover a l'origine, turnover a destination, 
#      pays d'origine, pays de destination, distance geographique, similarite entre structure industrielle
# secteurs d'activite: https://ec.europa.eu/competition/mergers/cases/index/nace_all.html

library(readr)
library(dplyr)
library(sf)
library(mapsf)

# 1.1) Charger les donnees: data/firmsnetwork/{cities.csv,links.csv}
cities = read_csv('data/firmsnetwork/cities.csv')
links = read_csv('data/firmsnetwork/links.csv')


# 1.2) Cartographier la specialisation des aires urbaines
# mf_map(mtq, var = c("", ""), type = "prop_choro")
# fond de carte pays https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#  -> fichier data/pays/ne_110m_admin_0_countries.shp
pays = st_read(dsn='data/pays/',layer='ne_110m_admin_0_countries')

specialisation <- function(zones,activites,activitespec){
  counts = as_tibble(zones)[,activites]
  counts[is.na(counts)]=0
  localshare = counts[,activitespec] / rowSums(counts)
  globalShare = sum(counts[,activitespec])/sum(counts)
  return(localshare/globalShare)
}

activites = c("sectorB","sectorC","sectorM","sectorK","sectorG","sectorD","sectorJ", "sectorH","sectorF", 
              "sectorI","sectorO","sectorN","sectorL","sectorS","sectorE","sectorA","sectorR","sectorQ",
              "sectorP","sectorT","sectorU")

cities$specA = unlist(specialisation(cities,activites,"sectorA"))

cities$specK = unlist(specialisation(cities,activites,"sectorK"))


sfcities =  st_as_sf(cities,coords = c("X","Y"))

mf_map(sfcities)
mf_map(sfcities, var = c("pop", "specA"), type = "prop_choro")
#mf_map(pays)

mf_map(sfcities)
mf_map(sfcities, var = c("pop", "specK"), type = "prop_choro")
#mf_map(pays)


# 1.3) Modeles d'interaction spatiale simples

spint_simple = lm(data=links,formula = log(weight)~log(distance))
summary(spint_simple)

spint_turnover = lm(data=links,formula = log(weight)~log(distance)+
                  log(from_turnover)+log(to_turnover))
summary(spint_turnover)

spint_sim = lm(data=links,formula = log(weight)~log(distance)+
                      log(from_turnover)+log(to_turnover)+log(sim))
summary(spint_sim)

# presence d'overfitting? Non car difference positive
AIC(spint_turnover) - AIC(spint_sim)

# ajouter population et gdp
#links = left_join(links,cities[,c("fua","pop","gdp")],by=c("from_fua"="fua"))
#names(links)[11:12]<-c("from_pop","from_gdp")
#links = left_join(links,cities[,c("fua","pop","gdp")],by=c("to_fua"="fua"))
#names(links)[13:14]<-c("to_pop","to_gdp")
#
#
#spint_popgdp = lm(data=links,formula = log(weight)~log(distance)+
#                    log(from_turnover)+log(to_turnover)+log(sim)+
#                    log(from_pop)+log(from_gdp)+log(to_pop)+log(to_gdp)
#                    )
#summary(spint_popgdp)
# modele moins bon en termes de R2


spint_all = lm(data=links,formula = log(weight)~log(distance)+
                 log(from_turnover)+log(to_turnover)+log(sim)+
                 from_country+to_country
                 )
summary(spint_all)


# 1.4) Modeles contraints (origine et/ou destination)

spint_turnover_constraint_origin = lm(data=links,formula = log(weight)~log(distance)+
                      log(from_turnover)+log(to_turnover)+from_fua)
summary(spint_turnover_constraint_origin)

spint_turnover_constraint_destination = lm(data=links,formula = log(weight)~log(distance)+
                                        log(from_turnover)+log(to_turnover)+to_fua )
summary(spint_turnover_constraint_destination)

# contrainte double
spint_turnover_constraint_OD = lm(data=links,formula = log(weight)~log(distance)+
                                             log(from_turnover)+log(to_turnover)+
                                    from_fua+to_fua)
summary(spint_turnover_constraint_OD)


# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log'))) : generalized linear model

links$intweight = round(links$weight)

spint_poisson_all = glm(data=links,formula = intweight~log(distance)+
      log(from_turnover)+log(to_turnover)+log(sim)+
      from_country+to_country,
     family = poisson(link='log')
    )
summary(spint_poisson_all)

1 - sum((links$intweight - fitted(spint_poisson_all))^2) / sum((links$intweight - mean(links$intweight))^2)







