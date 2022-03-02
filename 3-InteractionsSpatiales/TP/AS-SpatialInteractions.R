setwd('ComplexSystems/Teaching/2022-AnalyseSpatiale/AnalyseSpatiale-DeSIGeo/3-InteractionsSpatiales/TP/')

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


# 1.1) Charger les donnees: data/firmsnetwork/{cities.csv,links.csv}



# 1.2) Cartographier la specialisation des aires urbaines
# mf_map(mtq, var = c("", ""), type = "prop_choro")
# fond de carte pays https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#  -> fichier data/pays/ne_110m_admin_0_countries.shp


# 1.3) Modeles d'interaction spatiale simples



# 1.4) Modeles contraints (origine et/ou destination)



# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log'))) : generalized linear model










