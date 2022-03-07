
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()
# -> matrice d'adjacence, diametre (plus long plus court chemin); plotter le graph: layout algorithms
g_erenyi = erdos.renyi.game(n = 1000, p.or.m = 0.1, type="gnp")
#g_erenyi = erdos.renyi.game(n = 1000, p.or.m = 10000, type="gnm")

# densite
2 * ecount(g_erenyi) / (vcount(g_erenyi)*(vcount(g_erenyi) - 1))

# sommets
V(g_erenyi)
# sommet no 1
V(g_erenyi)[1]
# attributs
V(g_erenyi)$name = paste0("sommet",1:100)
V(g_erenyi)$name

# edges
E(g_erenyi)
# nombre de liens
length(E(g_erenyi))
ecount(g_erenyi)

# definir des poids
E(g_erenyi)$weights = runif(ecount(g_erenyi))

# matrice d'adjacence: class Matrix -> representation sparse
A = as_adj(g_erenyi)

# nombre de composante connexes?
comps = components(g_erenyi)
comps$membership
comps$csize
# extraire le sous-graphe correspondant a la plus grosse composante (le faire si le graphe en a plusieurs)
index_of_largest_component = which(comps$csize==max(comps$csize))
vertices_in_largest = comps$membership==index_of_largest_component
subgraph_largest = induced_subgraph(g_erenyi, vertices_in_largest)
diameter(subgraph_largest)

# diametre du graphe (non pondere - ! attribut "weight" pris par default, ne pas donner ce nom
# pour faire du non-pondere
diameter(g_erenyi,unconnected = T)

# diametre pondere
diameter(g_erenyi,weights = E(g_erenyi)$weights)

# diametre en fonction taille et proba
res = data.frame()
for(n in seq(from=100,to=1000,by=100)){
  for(p in seq(from=-3,to=-0.3,by=0.1)){
    g_erenyi = erdos.renyi.game(n = n, p.or.m = 10^p, type="gnp")
    d = diameter(g_erenyi,unconnected = T)
    res = rbind(res,c(n,p,d))
  }
}
colnames(res)<-c("n","p","d")

g=ggplot(res,aes(x=p,y=d,group=n,color=n))
g+geom_point()+geom_line()


# plotter le graphe
g_erenyi = erdos.renyi.game(n = 100, p.or.m = 0.1, type="gnp")
plot(g_erenyi,vertex.size=0,vertex.label=NA)

# layouts: algorithme de spatialisation du graphe
#positions = layout_in_circle(g_erenyi)
positions = layout.fruchterman.reingold(g_erenyi)

V(g_erenyi)$x = positions[,1];V(g_erenyi)$y = positions[,2]
plot(g_erenyi,vertex.size=0,vertex.label=NA)





# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice


# 1.3) Supprimer des liens aléatoirement; étudier la taille de la plus grande composante connexe
#  en fonction du nommbre de lien supprimés


# 1.4) perturber les coordonnées des noeuds de la grille pour obtenir des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths() pour Dijsktra, distances pour Floyd-Warshall




#####
## Partie 2









