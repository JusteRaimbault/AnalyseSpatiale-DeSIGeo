
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
g_lattice = igraph::make_lattice(dimvector = c(50,50))

positions = layout_on_grid(g_lattice)
V(g_lattice)$x = positions[,1];V(g_lattice)$y = positions[,2]

plot(g_lattice,vertex.size=0,vertex.label=NA)



# 1.3) Supprimer des liens aléatoirement;
#  étudier la taille de la plus grande composante connexe
#  en fonction du nommbre de lien supprimés

p = 0.4
g_lattice_del = subgraph.edges(g_lattice,
                               sample.int(
                                 n = ecount(g_lattice),
                                 size = floor(p* ecount(g_lattice)),
                                 replace = F
                                 ),
                                 delete.vertices = T
                               )

plot(g_lattice_del,vertex.size=0,vertex.label=NA)

res=data.frame()
for(n in c(10,20,30,40,50)){
  show(n)
  for(p in seq(from=0.2,to=0.8,by=0.01)){
    g_lattice = igraph::make_lattice(dimvector = c(n,n))
    g_lattice_del = subgraph.edges(g_lattice,sample.int(n = ecount(g_lattice),
                        size = floor(p* ecount(g_lattice)),replace = F),
                        delete.vertices = T)
    comps = components(g_lattice_del)
    maxcsize = max(comps$csize)/vcount(g_lattice)
    res=rbind(res,c(n,p,maxcsize,"lattice"))
    
    #g_random = erdos.renyi.game(n=n*n,p,type = "gnp")
    #comps = components(g_random)
    #maxcsize = max(comps$csize)/vcount(g_random)
    #res=rbind(res,c(n,p,maxcsize,"random"))
  }
}
names(res)<- c("n","p","maxcsize","type")
res$maxcsize=as.numeric(res$maxcsize);res$n=as.numeric(res$n);res$p=as.numeric(res$p)

g=ggplot(res,aes(x=p,y=maxcsize,color=n,group=interaction(n,type),shape=type))
g+geom_point()+geom_line()


# 1.4) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)

g_lattice = igraph::make_lattice(dimvector = c(50,50))
positions = layout_on_grid(g_lattice)
V(g_lattice)$x = positions[,1];V(g_lattice)$y = positions[,2]

p = 0.55
g_lattice_del = subgraph.edges(g_lattice,
                               sample.int(
                                 n = ecount(g_lattice),
                                 size = floor(p* ecount(g_lattice)),
                                 replace = F
                               ),
                               delete.vertices = T
)

V(g_lattice_del)$x = jitter(factor = 1.0,V(g_lattice_del)$x)
V(g_lattice_del)$y = jitter(factor = 1.0,V(g_lattice_del)$y)

plot(g_lattice_del,vertex.size=0,vertex.label=NA)

# calculer les poids: distance geographique
#edge_lengths = lapply(E(g_lattice_del),function(e){
#  bothends = ends(e)
#  sqrt((bothends[1]$x - bothends[2]$x)^2 + 
#         (bothends[1]$y - bothends[2]$y)^2)
#})
vertices_ends = ends(g_lattice_del,es = 1:ecount(g_lattice_del))
edge_lengths = apply(vertices_ends,1,function(e){
  return(
  sqrt(
  (V(g_lattice_del)$x[e[1]] - V(g_lattice_del)$x[e[2]])^2 +
    (V(g_lattice_del)$y[e[1]] - V(g_lattice_del)$y[e[2]])^2
  )
  )
})

E(g_lattice_del)$weight = edge_lengths

# tous les plus courts chemins
d = distances(g_lattice_del)
summary(c(d[d<Inf]))

# un plus court chemin
path = shortest_paths(g_lattice_del,from = sample.int(vcount(g_lattice_del),1),
               to = sample.int(vcount(g_lattice_del),1)
               )$vpath[[1]]

plot(g_lattice_del,vertex.size=2,vertex.label=NA,
      vertex.color = ifelse(V(g_lattice_del)%in%path,'green','black')
     )

# plus court chemin entre coins
comps = components(g_lattice_del)
index_of_largest_component = which(comps$csize==max(comps$csize))
vertices_in_largest = comps$membership==index_of_largest_component
subgraph_largest = induced_subgraph(g_lattice_del, vertices_in_largest)

first_col = V(subgraph_largest)[V(subgraph_largest)$x < min(V(subgraph_largest)$x + 1)]
from = first_col[first_col$y==max(first_col$y)]

last_col = V(subgraph_largest)[V(subgraph_largest)$x > max(V(subgraph_largest)$x - 1)]
to = last_col[last_col$y==min(last_col$y)]

path = shortest_paths(subgraph_largest,from = from,to = to)$vpath[[1]]

plot(subgraph_largest,vertex.size=2,vertex.label=NA,
     vertex.color = ifelse(V(subgraph_largest)%in%path,'green','black')
)




#####
## Partie 2
# Analyse de reseau social
# Data : co-occurence des personnages de A Song of Ice and Fire
#  https://github.com/mathbeveridge/asoiaf

library(readr)
library(igraph)

# 1.1) charger les donnees
# Data available under a CC-BY-NC-SA Licence at https://github.com/mathbeveridge/asoiaf
nodes <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv")
edges <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")

# 1.2) ploter le graph avec un layout adapte


# 1.3) distribution des degres


# 1.4) centralites


# 1.5) detection de communautes


# 1.6) plotter avec multiples infos: communaute, centralite, degre








