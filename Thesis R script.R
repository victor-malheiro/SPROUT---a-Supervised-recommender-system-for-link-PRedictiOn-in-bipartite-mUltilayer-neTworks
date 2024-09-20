install.packages("igraph")
install.packages("tidyr")
install.packages("dplyr")
install.packages("pROC")
install.packages("BDgraph")
install.packages("graphlayouts")
install.packages("ggraph")
install.packages("metric.cluster.global")
install.packages("MLmetrics")
install.packages("Metrics")
install.packages("recipes", type = "binary")
install.packages("caret")

library(igraph)
library(tidyr)
library(dplyr)
library(pROC)
library(BDgraph)
library(graphlayouts)
library(ggraph)
library(metric.cluster.global)
library(MLmetrics)
library(Metrics)
library(recipes)
library(caret)

set.seed(10)

ratings <- read.csv("ratings.csv")
movies <- read.csv("movies.csv")

#merged <- merge(ratings, movies)
#write.csv(merged, "merged.csv")

movies1 <- separate(data=movies, col=genres, into=c("genre1", "genre2", "genre3", "genre4", "genre5", "genre6"), sep="\\|")

#action
mlaction1 <- movies1[grep("Action", movies1$genre1),]
mlaction2 <- movies1[grep("Action", movies1$genre2),]
mlaction3 <- movies1[grep("Action", movies1$genre3),]
mlaction4 <- movies1[grep("Action", movies1$genre4),]
mlaction5 <- movies1[grep("Action", movies1$genre5),]
mlaction6 <- movies1[grep("Action", movies1$genre6),]
mlaction <- rbind(mlaction1, mlaction2, mlaction3, mlaction4, mlaction5, mlaction6)

#adventure
mladventure1 <- movies1[grep("Adventure", movies1$genre1),]
mladventure2 <- movies1[grep("Adventure", movies1$genre2),]
mladventure3 <- movies1[grep("Adventure", movies1$genre3),]
mladventure4 <- movies1[grep("Adventure", movies1$genre4),]
mladventure5 <- movies1[grep("Adventure", movies1$genre5),]
mladventure6 <- movies1[grep("Adventure", movies1$genre6),]
mladventure <- rbind(mladventure1, mladventure2, mladventure3, mladventure4, mladventure5, mladventure6)

#animation
mlanimation1 <- movies1[grep("Animation", movies1$genre1),]
mlanimation2 <- movies1[grep("Animation", movies1$genre2),]
mlanimation3 <- movies1[grep("Animation", movies1$genre3),]
mlanimation4 <- movies1[grep("Animation", movies1$genre4),]
mlanimation5 <- movies1[grep("Animation", movies1$genre5),]
mlanimation6 <- movies1[grep("Animation", movies1$genre6),]
mlanimation <- rbind(mlanimation1, mlanimation2, mlanimation3, mlanimation4, mlanimation5, mlanimation6)

#children
mlchildren1 <- movies1[grep("Children", movies1$genre1),]
mlchildren2 <- movies1[grep("Children", movies1$genre2),]
mlchildren3 <- movies1[grep("Children", movies1$genre3),]
mlchildren4 <- movies1[grep("Children", movies1$genre4),]
mlchildren5 <- movies1[grep("Children", movies1$genre5),]
mlchildren6 <- movies1[grep("Children", movies1$genre6),]
mlchildren <- rbind(mlchildren1, mlchildren2, mlchildren3, mlchildren4, mlchildren5, mlchildren6)

#comedy
mlcomedy1 <- movies1[grep("Comedy", movies1$genre1),]
mlcomedy2 <- movies1[grep("Comedy", movies1$genre2),]
mlcomedy3 <- movies1[grep("Comedy", movies1$genre3),]
mlcomedy4 <- movies1[grep("Comedy", movies1$genre4),]
mlcomedy5 <- movies1[grep("Comedy", movies1$genre5),]
mlcomedy6 <- movies1[grep("Comedy", movies1$genre6),]
mlcomedy <- rbind(mlcomedy1, mlcomedy2, mlcomedy3, mlcomedy4, mlcomedy5, mlcomedy6)

#crime
mlcrime1 <- movies1[grep("Crime", movies1$genre1),]
mlcrime2 <- movies1[grep("Crime", movies1$genre2),]
mlcrime3 <- movies1[grep("Crime", movies1$genre3),]
mlcrime4 <- movies1[grep("Crime", movies1$genre4),]
mlcrime5 <- movies1[grep("Crime", movies1$genre5),]
mlcrime6 <- movies1[grep("Crime", movies1$genre6),]
mlcrime <- rbind(mlcrime1, mlcrime2, mlcrime3, mlcrime4, mlcrime5, mlcrime6)

#documentary
mldocumentary1 <- movies1[grep("Documentary", movies1$genre1),]
mldocumentary2 <- movies1[grep("Documentary", movies1$genre2),]
mldocumentary3 <- movies1[grep("Documentary", movies1$genre3),]
mldocumentary4 <- movies1[grep("Documentary", movies1$genre4),]
mldocumentary5 <- movies1[grep("Documentary", movies1$genre5),]
mldocumentary6 <- movies1[grep("Documentary", movies1$genre6),]
mldocumentary <- rbind(mldocumentary1, mldocumentary2, mldocumentary3, mldocumentary4, mldocumentary5, mldocumentary6)

#drama
mldrama1 <- movies1[grep("Drama", movies1$genre1),]
mldrama2 <- movies1[grep("Drama", movies1$genre2),]
mldrama3 <- movies1[grep("Drama", movies1$genre3),]
mldrama4 <- movies1[grep("Drama", movies1$genre4),]
mldrama5 <- movies1[grep("Drama", movies1$genre5),]
mldrama6 <- movies1[grep("Drama", movies1$genre6),]
mldrama <- rbind(mldrama1, mldrama2, mldrama3, mldrama4, mldrama5, mldrama6)

#fantasy
mlfantasy1 <- movies1[grep("Fantasy", movies1$genre1),]
mlfantasy2 <- movies1[grep("Fantasy", movies1$genre2),]
mlfantasy3 <- movies1[grep("Fantasy", movies1$genre3),]
mlfantasy4 <- movies1[grep("Fantasy", movies1$genre4),]
mlfantasy5 <- movies1[grep("Fantasy", movies1$genre5),]
mlfantasy6 <- movies1[grep("Fantasy", movies1$genre6),]
mlfantasy <- rbind(mlfantasy1, mlfantasy2, mlfantasy3, mlfantasy4, mlfantasy5, mlfantasy6)

#filmnoir
mlfilmnoir1 <- movies1[grep("Film-Noir", movies1$genre1),]
mlfilmnoir2 <- movies1[grep("Film-Noir", movies1$genre2),]
mlfilmnoir3 <- movies1[grep("Film-Noir", movies1$genre3),]
mlfilmnoir4 <- movies1[grep("Film-Noir", movies1$genre4),]
mlfilmnoir5 <- movies1[grep("Film-Noir", movies1$genre5),]
mlfilmnoir6 <- movies1[grep("Film-Noir", movies1$genre6),]
mlfilmnoir <- rbind(mlfilmnoir1, mlfilmnoir2, mlfilmnoir3, mlfilmnoir4, mlfilmnoir5, mlfilmnoir6)

#horror
mlhorror1 <- movies1[grep("Horror", movies1$genre1),]
mlhorror2 <- movies1[grep("Horror", movies1$genre2),]
mlhorror3 <- movies1[grep("Horror", movies1$genre3),]
mlhorror4 <- movies1[grep("Horror", movies1$genre4),]
mlhorror5 <- movies1[grep("Horror", movies1$genre5),]
mlhorror6 <- movies1[grep("Horror", movies1$genre6),]
mlhorror <- rbind(mlhorror1, mlhorror2, mlhorror3, mlhorror4, mlhorror5, mlhorror6)

#musical
mlmusical1 <- movies1[grep("Musical", movies1$genre1),]
mlmusical2 <- movies1[grep("Musical", movies1$genre2),]
mlmusical3 <- movies1[grep("Musical", movies1$genre3),]
mlmusical4 <- movies1[grep("Musical", movies1$genre4),]
mlmusical5 <- movies1[grep("Musical", movies1$genre5),]
mlmusical6 <- movies1[grep("Musical", movies1$genre6),]
mlmusical <- rbind(mlmusical1, mlmusical2, mlmusical3, mlmusical4, mlmusical5, mlmusical6)

#mystery
mlmystery1 <- movies1[grep("Mystery", movies1$genre1),]
mlmystery2 <- movies1[grep("Mystery", movies1$genre2),]
mlmystery3 <- movies1[grep("Mystery", movies1$genre3),]
mlmystery4 <- movies1[grep("Mystery", movies1$genre4),]
mlmystery5 <- movies1[grep("Mystery", movies1$genre5),]
mlmystery6 <- movies1[grep("Mystery", movies1$genre6),]
mlmystery <- rbind(mlmystery1, mlmystery2, mlmystery3, mlmystery4, mlmystery5, mlmystery6)

#romance
mlromance1 <- movies1[grep("Romance", movies1$genre1),]
mlromance2 <- movies1[grep("Romance", movies1$genre2),]
mlromance3 <- movies1[grep("Romance", movies1$genre3),]
mlromance4 <- movies1[grep("Romance", movies1$genre4),]
mlromance5 <- movies1[grep("Romance", movies1$genre5),]
mlromance6 <- movies1[grep("Romance", movies1$genre6),]
mlromance <- rbind(mlromance1, mlromance2, mlromance3, mlromance4, mlromance5, mlromance6)

#scifi
mlscifi1 <- movies1[grep("Sci-Fi", movies1$genre1),]
mlscifi2 <- movies1[grep("Sci-Fi", movies1$genre2),]
mlscifi3 <- movies1[grep("Sci-Fi", movies1$genre3),]
mlscifi4 <- movies1[grep("Sci-Fi", movies1$genre4),]
mlscifi5 <- movies1[grep("Sci-Fi", movies1$genre5),]
mlscifi6 <- movies1[grep("Sci-Fi", movies1$genre6),]
mlscifi <- rbind(mlscifi1, mlscifi2, mlscifi3, mlscifi4, mlscifi5, mlscifi6)

#thriller
mlthriller1 <- movies1[grep("Thriller", movies1$genre1),]
mlthriller2 <- movies1[grep("Thriller", movies1$genre2),]
mlthriller3 <- movies1[grep("Thriller", movies1$genre3),]
mlthriller4 <- movies1[grep("Thriller", movies1$genre4),]
mlthriller5 <- movies1[grep("Thriller", movies1$genre5),]
mlthriller6 <- movies1[grep("Thriller", movies1$genre6),]
mlthriller <- rbind(mlthriller1, mlthriller2, mlthriller3, mlthriller4, mlthriller5, mlthriller6)

#war
mlwar1 <- movies1[grep("War", movies1$genre1),]
mlwar2 <- movies1[grep("War", movies1$genre2),]
mlwar3 <- movies1[grep("War", movies1$genre3),]
mlwar4 <- movies1[grep("War", movies1$genre4),]
mlwar5 <- movies1[grep("War", movies1$genre5),]
mlwar6 <- movies1[grep("War", movies1$genre6),]
mlwar <- rbind(mlwar1, mlwar2, mlwar3, mlwar4, mlwar5, mlwar6)

#western
mlwestern1 <- movies1[grep("Western", movies1$genre1),]
mlwestern2 <- movies1[grep("Western", movies1$genre2),]
mlwestern3 <- movies1[grep("Western", movies1$genre3),]
mlwestern4 <- movies1[grep("Western", movies1$genre4),]
mlwestern5 <- movies1[grep("Western", movies1$genre5),]
mlwestern6 <- movies1[grep("Western", movies1$genre6),]
mlwestern <- rbind(mlwestern1, mlwestern2, mlwestern3, mlwestern4, mlwestern5, mlwestern6)

ratings$userId <- as.character(ratings$userId)

dfaction <- merge(ratings, mlaction)
dfadventure <- merge(ratings, mladventure)
dfanimation <- merge(ratings, mlanimation)
dfchildren <- merge(ratings, mlchildren)
dfcomedy <- merge(ratings, mlcomedy)
dfcrime <- merge(ratings, mlcrime)
dfdocumentary <- merge(ratings, mldocumentary)
dfdrama <- merge(ratings, mldrama)
dffantasy <- merge(ratings, mlfantasy)
dffilmnoir<- merge(ratings, mlfilmnoir)
dfhorror <- merge(ratings, mlhorror)
dfmusical <- merge(ratings, mlmusical)
dfmystery <- merge(ratings, mlmystery)
dfromance <- merge(ratings, mlromance)
dfscifi <- merge(ratings, mlscifi)
dfthriller <- merge(ratings, mlthriller)
dfwar <- merge(ratings, mlwar)
dfwestern <- merge(ratings, mlwestern)


netaction <- graph.edgelist(as.matrix(dfaction[,c(2,5)]))
netadventure <- graph.edgelist(as.matrix(dfadventure[,c(2,5)]))
netanimation <- graph.edgelist(as.matrix(dfanimation[,c(2,5)]))
netchildren <- graph.edgelist(as.matrix(dfchildren[,c(2,5)]))
netcomedy <- graph.edgelist(as.matrix(dfcomedy[,c(2,5)]))
netcrime <- graph.edgelist(as.matrix(dfcrime[,c(2,5)]))
netdocumentary <- graph.edgelist(as.matrix(dfdocumentary[,c(2,5)]))
netdrama <- graph.edgelist(as.matrix(dfdrama[,c(2,5)]))
netfantasy <- graph.edgelist(as.matrix(dffantasy[,c(2,5)]))
netfilmnoir <- graph.edgelist(as.matrix(dffilmnoir[,c(2,5)]))
nethorror <- graph.edgelist(as.matrix(dfhorror[,c(2,5)]))
netmusical <- graph.edgelist(as.matrix(dfmusical[,c(2,5)]))
netmystery <- graph.edgelist(as.matrix(dfmystery[,c(2,5)]))
netromance <- graph.edgelist(as.matrix(dfromance[,c(2,5)]))
netscifi <- graph.edgelist(as.matrix(dfscifi[,c(2,5)]))
netthriller <- graph.edgelist(as.matrix(dfthriller[,c(2,5)]))
netwar <- graph.edgelist(as.matrix(dfwar[,c(2,5)]))
netwestern <- graph.edgelist(as.matrix(dfwestern[,c(2,5)]))


V(netaction)$type <- bipartite.mapping(netaction)$type
V(netadventure)$type <- bipartite.mapping(netadventure)$type
V(netanimation)$type <- bipartite.mapping(netanimation)$type
V(netchildren)$type <- bipartite.mapping(netchildren)$type
V(netcomedy)$type <- bipartite.mapping(netcomedy)$type
V(netcrime)$type <- bipartite.mapping(netcrime)$type
V(netdocumentary)$type <- bipartite.mapping(netdocumentary)$type
V(netdrama)$type <- bipartite.mapping(netdrama)$type
V(netfantasy)$type <- bipartite.mapping(netfantasy)$type
V(netfilmnoir)$type <- bipartite.mapping(netfilmnoir)$type
V(nethorror)$type <- bipartite.mapping(nethorror)$type
V(netmusical)$type <- bipartite.mapping(netmusical)$type
V(netmystery)$type <- bipartite.mapping(netmystery)$type
V(netromance)$type <- bipartite.mapping(netromance)$type
V(netscifi)$type <- bipartite.mapping(netscifi)$type
V(netthriller)$type <- bipartite.mapping(netthriller)$type
V(netwar)$type <- bipartite.mapping(netwar)$type
V(netwestern)$type <- bipartite.mapping(netwestern)$type


#projetar a rede bipartida em duas redes one mode
#Action
proj_net_action <- bipartite.projection(netaction)

  plot(netaction, 
     main = "Bipartite Network for layer Action",
     layout=-layout.bipartite(netaction)[,2:1])

#para ver as redes one mode
proj_net_action$proj1 #user
proj_net_action$proj2 #movie

plot(proj_net_action$proj1)
plot(proj_net_action$proj2)

ecount(proj_net_action$proj1)
mean(degree(proj_net_action$proj1))
graph.density(proj_net_action$proj1, loops = FALSE)
transitivity(proj_net_action$proj1, type = "average")

ggraph(proj_net_action$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 5)+
  theme_graph()

#Adventure
proj_net_adventure <- bipartite.projection(netadventure)

plot(netadventure, 
     main = "Bipartite Network for layer Adventure",
     layout=-layout.bipartite(netadventure)[,2:1])

#para ver as redes one mode
proj_net_adventure$proj1 #user
proj_net_adventure$proj2 #movie

plot(proj_net_adventure$proj1)
plot(proj_net_adventure$proj2)

ecount(proj_net_adventure$proj1)
mean(degree(proj_net_adventure$proj1))
graph.density(proj_net_adventure$proj1, loops = FALSE)
transitivity(proj_net_adventure$proj1, type = "average")

ggraph(proj_net_adventure$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 5)+
  theme_graph()

#Animation
proj_net_animation <- bipartite.projection(netanimation)

plot(netanimation, 
     main = "Bipartite Network for layer Animation",
     layout=-layout.bipartite(netanimation)[,2:1])

#para ver as redes one mode
proj_net_animation$proj1 #user
proj_net_animation$proj2 #movie

plot(proj_net_animation$proj1)
plot(proj_net_animation$proj2)

ecount(proj_net_animation$proj1)
mean(degree(proj_net_animation$proj1))
graph.density(proj_net_animation$proj1, loops = FALSE)
transitivity(proj_net_animation$proj1, type = "average")

ggraph(proj_net_animation$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 5)+
  theme_graph()

#Children
proj_net_children <- bipartite.projection(netchildren)

plot(netchildren, 
     main = "Bipartite Network for layer Children",
     layout=-layout.bipartite(netchildren)[,2:1])

#para ver as redes one mode
proj_net_children$proj1 #user
proj_net_children$proj2 #movie

plot(proj_net_children$proj1)
plot(proj_net_children$proj2)

ecount(proj_net_children$proj1)
mean(degree(proj_net_children$proj1))
graph.density(proj_net_children$proj1, loops = FALSE)
transitivity(proj_net_children$proj1, type = "average")

ggraph(proj_net_children$proj1,layout = "sparse_stress",pivots = 10, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 5)+
  theme_graph()

#Comedy
proj_net_comedy <- bipartite.projection(netcomedy)

plot(netcomedy, 
     main = "Bipartite Network for layer Comedy",
     layout=-layout.bipartite(netcomedy)[,2:1])

#para ver as redes one mode
proj_net_comedy$proj1 #user
proj_net_comedy$proj2 #movie

plot(proj_net_comedy$proj1)
plot(proj_net_comedy$proj2)

ecount(proj_net_comedy$proj1)
mean(degree(proj_net_comedy$proj1))
graph.density(proj_net_comedy$proj1, loops = FALSE)
transitivity(proj_net_comedy$proj1, type = "average")

ggraph(proj_net_comedy$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 2)+
  theme_graph()

#Crime
proj_net_crime <- bipartite.projection(netcrime)

plot(netcrime, 
     main = "Bipartite Network for layer Crime",
     layout=-layout.bipartite(netcrime)[,2:1])

#para ver as redes one mode
proj_net_crime$proj1 #user
proj_net_crime$proj2 #movie

plot(proj_net_crime$proj1)
plot(proj_net_crime$proj2)

ecount(proj_net_crime$proj1)
mean(degree(proj_net_crime$proj1))
graph.density(proj_net_crime$proj1, loops = FALSE)
transitivity(proj_net_crime$proj1, type = "average")

ggraph(proj_net_crime$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 2)+
  theme_graph()

#Documentary
proj_net_documentary <- bipartite.projection(netdocumentary)

plot(netdocumentary, 
     main = "Bipartite Network for layer Documentary",
     layout=-layout.bipartite(netdocumentary)[,2:1])

#para ver as redes one mode
proj_net_documentary$proj1 #user
proj_net_documentary$proj2 #movie

plot(proj_net_documentary$proj1)
plot(proj_net_documentary$proj2)

ecount(proj_net_documentary$proj1)
mean(degree(proj_net_documentary$proj1))
graph.density(proj_net_documentary$proj1, loops = FALSE)
transitivity(proj_net_documentary$proj1, type = "average")

ggraph(proj_net_documentary$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 2)+
  theme_graph()

#Drama
proj_net_drama <- bipartite.projection(netdrama)

plot(netdrama, 
     main = "Bipartite Network for layer Drama",
     layout=-layout.bipartite(netdrama)[,2:1])

#para ver as redes one mode
proj_net_drama$proj1 #user
proj_net_drama$proj2 #movie

plot(proj_net_drama$proj1)
plot(proj_net_drama$proj2)

ecount(proj_net_drama$proj1)
mean(degree(proj_net_drama$proj1))
graph.density(proj_net_drama$proj1, loops = FALSE)
transitivity(proj_net_drama$proj1, type = "average")

ggraph(proj_net_drama$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 2)+
  theme_graph()

#Fantasy
proj_net_fantasy <- bipartite.projection(netfantasy)

plot(netfantasy, 
     main = "Bipartite Network for layer Fantasy",
     layout=-layout.bipartite(netfantasy)[,2:1])

#para ver as redes one mode
proj_net_fantasy$proj1 #user
proj_net_fantasy$proj2 #movie

plot(proj_net_fantasy$proj1)
plot(proj_net_fantasy$proj2)

ecount(proj_net_fantasy$proj1)
mean(degree(proj_net_fantasy$proj1))
graph.density(proj_net_fantasy$proj1, loops = FALSE)
transitivity(proj_net_fantasy$proj1, type = "average")

ggraph(proj_net_fantasy$proj1,layout = "sparse_stress",pivots = 100, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 2)+
  theme_graph()

#Film-Noir
proj_net_filmnoir <- bipartite.projection(netfilmnoir)

plot(netfilmnoir, 
     main = "Bipartite Network for layer Film-Noir",
     layout=-layout.bipartite(netfilmnoir)[,2:1])

#para ver as redes one mode
proj_net_filmnoir$proj1 #user
proj_net_filmnoir$proj2 #movie

plot(proj_net_filmnoir$proj1)
plot(proj_net_filmnoir$proj2)

ecount(proj_net_filmnoir$proj1)
mean(degree(proj_net_filmnoir$proj1))
graph.density(proj_net_filmnoir$proj1, loops = FALSE)
transitivity(proj_net_filmnoir$proj1, type = "average")

ggraph(proj_net_filmnoir$proj1,layout = "sparse_stress",pivots = 200, weights = NA)+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(shape = 21,fill = "grey25",size = 5)+
  theme_graph()

#Horror
proj_net_horror <- bipartite.projection(nethorror)

plot(nethorror, 
     main = "Bipartite Network for layer Horror",
     layout=-layout.bipartite(nethorror)[,2:1])

#para ver as redes one mode
proj_net_horror$proj1 #user
proj_net_horror$proj2 #movie

plot(proj_net_horror$proj1)
plot(proj_net_horror$proj2)

ecount(proj_net_horror$proj1)
mean(degree(proj_net_horror$proj1))
graph.density(proj_net_horror$proj1, loops = FALSE)
transitivity(proj_net_horror$proj1, type = "average")

#Musical
proj_net_musical <- bipartite.projection(netmusical)

plot(netmusical, 
     main = "Bipartite Network for layer Musical",
     layout=-layout.bipartite(netmusical)[,2:1])

#para ver as redes one mode
proj_net_musical$proj1 #user
proj_net_musical$proj2 #movie

plot(proj_net_musical$proj1)
plot(proj_net_musical$proj2)

ecount(proj_net_musical$proj1)
mean(degree(proj_net_musical$proj1))
graph.density(proj_net_musical$proj1, loops = FALSE)
transitivity(proj_net_musical$proj1, type = "average")

#Mystery
proj_net_mystery <- bipartite.projection(netmystery)

plot(netmystery, 
     main = "Bipartite Network for layer Mystery",
     layout=-layout.bipartite(netmystery)[,2:1])

#para ver as redes one mode
proj_net_mystery$proj1 #user
proj_net_mystery$proj2 #movie

plot(proj_net_mystery$proj1)
plot(proj_net_mystery$proj2)

ecount(proj_net_mystery$proj1)
mean(degree(proj_net_mystery$proj1))
graph.density(proj_net_mystery$proj1, loops = FALSE)
transitivity(proj_net_mystery$proj1, type = "average")

#Romance
proj_net_romance <- bipartite.projection(netromance)

plot(netromance, 
     main = "Bipartite Network for layer Romance",
     layout=-layout.bipartite(netromance)[,2:1])

#para ver as redes one mode
proj_net_romance$proj1 #user
proj_net_romance$proj2 #movie

plot(proj_net_romance$proj1)
plot(proj_net_romance$proj2)

ecount(proj_net_romance$proj1)
mean(degree(proj_net_romance$proj1))
graph.density(proj_net_romance$proj1, loops = FALSE)
transitivity(proj_net_romance$proj1, type = "average")

#Sci-Fi
proj_net_scifi <- bipartite.projection(netscifi)

plot(netscifi, 
     main = "Bipartite Network for layer Sci-Fi",
     layout=-layout.bipartite(netscifi)[,2:1])

#para ver as redes one mode
proj_net_scifi$proj1 #user
proj_net_scifi$proj2 #movie

plot(proj_net_scifi$proj1)
plot(proj_net_scifi$proj2)

ecount(proj_net_scifi$proj1)
mean(degree(proj_net_scifi$proj1))
graph.density(proj_net_scifi$proj1, loops = FALSE)
transitivity(proj_net_scifi$proj1, type = "average")

#Thriller
proj_net_thriller <- bipartite.projection(netthriller)

plot(netthriller, 
     main = "Bipartite Network for layer Thriller",
     layout=-layout.bipartite(netthriller)[,2:1])

#para ver as redes one mode
proj_net_thriller$proj1 #user
proj_net_thriller$proj2 #movie

plot(proj_net_thriller$proj1)
plot(proj_net_thriller$proj2)

ecount(proj_net_thriller$proj1)
mean(degree(proj_net_thriller$proj1))
graph.density(proj_net_thriller$proj1, loops = FALSE)
transitivity(proj_net_thriller$proj1, type = "average")

#War
proj_net_war <- bipartite.projection(netwar)

plot(netwar, 
     main = "Bipartite Network for layer War",
     layout=-layout.bipartite(netwar)[,2:1])

#para ver as redes one mode
proj_net_war$proj1 #user
proj_net_war$proj2 #movie

plot(proj_net_war$proj1)
plot(proj_net_war$proj2)

ecount(proj_net_war$proj1)
mean(degree(proj_net_war$proj1))
graph.density(proj_net_war$proj1, loops = FALSE)
transitivity(proj_net_war$proj1, type = "average")

#Western
proj_net_western <- bipartite.projection(netwestern)

plot(netwestern, 
     main = "Bipartite Network for layer Western",
     layout=-layout.bipartite(netwestern)[,2:1])

#para ver as redes one mode
proj_net_western$proj1 #user
proj_net_western$proj2 #movie

plot(proj_net_western$proj1)
plot(proj_net_western$proj2)

ecount(proj_net_western$proj1)
mean(degree(proj_net_western$proj1))
graph.density(proj_net_western$proj1, loops = FALSE)
transitivity(proj_net_western$proj1, type = "average")


#Adjacency matrices
adjaction <- as_adjacency_matrix(proj_net_action$proj1)
adjadventure <- as_adjacency_matrix(proj_net_adventure$proj1)
adjanimation <- as_adjacency_matrix(proj_net_animation$proj1)
adjchildren <- as_adjacency_matrix(proj_net_children$proj1)
adjcomedy <- as_adjacency_matrix(proj_net_comedy$proj1)
adjcrime <- as_adjacency_matrix(proj_net_crime$proj1)
adjdocumentary <- as_adjacency_matrix(proj_net_documentary$proj1)
adjdrama <- as_adjacency_matrix(proj_net_drama$proj1)
adjfantasy <- as_adjacency_matrix(proj_net_fantasy$proj1)
adjfilmnoir <- as_adjacency_matrix(proj_net_filmnoir$proj1)
adjhorror <- as_adjacency_matrix(proj_net_horror$proj1)
adjmusical <- as_adjacency_matrix(proj_net_musical$proj1)
adjmystery <- as_adjacency_matrix(proj_net_mystery$proj1)
adjromance <- as_adjacency_matrix(proj_net_romance$proj1)
adjscifi <- as_adjacency_matrix(proj_net_scifi$proj1)
adjthriller <- as_adjacency_matrix(proj_net_thriller$proj1)
adjwar <- as_adjacency_matrix(proj_net_war$proj1)
adjwestern <- as_adjacency_matrix(proj_net_western$proj1)


###interlayer similarity
#edge betweeness of each one of the layers
eblaction <- betweenness(proj_net_action$proj1)
nLaction <- names(eblaction)
ebladventure <- betweenness(proj_net_adventure$proj1)
nLadventure <- names(ebladventure)
eblanimation <- betweenness(proj_net_animation$proj1)
nLanimation <- names(eblanimation)
eblchildren <- betweenness(proj_net_children$proj1)
nLchildren <- names(eblchildren)
eblcomedy <- betweenness(proj_net_comedy$proj1)
nLcomedy <- names(eblcomedy)
eblcrime <- betweenness(proj_net_crime$proj1)
nLcrime <- names(eblcrime)
ebldocumentary <- betweenness(proj_net_documentary$proj1)
nLdocumentary <- names(ebldocumentary)
ebldrama <- betweenness(proj_net_drama$proj1)
nLdrama <- names(ebldrama)
eblfantasy <- betweenness(proj_net_fantasy$proj1)
nLfantasy <- names(eblfantasy)
eblfilmnoir <- betweenness(proj_net_filmnoir$proj1)
nLfilmnoir <- names(eblfilmnoir)
eblhorror <- betweenness(proj_net_horror$proj1)
nLhorror <- names(eblhorror)
eblmusical <- betweenness(proj_net_musical$proj1)
nLmusical <- names(eblmusical)
eblmystery <- betweenness(proj_net_mystery$proj1)
nLmystery <- names(eblmystery)
eblromance <- betweenness(proj_net_romance$proj1)
nLromance <- names(eblromance)
eblscifi <- betweenness(proj_net_scifi$proj1)
nLscifi <- names(eblscifi)
eblthriller <- betweenness(proj_net_thriller$proj1)
nLthriller <- names(eblthriller)
eblwar <- betweenness(proj_net_war$proj1)
nLwar <- names(eblwar)
eblwestern <- betweenness(proj_net_western$proj1)
nLwestern <- names(eblwestern)

###########LM -> Crime#####################
#to guarantee that both matrices have only the nodes in common
#manipulation of the matrices of the betweeness of layers Lm and Laction 
eblactiont <- eblaction[names(eblcrime)]
eblactionf <- eblactiont[!is.na(eblactiont)]
eblcrimef1 <- eblcrime[names(eblactionf)]

#manipulation of the matrices of the betweeness of layers Lm and Ladventure 
ebladventuret <- ebladventure[names(eblcrime)]
ebladventuref <- ebladventuret[!is.na(ebladventuret)]
eblcrimef2 <- eblcrime[names(ebladventuref)]

#manipulation of the matrices of the betweeness of layers Lm and Lanimation 
eblanimationt <- eblanimation[names(eblcrime)]
eblanimationf <- eblanimationt[!is.na(eblanimationt)]
eblcrimef3 <- eblcrime[names(eblanimationf)]

#manipulation of the matrices of the betweeness of layers Lm and Lchildren
eblchildrent <- eblchildren[names(eblcrime)]
eblchildrenf <- eblchildrent[!is.na(eblchildrent)]
eblcrimef4 <- eblcrime[names(eblchildrenf)]

#manipulation of the matrices of the betweeness of layers Lm and Lcomedy
eblcomedyt <- eblcomedy[names(eblcrime)]
eblcomedyf <- eblcomedyt[!is.na(eblcomedyt)]
eblcrimef5 <- eblcrime[names(eblcomedyf)]

#manipulation of the matrices of the betweeness of layers Lm and Lcrime
#eblcrimet <- eblcrime[names(eblcrime)]
#eblcrimef <- eblcrimet[!is.na(eblcrimet)]
#eblcrimef6 <- eblm[names(eblcrimef)]

#manipulation of the matrices of the betweeness of layers Lm and Ldocumentary
ebldocumentaryt <- ebldocumentary[names(eblcrime)]
ebldocumentaryf <- ebldocumentaryt[!is.na(ebldocumentaryt)]
eblcrimef7 <- eblcrime[names(ebldocumentaryf)]

#manipulation of the matrices of the betweeness of layers Lm and Ldrama
ebldramat <- ebldrama[names(eblcrime)]
ebldramaf <- ebldramat[!is.na(ebldramat)]
eblcrimef8 <- eblcrime[names(ebldramaf)]

#manipulation of the matrices of the betweeness of layers Lm and Lfantasy
eblfantasyt <- eblfantasy[names(eblcrime)]
eblfantasyf <- eblfantasyt[!is.na(eblfantasyt)]
eblcrimef9 <- eblcrime[names(eblfantasyf)]

#manipulation of the matrices of the betweeness of layers Lm and Lfilm-noir
eblfilmnoirt <- eblfilmnoir[names(eblcrime)]
eblfilmnoirf <- eblfilmnoirt[!is.na(eblfilmnoirt)]
eblcrimef10 <- eblcrime[names(eblfilmnoirf)]

#manipulation of the matrices of the betweeness of layers Lm and Lhorror
eblhorrort <- eblhorror[names(eblcrime)]
eblhorrorf <- eblhorrort[!is.na(eblhorrort)]
eblcrimef11 <- eblcrime[names(eblhorrorf)]

#manipulation of the matrices of the betweeness of layers Lm and Lmusical
eblmusicalt <- eblmusical[names(eblcrime)]
eblmusicalf <- eblmusicalt[!is.na(eblmusicalt)]
eblcrimef12 <- eblcrime[names(eblmusicalf)]

#manipulation of the matrices of the betweeness of layers Lm and Lmystery
eblmysteryt <- eblmystery[names(eblcrime)]
eblmysteryf <- eblmysteryt[!is.na(eblmysteryt)]
eblcrimef13 <- eblcrime[names(eblmysteryf)]

#manipulation of the matrices of the betweeness of layers Lm and Lromance
eblromancet <- eblromance[names(eblcrime)]
eblromancef <- eblromancet[!is.na(eblromancet)]
eblcrimef14 <- eblcrime[names(eblromancef)]

#manipulation of the matrices of the betweeness of layers Lm and Lsci-fi
eblscifit <- eblscifi[names(eblcrime)]
eblscifif <- eblscifit[!is.na(eblscifit)]
eblcrimef15 <- eblcrime[names(eblscifif)]

#manipulation of the matrices of the betweeness of layers Lm and Lthriller
eblthrillert <- eblthriller[names(eblcrime)]
eblthrillerf <- eblthrillert[!is.na(eblthrillert)]
eblcrimef16 <- eblcrime[names(eblthrillerf)]

#manipulation of the matrices of the betweeness of layers Lm and Lwar
eblwart <- eblwar[names(eblcrime)]
eblwarf <- eblwart[!is.na(eblwart)]
eblcrimef17 <- eblcrime[names(eblwarf)]

#manipulation of the matrices of the betweeness of layers Lm and Lwestern
eblwesternt <- eblwestern[names(eblcrime)]
eblwesternf <- eblwesternt[!is.na(eblwesternt)]
eblcrimef18 <- eblcrime[names(eblwesternf)]


#to have the betweeness accross the two layers need to subtract the values for each pair of nodes
#action
dbwipaction <- matrix(0, nrow = length(eblcrimef1), ncol = length(eblactionf))
for (i in 1:length(eblcrimef1)) {
  for (j in 1:length(eblactionf)) {
    dbwipaction[i,j] <- abs((eblcrimef1)[i] - (eblactionf)[j])
  } 
}

#to guarantee that which matrix index corresponds to which "userId"
rownames(dbwipaction) <- names(eblcrimef1)
colnames(dbwipaction) <- names(eblactionf)

sbwipaction <- abs(1 - dbwipaction)

#adventure
dbwipadventure <- matrix(0, nrow = length(eblcrimef2), ncol = length(ebladventuref))
for (i in 1:length(eblcrimef2)) {
  for (j in 1:length(ebladventuref)) {
    dbwipadventure[i,j] <- abs((eblcrimef2)[i] - (ebladventuref)[j])
  } 
}

rownames(dbwipadventure) <- names(eblcrimef2)
colnames(dbwipadventure) <- names(ebladventuref)

sbwipadventure <- abs(1 - dbwipadventure)

#animation
dbwipanimation <- matrix(0, nrow = length(eblcrimef3), ncol = length(eblanimationf))
for (i in 1:length(eblcrimef3)) {
  for (j in 1:length(eblanimationf)) {
    dbwipanimation[i,j] <- abs((eblcrimef3)[i] - (eblanimationf)[j])
  } 
}

rownames(dbwipanimation) <- names(eblcrimef3)
colnames(dbwipanimation) <- names(eblanimationf)

sbwipanimation <- abs(1 - dbwipanimation)

#children
dbwipchildren <- matrix(0, nrow = length(eblcrimef4), ncol = length(eblchildrenf))
for (i in 1:length(eblcrimef4)) {
  for (j in 1:length(eblchildrenf)) {
    dbwipchildren[i,j] <- abs((eblcrimef4)[i] - (eblchildrenf)[j])
  } 
}

rownames(dbwipchildren) <- names(eblcrimef4)
colnames(dbwipchildren) <- names(eblchildrenf)

sbwipchildren <- abs(1 - dbwipchildren)

#comedy
dbwipcomedy <- matrix(0, nrow = length(eblcrimef5), ncol = length(eblcomedyf))
for (i in 1:length(eblcrimef5)) {
  for (j in 1:length(eblcomedyf)) {
    dbwipcomedy[i,j] <- abs((eblcrimef5)[i] - (eblcomedyf)[j])
  } 
}

rownames(dbwipcomedy) <- names(eblcrimef5)
colnames(dbwipcomedy) <- names(eblcomedyf)

sbwipcomedy <- abs(1 - dbwipcomedy)

#crime
#dbwipcrime <- matrix(0, nrow = length(eblcrimef6), ncol = length(eblcrimef))
#for (i in 1:length(eblcrimef6)) {
#  for (j in 1:length(eblcrimef)) {
#    dbwipcrime[i,j] <- abs((eblcrimef6)[i] - (eblcrimef)[j])
#  } 
#}

#rownames(dbwipcrime) <- names(eblcrimef6)
#colnames(dbwipcrime) <- names(eblcrimef)

#sbwipcrime <- abs(1 - dbwipcrime)

#documentary
dbwipdocumentary <- matrix(0, nrow = length(eblcrimef7), ncol = length(ebldocumentaryf))
for (i in 1:length(eblcrimef7)) {
  for (j in 1:length(ebldocumentaryf)) {
    dbwipdocumentary[i,j] <- abs((eblcrimef7)[i] - (ebldocumentaryf)[j])
  } 
}

rownames(dbwipdocumentary) <- names(eblcrimef7)
colnames(dbwipdocumentary) <- names(ebldocumentaryf)

sbwipdocumentary <- abs(1 - dbwipdocumentary)

#drama
dbwipdrama <- matrix(0, nrow = length(eblcrimef8), ncol = length(ebldramaf))
for (i in 1:length(eblcrimef8)) {
  for (j in 1:length(ebldramaf)) {
    dbwipdrama[i,j] <- abs((eblcrimef8)[i] - (ebldramaf)[j])
  } 
}

rownames(dbwipdrama) <- names(eblcrimef8)
colnames(dbwipdrama) <- names(ebldramaf)

sbwipdrama <- abs(1 - dbwipdrama)

#fantasy
dbwipfantasy <- matrix(0, nrow = length(eblcrimef9), ncol = length(eblfantasyf))
for (i in 1:length(eblcrimef9)) {
  for (j in 1:length(eblfantasyf)) {
    dbwipfantasy[i,j] <- abs((eblcrimef9)[i] - (eblfantasyf)[j])
  } 
}

rownames(dbwipfantasy) <- names(eblcrimef9)
colnames(dbwipfantasy) <- names(eblfantasyf)

sbwipfantasy <- abs(1 - dbwipfantasy)

#film-noir
dbwipfilmnoir <- matrix(0, nrow = length(eblcrimef10), ncol = length(eblfilmnoirf))
for (i in 1:length(eblcrimef10)) {
  for (j in 1:length(eblfilmnoirf)) {
    dbwipfilmnoir[i,j] <- abs((eblcrimef10)[i] - (eblfilmnoirf)[j])
  } 
}

rownames(dbwipfilmnoir) <- names(eblcrimef10)
colnames(dbwipfilmnoir) <- names(eblfilmnoirf)

sbwipfilmnoir <- abs(1 - dbwipfilmnoir)

#horror
dbwiphorror <- matrix(0, nrow = length(eblcrimef11), ncol = length(eblhorrorf))
for (i in 1:length(eblcrimef11)) {
  for (j in 1:length(eblhorrorf)) {
    dbwiphorror[i,j] <- abs((eblcrimef11)[i] - (eblhorrorf)[j])
  } 
}

rownames(dbwiphorror) <- names(eblcrimef11)
colnames(dbwiphorror) <- names(eblhorrorf)

sbwiphorror <- abs(1 - dbwiphorror)

#musical
dbwipmusical <- matrix(0, nrow = length(eblcrimef12), ncol = length(eblmusicalf))
for (i in 1:length(eblcrimef12)) {
  for (j in 1:length(eblmusicalf)) {
    dbwipmusical[i,j] <- abs((eblcrimef12)[i] - (eblmusicalf)[j])
  } 
}

rownames(dbwipmusical) <- names(eblcrimef12)
colnames(dbwipmusical) <- names(eblmusicalf)

sbwipmusical <- abs(1 - dbwipmusical)

#mystery
dbwipmystery <- matrix(0, nrow = length(eblcrimef13), ncol = length(eblmysteryf))
for (i in 1:length(eblcrimef13)) {
  for (j in 1:length(eblmysteryf)) {
    dbwipmystery[i,j] <- abs((eblcrimef13)[i] - (eblmysteryf)[j])
  } 
}

rownames(dbwipmystery) <- names(eblcrimef13)
colnames(dbwipmystery) <- names(eblmysteryf)

sbwipmystery <- abs(1 - dbwipmystery)

#romance
dbwipromance <- matrix(0, nrow = length(eblcrimef14), ncol = length(eblromancef))
for (i in 1:length(eblcrimef14)) {
  for (j in 1:length(eblromancef)) {
    dbwipromance[i,j] <- abs((eblcrimef14)[i] - (eblromancef)[j])
  } 
}

rownames(dbwipromance) <- names(eblcrimef14)
colnames(dbwipromance) <- names(eblromancef)

sbwipromance <- abs(1 - dbwipromance)

#sci-fi
dbwipscifi <- matrix(0, nrow = length(eblcrimef15), ncol = length(eblscifif))
for (i in 1:length(eblcrimef15)) {
  for (j in 1:length(eblscifif)) {
    dbwipscifi[i,j] <- abs((eblcrimef15)[i] - (eblscifif)[j])
  } 
}

rownames(dbwipscifi) <- names(eblcrimef15)
colnames(dbwipscifi) <- names(eblscifif)

sbwipscifi <- abs(1 - dbwipscifi)

#thriller
dbwipthriller <- matrix(0, nrow = length(eblcrimef16), ncol = length(eblthrillerf))
for (i in 1:length(eblcrimef16)) {
  for (j in 1:length(eblthrillerf)) {
    dbwipthriller[i,j] <- abs((eblcrimef16)[i] - (eblthrillerf)[j])
  } 
}

rownames(dbwipthriller) <- names(eblcrimef16)
colnames(dbwipthriller) <- names(eblthrillerf)

sbwipthriller <- abs(1 - dbwipthriller)

#war
dbwipwar <- matrix(0, nrow = length(eblcrimef17), ncol = length(eblwarf))
for (i in 1:length(eblcrimef17)) {
  for (j in 1:length(eblwarf)) {
    dbwipwar[i,j] <- abs((eblcrimef17)[i] - (eblwarf)[j])
  } 
}

rownames(dbwipwar) <- names(eblcrimef17)
colnames(dbwipwar) <- names(eblwarf)

sbwipwar <- abs(1 - dbwipwar)

#western
dbwipwestern <- matrix(0, nrow = length(eblcrimef18), ncol = length(eblwesternf))
for (i in 1:length(eblcrimef18)) {
  for (j in 1:length(eblwesternf)) {
    dbwipwestern[i,j] <- abs((eblcrimef18)[i] - (eblwesternf)[j])
  } 
}

rownames(dbwipwestern) <- names(eblcrimef18)
colnames(dbwipwestern) <- names(eblwesternf)

sbwipwestern <- abs(1 - dbwipwestern)


#intralayer similarity
#Jaccard index for each layer
jacaction <- similarity(proj_net_action$proj1, vids = V(proj_net_action$proj1), method = "jaccard")
jacadventure <- similarity(proj_net_adventure$proj1, vids = V(proj_net_adventure$proj1), method = "jaccard")
jacanimation <- similarity(proj_net_animation$proj1, vids = V(proj_net_animation$proj1), method = "jaccard")
jacchildren <- similarity(proj_net_children$proj1, vids = V(proj_net_children$proj1), method = "jaccard")
jaccomedy <- similarity(proj_net_comedy$proj1, vids = V(proj_net_comedy$proj1), method = "jaccard")
jaccrime <- similarity(proj_net_crime$proj1, vids = V(proj_net_crime$proj1), method = "jaccard")
jacdocumentary <- similarity(proj_net_documentary$proj1, vids = V(proj_net_documentary$proj1), method = "jaccard")
jacdrama <- similarity(proj_net_drama$proj1, vids = V(proj_net_drama$proj1), method = "jaccard")
jacfantasy <- similarity(proj_net_fantasy$proj1, vids = V(proj_net_fantasy$proj1), method = "jaccard")
jacfilmnoir <- similarity(proj_net_filmnoir$proj1, vids = V(proj_net_filmnoir$proj1), method = "jaccard")
jachorror <- similarity(proj_net_horror$proj1, vids = V(proj_net_horror$proj1), method = "jaccard")
jacmusical <- similarity(proj_net_musical$proj1, vids = V(proj_net_musical$proj1), method = "jaccard")
jacmystery <- similarity(proj_net_mystery$proj1, vids = V(proj_net_mystery$proj1), method = "jaccard")
jacromance <- similarity(proj_net_romance$proj1, vids = V(proj_net_romance$proj1), method = "jaccard")
jacscifi <- similarity(proj_net_scifi$proj1, vids = V(proj_net_scifi$proj1), method = "jaccard")
jacthriller <- similarity(proj_net_thriller$proj1, vids = V(proj_net_thriller$proj1), method = "jaccard")
jacwar <- similarity(proj_net_war$proj1, vids = V(proj_net_war$proj1), method = "jaccard")
jacwestern <- similarity(proj_net_western$proj1, vids = V(proj_net_western$proj1), method = "jaccard")


#to guarantee that which matrix index corresponds to which "userId"
#Laction
colnames(jacaction) <- names(proj_net_action$proj1[[]])
rownames(jacaction) <- names(proj_net_action$proj1[[]])
#Ladventure
colnames(jacadventure) <- names(proj_net_adventure$proj1[[]])
rownames(jacadventure) <- names(proj_net_adventure$proj1[[]])
#Lanimation
colnames(jacanimation) <- names(proj_net_animation$proj1[[]])
rownames(jacanimation) <- names(proj_net_animation$proj1[[]])
#Lchildren
colnames(jacchildren) <- names(proj_net_children$proj1[[]])
rownames(jacchildren) <- names(proj_net_children$proj1[[]])
#Lcomedy
colnames(jaccomedy) <- names(proj_net_comedy$proj1[[]])
rownames(jaccomedy) <- names(proj_net_comedy$proj1[[]])
#Lcrime
colnames(jaccrime) <- names(proj_net_crime$proj1[[]])
rownames(jaccrime) <- names(proj_net_crime$proj1[[]])
#Ldocumentary
colnames(jacdocumentary) <- names(proj_net_documentary$proj1[[]])
rownames(jacdocumentary) <- names(proj_net_documentary$proj1[[]])
#Ldrama
colnames(jacdrama) <- names(proj_net_drama$proj1[[]])
rownames(jacdrama) <- names(proj_net_drama$proj1[[]])
#Lfantasy
colnames(jacfantasy) <- names(proj_net_fantasy$proj1[[]])
rownames(jacfantasy) <- names(proj_net_fantasy$proj1[[]])
#Lfilmnoir
colnames(jacfilmnoir) <- names(proj_net_filmnoir$proj1[[]])
rownames(jacfilmnoir) <- names(proj_net_filmnoir$proj1[[]])
#Lhorror
colnames(jachorror) <- names(proj_net_horror$proj1[[]])
rownames(jachorror) <- names(proj_net_horror$proj1[[]])
#Lmusical
colnames(jacmusical) <- names(proj_net_musical$proj1[[]])
rownames(jacmusical) <- names(proj_net_musical$proj1[[]])
#Lmystery
colnames(jacmystery) <- names(proj_net_mystery$proj1[[]])
rownames(jacmystery) <- names(proj_net_mystery$proj1[[]])
#Lromance
colnames(jacromance) <- names(proj_net_romance$proj1[[]])
rownames(jacromance) <- names(proj_net_romance$proj1[[]])
#Lsci-fi
colnames(jacscifi) <- names(proj_net_scifi$proj1[[]])
rownames(jacscifi) <- names(proj_net_scifi$proj1[[]])
#Lthriller
colnames(jacthriller) <- names(proj_net_thriller$proj1[[]])
rownames(jacthriller) <- names(proj_net_thriller$proj1[[]])
#Lwar
colnames(jacwar) <- names(proj_net_war$proj1[[]])
rownames(jacwar) <- names(proj_net_war$proj1[[]])
#Lwestern
colnames(jacwestern) <- names(proj_net_western$proj1[[]])
rownames(jacwestern) <- names(proj_net_western$proj1[[]])


#select only the values of the nodes common to both layers
#Laction
jacactionf <- jacaction[names(eblcrimef1),names(eblactionf)]
jaccrimef1 <- jaccrime[names(eblcrimef1),names(eblactionf)]

#Ladventure
jacadventuref <- jacadventure[names(eblcrimef2),names(ebladventuref)]
jaccrimef2 <- jaccrime[names(eblcrimef2),names(ebladventuref)]

#Lanimation
jacanimationf <- jacanimation[names(eblcrimef3),names(eblanimationf)]
jaccrimef3 <- jaccrime[names(eblcrimef3),names(eblanimationf)]

#Lchildren
jacchildrenf <- jacchildren[names(eblcrimef4),names(eblchildrenf)]
jaccrimef4 <- jaccrime[names(eblcrimef4),names(eblchildrenf)]

#Lcomedy
jaccomedyf <- jaccomedy[names(eblcrimef5),names(eblcomedyf)]
jaccrimef5 <- jaccrime[names(eblcrimef5),names(eblcomedyf)]

#Lcrime
#jaccrimef <- jaccrime[names(eblcrimef6),names(eblcrimef)]
#jaccrimef6 <- jaccrime[names(eblcrimef6),names(eblcrimef)]

#Ldocumentary
jacdocumentaryf <- jacdocumentary[names(eblcrimef7),names(ebldocumentaryf)]
jaccrimef7 <- jaccrime[names(eblcrimef7),names(ebldocumentaryf)]

#Ldrama
jacdramaf <- jacdrama[names(eblcrimef8),names(ebldramaf)]
jaccrimef8 <- jaccrime[names(eblcrimef8),names(ebldramaf)]

#Lfantasy
jacfantasyf <- jacfantasy[names(eblcrimef9),names(eblfantasyf)]
jaccrimef9 <- jaccrime[names(eblcrimef9),names(eblfantasyf)]

#Lfilmnoir
jacfilmnoirf <- jacfilmnoir[names(eblcrimef10),names(eblfilmnoirf)]
jaccrimef10 <- jaccrime[names(eblcrimef10),names(eblfilmnoirf)]

#Lhorror
jachorrorf <- jachorror[names(eblcrimef11),names(eblhorrorf)]
jaccrimef11 <- jaccrime[names(eblcrimef11),names(eblhorrorf)]

#Lmusical
jacmusicalf <- jacmusical[names(eblcrimef12),names(eblmusicalf)]
jaccrimef12 <- jaccrime[names(eblcrimef12),names(eblmusicalf)]

#Lmystery
jacmysteryf <- jacmystery[names(eblcrimef13),names(eblmysteryf)]
jaccrimef13 <- jaccrime[names(eblcrimef13),names(eblmysteryf)]

#Lromance
jacromancef <- jacromance[names(eblcrimef14),names(eblromancef)]
jaccrimef14 <- jaccrime[names(eblcrimef14),names(eblromancef)]

#Lscifi
jacscifif <- jacscifi[names(eblcrimef15),names(eblscifif)]
jaccrimef15 <- jaccrime[names(eblcrimef15),names(eblscifif)]

#Lthriller
jacthrillerf <- jacthriller[names(eblcrimef16),names(eblthrillerf)]
jaccrimef16 <- jaccrime[names(eblcrimef16),names(eblthrillerf)]

#Lwar
jacwarf <- jacwar[names(eblcrimef17),names(eblwarf)]
jaccrimef17 <- jaccrime[names(eblcrimef17),names(eblwarf)]

#Lwestern
jacwesternf <- jacwestern[names(eblcrimef18),names(eblwesternf)]
jaccrimef18 <- jaccrime[names(eblcrimef18),names(eblwesternf)]


#select only the values of the nodes common to both layers
#Laction
adjactionf <- adjaction[names(eblcrimef1), names(eblactionf)]
adjcrimef1 <- adjcrime[names(eblcrimef1), names(eblactionf)]

#Ladventure
adjadventuref <- adjadventure[names(eblcrimef2), names(ebladventuref)]
adjcrimef2 <- adjcrime[names(eblcrimef2), names(ebladventuref)]

#Lanimation
adjanimationf <- adjanimation[names(eblcrimef3), names(eblanimationf)]
adjcrimef3 <- adjcrime[names(eblcrimef3), names(eblanimationf)]

#Lchildren
adjchildrenf <- adjchildren[names(eblcrimef4), names(eblchildrenf)]
adjcrimef4 <- adjcrime[names(eblcrimef4), names(eblchildrenf)]

#Lcomedy
adjcomedyf <- adjcomedy[names(eblcrimef5), names(eblcomedyf)]
adjcrimef5 <- adjcrime[names(eblcrimef5), names(eblcomedyf)]

#Lcrime
#adjcrimef <- adjcrime[names(eblcrimef6), names(eblcrimef)]
#adjcrimef6 <- adjcrime[names(eblcrimef6), names(eblcrimef)]

#Ldocumentary
adjdocumentaryf <- adjdocumentary[names(eblcrimef7), names(ebldocumentaryf)]
adjcrimef7 <- adjcrime[names(eblcrimef7), names(ebldocumentaryf)]

#Ldrama
adjdramaf <- adjdrama[names(eblcrimef8), names(ebldramaf)]
adjcrimef8 <- adjcrime[names(eblcrimef8), names(ebldramaf)]

#Lfantasy
adjfantasyf <- adjfantasy[names(eblcrimef9), names(eblfantasyf)]
adjcrimef9 <- adjcrime[names(eblcrimef9), names(eblfantasyf)]

#Lfilmnoir
adjfilmnoirf <- adjfilmnoir[names(eblcrimef10), names(eblfilmnoirf)]
adjcrimef10 <- adjcrime[names(eblcrimef10), names(eblfilmnoirf)]

#Lhorror
adjhorrorf <- adjhorror[names(eblcrimef11), names(eblhorrorf)]
adjcrimef11 <- adjcrime[names(eblcrimef11), names(eblhorrorf)]

#Lmusical
adjmusicalf <- adjmusical[names(eblcrimef12), names(eblmusicalf)]
adjcrimef12 <- adjcrime[names(eblcrimef12), names(eblmusicalf)]

#Lmystery
adjmysteryf <- adjmystery[names(eblcrimef13), names(eblmysteryf)]
adjcrimef13 <- adjcrime[names(eblcrimef13), names(eblmysteryf)]

#Lromance
adjromancef <- adjromance[names(eblcrimef14), names(eblromancef)]
adjcrimef14 <- adjcrime[names(eblcrimef14), names(eblromancef)]

#Lscifi
adjscifif <- adjscifi[names(eblcrimef15), names(eblscifif)]
adjcrimef15 <- adjcrime[names(eblcrimef15), names(eblscifif)]

#Lthriller
adjthrillerf <- adjthriller[names(eblcrimef16), names(eblthrillerf)]
adjcrimef16 <- adjcrime[names(eblcrimef16), names(eblthrillerf)]

#Lwar
adjwarf <- adjwar[names(eblcrimef17), names(eblwarf)]
adjcrimef17 <- adjcrime[names(eblcrimef17), names(eblwarf)]

#Lwestern
adjwesternf <- adjwestern[names(eblcrimef18), names(eblwesternf)]
adjcrimef18 <- adjcrime[names(eblcrimef18), names(eblwesternf)]


#####################
#p inter -> link existence probabilities using interlayer features for crime layer using action layer
p_inter_LcrimeLaction <- matrix(0, nrow = length(eblcrimef1), ncol = length(eblactionf))
for (i in 1:length(eblcrimef1)) {
  for (j in 1:length(eblactionf)) {
    if(adjactionf[i,j] == 1){
      p_inter_LcrimeLaction[i,j] <- jacactionf[i,j] * sbwipaction[i,j]
    } else {
      p_inter_LcrimeLaction[i,j] <- (1 - jacactionf[i,j]) * (1 - sbwipaction[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLaction) <- names(eblactionf)
rownames(p_inter_LcrimeLaction) <- names(eblcrimef1)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLaction <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLaction) <- names(eblcrime)
colnames(zeroLcrimeLaction) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLaction), rownames(p_inter_LcrimeLaction))
dfcols <- setdiff(colnames(zeroLcrimeLaction), rownames(p_inter_LcrimeLaction))
rownames(zeroLcrimeLaction) <- c(rownames(p_inter_LcrimeLaction), dfrows)
colnames(zeroLcrimeLaction) <- c(colnames(p_inter_LcrimeLaction), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLaction[1:nrow(p_inter_LcrimeLaction), 1:ncol(p_inter_LcrimeLaction)] <- p_inter_LcrimeLaction

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLaction <- zeroLcrimeLaction[names(eblcrime), names(eblcrime)]

#######################################

#p inter -> link existence probabilities using interlayer features for crime layer using adventure layer
p_inter_LcrimeLadventure <- matrix(0, nrow = length(eblcrimef2), ncol = length(ebladventuref))
for (i in 1:length(eblcrimef2)) {
  for (j in 1:length(ebladventuref)) {
    if(adjadventuref[i,j] == 1){
      p_inter_LcrimeLadventure[i,j] <- jacadventuref[i,j] * sbwipadventure[i,j]
    } else {
      p_inter_LcrimeLadventure[i,j] <- (1 - jacadventuref[i,j]) * (1 - sbwipadventure[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLadventure) <- names(ebladventuref)
rownames(p_inter_LcrimeLadventure) <- names(eblcrimef2)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLadventure <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLadventure) <- names(eblcrime)
colnames(zeroLcrimeLadventure) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLadventure), rownames(p_inter_LcrimeLadventure))
dfcols <- setdiff(colnames(zeroLcrimeLadventure), rownames(p_inter_LcrimeLadventure))
rownames(zeroLcrimeLadventure) <- c(rownames(p_inter_LcrimeLadventure), dfrows)
colnames(zeroLcrimeLadventure) <- c(colnames(p_inter_LcrimeLadventure), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLadventure[1:nrow(p_inter_LcrimeLadventure), 1:ncol(p_inter_LcrimeLadventure)] <- p_inter_LcrimeLadventure

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLadventure <- zeroLcrimeLadventure[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using animation layer
p_inter_LcrimeLanimation <- matrix(0, nrow = length(eblcrimef3), ncol = length(eblanimationf))
for (i in 1:length(eblcrimef3)) {
  for (j in 1:length(eblanimationf)) {
    if(adjanimationf[i,j] == 1){
      p_inter_LcrimeLanimation[i,j] <- jacanimationf[i,j] * sbwipanimation[i,j]
    } else {
      p_inter_LcrimeLanimation[i,j] <- (1 - jacanimationf[i,j]) * (1 - sbwipanimation[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLanimation) <- names(eblanimationf)
rownames(p_inter_LcrimeLanimation) <- names(eblcrimef3)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLanimation <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLanimation) <- names(eblcrime)
colnames(zeroLcrimeLanimation) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLanimation), rownames(p_inter_LcrimeLanimation))
dfcols <- setdiff(colnames(zeroLcrimeLanimation), rownames(p_inter_LcrimeLanimation))
rownames(zeroLcrimeLanimation) <- c(rownames(p_inter_LcrimeLanimation), dfrows)
colnames(zeroLcrimeLanimation) <- c(colnames(p_inter_LcrimeLanimation), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLanimation[1:nrow(p_inter_LcrimeLanimation), 1:ncol(p_inter_LcrimeLanimation)] <- p_inter_LcrimeLanimation

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLanimation <- zeroLcrimeLanimation[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using children layer
p_inter_LcrimeLchildren <- matrix(0, nrow = length(eblcrimef4), ncol = length(eblchildrenf))
for (i in 1:length(eblcrimef4)) {
  for (j in 1:length(eblchildrenf)) {
    if(adjchildrenf[i,j] == 1){
      p_inter_LcrimeLchildren[i,j] <- jacchildrenf[i,j] * sbwipchildren[i,j]
    } else {
      p_inter_LcrimeLchildren[i,j] <- (1 - jacchildrenf[i,j]) * (1 - sbwipchildren[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLchildren) <- names(eblchildrenf)
rownames(p_inter_LcrimeLchildren) <- names(eblcrimef4)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLchildren <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLchildren) <- names(eblcrime)
colnames(zeroLcrimeLchildren) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLchildren), rownames(p_inter_LcrimeLchildren))
dfcols <- setdiff(colnames(zeroLcrimeLchildren), rownames(p_inter_LcrimeLchildren))
rownames(zeroLcrimeLchildren) <- c(rownames(p_inter_LcrimeLchildren), dfrows)
colnames(zeroLcrimeLchildren) <- c(colnames(p_inter_LcrimeLchildren), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLchildren[1:nrow(p_inter_LcrimeLchildren), 1:ncol(p_inter_LcrimeLchildren)] <- p_inter_LcrimeLchildren

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLchildren <- zeroLcrimeLchildren[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using comedy layer
p_inter_LcrimeLcomedy <- matrix(0, nrow = length(eblcrimef5), ncol = length(eblcomedyf))
for (i in 1:length(eblcrimef5)) {
  for (j in 1:length(eblcomedyf)) {
    if(adjcomedyf[i,j] == 1){
      p_inter_LcrimeLcomedy[i,j] <- jaccomedyf[i,j] * sbwipcomedy[i,j]
    } else {
      p_inter_LcrimeLcomedy[i,j] <- (1 - jaccomedyf[i,j]) * (1 - sbwipcomedy[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLcomedy) <- names(eblcomedyf)
rownames(p_inter_LcrimeLcomedy) <- names(eblcrimef5)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLcomedy <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLcomedy) <- names(eblcrime)
colnames(zeroLcrimeLcomedy) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLcomedy), rownames(p_inter_LcrimeLcomedy))
dfcols <- setdiff(colnames(zeroLcrimeLcomedy), rownames(p_inter_LcrimeLcomedy))
rownames(zeroLcrimeLcomedy) <- c(rownames(p_inter_LcrimeLcomedy), dfrows)
colnames(zeroLcrimeLcomedy) <- c(colnames(p_inter_LcrimeLcomedy), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLcomedy[1:nrow(p_inter_LcrimeLcomedy), 1:ncol(p_inter_LcrimeLcomedy)] <- p_inter_LcrimeLcomedy

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLcomedy <- zeroLcrimeLcomedy[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using documentary layer
p_inter_LcrimeLdocumentary <- matrix(0, nrow = length(eblcrimef7), ncol = length(ebldocumentaryf))
for (i in 1:length(eblcrimef7)) {
  for (j in 1:length(ebldocumentaryf)) {
    if(adjdocumentaryf[i,j] == 1){
      p_inter_LcrimeLdocumentary[i,j] <- jacdocumentaryf[i,j] * sbwipdocumentary[i,j]
    } else {
      p_inter_LcrimeLdocumentary[i,j] <- (1 - jacdocumentaryf[i,j]) * (1 - sbwipdocumentary[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLdocumentary) <- names(ebldocumentaryf)
rownames(p_inter_LcrimeLdocumentary) <- names(eblcrimef7)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLdocumentary <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLdocumentary) <- names(eblcrime)
colnames(zeroLcrimeLdocumentary) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLdocumentary), rownames(p_inter_LcrimeLdocumentary))
dfcols <- setdiff(colnames(zeroLcrimeLdocumentary), rownames(p_inter_LcrimeLdocumentary))
rownames(zeroLcrimeLdocumentary) <- c(rownames(p_inter_LcrimeLdocumentary), dfrows)
colnames(zeroLcrimeLdocumentary) <- c(colnames(p_inter_LcrimeLdocumentary), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLdocumentary[1:nrow(p_inter_LcrimeLdocumentary), 1:ncol(p_inter_LcrimeLdocumentary)] <- p_inter_LcrimeLdocumentary

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLdocumentary <- zeroLcrimeLdocumentary[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using drama layer
p_inter_LcrimeLdrama <- matrix(0, nrow = length(eblcrimef8), ncol = length(ebldramaf))
for (i in 1:length(eblcrimef8)) {
  for (j in 1:length(ebldramaf)) {
    if(adjdramaf[i,j] == 1){
      p_inter_LcrimeLdrama[i,j] <- jacdramaf[i,j] * sbwipdrama[i,j]
    } else {
      p_inter_LcrimeLdrama[i,j] <- (1 - jacdramaf[i,j]) * (1 - sbwipdrama[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLdrama) <- names(ebldramaf)
rownames(p_inter_LcrimeLdrama) <- names(eblcrimef8)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLdrama <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLdrama) <- names(eblcrime)
colnames(zeroLcrimeLdrama) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLdrama), rownames(p_inter_LcrimeLdrama))
dfcols <- setdiff(colnames(zeroLcrimeLdrama), rownames(p_inter_LcrimeLdrama))
rownames(zeroLcrimeLdrama) <- c(rownames(p_inter_LcrimeLdrama), dfrows)
colnames(zeroLcrimeLdrama) <- c(colnames(p_inter_LcrimeLdrama), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLdrama[1:nrow(p_inter_LcrimeLdrama), 1:ncol(p_inter_LcrimeLdrama)] <- p_inter_LcrimeLdrama

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLdrama <- zeroLcrimeLdrama[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using fantasy layer
p_inter_LcrimeLfantasy <- matrix(0, nrow = length(eblcrimef9), ncol = length(eblfantasyf))
for (i in 1:length(eblcrimef9)) {
  for (j in 1:length(eblfantasyf)) {
    if(adjfantasyf[i,j] == 1){
      p_inter_LcrimeLfantasy[i,j] <- jacfantasyf[i,j] * sbwipfantasy[i,j]
    } else {
      p_inter_LcrimeLfantasy[i,j] <- (1 - jacfantasyf[i,j]) * (1 - sbwipfantasy[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLfantasy) <- names(eblfantasyf)
rownames(p_inter_LcrimeLfantasy) <- names(eblcrimef9)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLfantasy <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLfantasy) <- names(eblcrime)
colnames(zeroLcrimeLfantasy) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLfantasy), rownames(p_inter_LcrimeLfantasy))
dfcols <- setdiff(colnames(zeroLcrimeLfantasy), rownames(p_inter_LcrimeLfantasy))
rownames(zeroLcrimeLfantasy) <- c(rownames(p_inter_LcrimeLfantasy), dfrows)
colnames(zeroLcrimeLfantasy) <- c(colnames(p_inter_LcrimeLfantasy), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLfantasy[1:nrow(p_inter_LcrimeLfantasy), 1:ncol(p_inter_LcrimeLfantasy)] <- p_inter_LcrimeLfantasy

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLfantasy <- zeroLcrimeLfantasy[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using filmnoir layer
p_inter_LcrimeLfilmnoir <- matrix(0, nrow = length(eblcrimef10), ncol = length(eblfilmnoirf))
for (i in 1:length(eblcrimef10)) {
  for (j in 1:length(eblfilmnoirf)) {
    if(adjfilmnoirf[i,j] == 1){
      p_inter_LcrimeLfilmnoir[i,j] <- jacfilmnoirf[i,j] * sbwipfilmnoir[i,j]
    } else {
      p_inter_LcrimeLfilmnoir[i,j] <- (1 - jacfilmnoirf[i,j]) * (1 - sbwipfilmnoir[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLfilmnoir) <- names(eblfilmnoirf)
rownames(p_inter_LcrimeLfilmnoir) <- names(eblcrimef10)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLfilmnoir <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLfilmnoir) <- names(eblcrime)
colnames(zeroLcrimeLfilmnoir) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLfilmnoir), rownames(p_inter_LcrimeLfilmnoir))
dfcols <- setdiff(colnames(zeroLcrimeLfilmnoir), rownames(p_inter_LcrimeLfilmnoir))
rownames(zeroLcrimeLfilmnoir) <- c(rownames(p_inter_LcrimeLfilmnoir), dfrows)
colnames(zeroLcrimeLfilmnoir) <- c(colnames(p_inter_LcrimeLfilmnoir), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLfilmnoir[1:nrow(p_inter_LcrimeLfilmnoir), 1:ncol(p_inter_LcrimeLfilmnoir)] <- p_inter_LcrimeLfilmnoir

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLfilmnoir <- zeroLcrimeLfilmnoir[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using horror layer
p_inter_LcrimeLhorror <- matrix(0, nrow = length(eblcrimef11), ncol = length(eblhorrorf))
for (i in 1:length(eblcrimef11)) {
  for (j in 1:length(eblhorrorf)) {
    if(adjhorrorf[i,j] == 1){
      p_inter_LcrimeLhorror[i,j] <- jachorrorf[i,j] * sbwiphorror[i,j]
    } else {
      p_inter_LcrimeLhorror[i,j] <- (1 - jachorrorf[i,j]) * (1 - sbwiphorror[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLhorror) <- names(eblhorrorf)
rownames(p_inter_LcrimeLhorror) <- names(eblcrimef11)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLhorror <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLhorror) <- names(eblcrime)
colnames(zeroLcrimeLhorror) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLhorror), rownames(p_inter_LcrimeLhorror))
dfcols <- setdiff(colnames(zeroLcrimeLhorror), rownames(p_inter_LcrimeLhorror))
rownames(zeroLcrimeLhorror) <- c(rownames(p_inter_LcrimeLhorror), dfrows)
colnames(zeroLcrimeLhorror) <- c(colnames(p_inter_LcrimeLhorror), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLhorror[1:nrow(p_inter_LcrimeLhorror), 1:ncol(p_inter_LcrimeLhorror)] <- p_inter_LcrimeLhorror

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLhorror <- zeroLcrimeLhorror[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using musical layer
p_inter_LcrimeLmusical <- matrix(0, nrow = length(eblcrimef12), ncol = length(eblmusicalf))
for (i in 1:length(eblcrimef12)) {
  for (j in 1:length(eblmusicalf)) {
    if(adjmusicalf[i,j] == 1){
      p_inter_LcrimeLmusical[i,j] <- jacmusicalf[i,j] * sbwipmusical[i,j]
    } else {
      p_inter_LcrimeLmusical[i,j] <- (1 - jacmusicalf[i,j]) * (1 - sbwipmusical[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLmusical) <- names(eblmusicalf)
rownames(p_inter_LcrimeLmusical) <- names(eblcrimef12)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLmusical <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLmusical) <- names(eblcrime)
colnames(zeroLcrimeLmusical) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLmusical), rownames(p_inter_LcrimeLmusical))
dfcols <- setdiff(colnames(zeroLcrimeLmusical), rownames(p_inter_LcrimeLmusical))
rownames(zeroLcrimeLmusical) <- c(rownames(p_inter_LcrimeLmusical), dfrows)
colnames(zeroLcrimeLmusical) <- c(colnames(p_inter_LcrimeLmusical), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLmusical[1:nrow(p_inter_LcrimeLmusical), 1:ncol(p_inter_LcrimeLmusical)] <- p_inter_LcrimeLmusical

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLmusical <- zeroLcrimeLmusical[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using mystery layer
p_inter_LcrimeLmystery <- matrix(0, nrow = length(eblcrimef13), ncol = length(eblmysteryf))
for (i in 1:length(eblcrimef13)) {
  for (j in 1:length(eblmysteryf)) {
    if(adjmysteryf[i,j] == 1){
      p_inter_LcrimeLmystery[i,j] <- jacmysteryf[i,j] * sbwipmystery[i,j]
    } else {
      p_inter_LcrimeLmystery[i,j] <- (1 - jacmysteryf[i,j]) * (1 - sbwipmystery[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLmystery) <- names(eblmysteryf)
rownames(p_inter_LcrimeLmystery) <- names(eblcrimef13)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLmystery <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLmystery) <- names(eblcrime)
colnames(zeroLcrimeLmystery) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLmystery), rownames(p_inter_LcrimeLmystery))
dfcols <- setdiff(colnames(zeroLcrimeLmystery), rownames(p_inter_LcrimeLmystery))
rownames(zeroLcrimeLmystery) <- c(rownames(p_inter_LcrimeLmystery), dfrows)
colnames(zeroLcrimeLmystery) <- c(colnames(p_inter_LcrimeLmystery), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLmystery[1:nrow(p_inter_LcrimeLmystery), 1:ncol(p_inter_LcrimeLmystery)] <- p_inter_LcrimeLmystery

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLmystery <- zeroLcrimeLmystery[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using romance layer
p_inter_LcrimeLromance <- matrix(0, nrow = length(eblcrimef14), ncol = length(eblromancef))
for (i in 1:length(eblcrimef14)) {
  for (j in 1:length(eblromancef)) {
    if(adjromancef[i,j] == 1){
      p_inter_LcrimeLromance[i,j] <- jacromancef[i,j] * sbwipromance[i,j]
    } else {
      p_inter_LcrimeLromance[i,j] <- (1 - jacromancef[i,j]) * (1 - sbwipromance[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLromance) <- names(eblromancef)
rownames(p_inter_LcrimeLromance) <- names(eblcrimef14)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLromance <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLromance) <- names(eblcrime)
colnames(zeroLcrimeLromance) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLromance), rownames(p_inter_LcrimeLromance))
dfcols <- setdiff(colnames(zeroLcrimeLromance), rownames(p_inter_LcrimeLromance))
rownames(zeroLcrimeLromance) <- c(rownames(p_inter_LcrimeLromance), dfrows)
colnames(zeroLcrimeLromance) <- c(colnames(p_inter_LcrimeLromance), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLromance[1:nrow(p_inter_LcrimeLromance), 1:ncol(p_inter_LcrimeLromance)] <- p_inter_LcrimeLromance

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLromance <- zeroLcrimeLromance[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using scifi layer
p_inter_LcrimeLscifi <- matrix(0, nrow = length(eblcrimef15), ncol = length(eblscifif))
for (i in 1:length(eblcrimef15)) {
  for (j in 1:length(eblscifif)) {
    if(adjscifif[i,j] == 1){
      p_inter_LcrimeLscifi[i,j] <- jacscifif[i,j] * sbwipscifi[i,j]
    } else {
      p_inter_LcrimeLscifi[i,j] <- (1 - jacscifif[i,j]) * (1 - sbwipscifi[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLscifi) <- names(eblscifif)
rownames(p_inter_LcrimeLscifi) <- names(eblcrimef15)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLscifi <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLscifi) <- names(eblcrime)
colnames(zeroLcrimeLscifi) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLscifi), rownames(p_inter_LcrimeLscifi))
dfcols <- setdiff(colnames(zeroLcrimeLscifi), rownames(p_inter_LcrimeLscifi))
rownames(zeroLcrimeLscifi) <- c(rownames(p_inter_LcrimeLscifi), dfrows)
colnames(zeroLcrimeLscifi) <- c(colnames(p_inter_LcrimeLscifi), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLscifi[1:nrow(p_inter_LcrimeLscifi), 1:ncol(p_inter_LcrimeLscifi)] <- p_inter_LcrimeLscifi

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLscifi <- zeroLcrimeLscifi[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using thriller layer
p_inter_LcrimeLthriller <- matrix(0, nrow = length(eblcrimef16), ncol = length(eblthrillerf))
for (i in 1:length(eblcrimef16)) {
  for (j in 1:length(eblthrillerf)) {
    if(adjthrillerf[i,j] == 1){
      p_inter_LcrimeLthriller[i,j] <- jacthrillerf[i,j] * sbwipthriller[i,j]
    } else {
      p_inter_LcrimeLthriller[i,j] <- (1 - jacthrillerf[i,j]) * (1 - sbwipthriller[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLthriller) <- names(eblthrillerf)
rownames(p_inter_LcrimeLthriller) <- names(eblcrimef16)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLthriller <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLthriller) <- names(eblcrime)
colnames(zeroLcrimeLthriller) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLthriller), rownames(p_inter_LcrimeLthriller))
dfcols <- setdiff(colnames(zeroLcrimeLthriller), rownames(p_inter_LcrimeLthriller))
rownames(zeroLcrimeLthriller) <- c(rownames(p_inter_LcrimeLthriller), dfrows)
colnames(zeroLcrimeLthriller) <- c(colnames(p_inter_LcrimeLthriller), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLthriller[1:nrow(p_inter_LcrimeLthriller), 1:ncol(p_inter_LcrimeLthriller)] <- p_inter_LcrimeLthriller

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLthriller <- zeroLcrimeLthriller[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using war layer
p_inter_LcrimeLwar <- matrix(0, nrow = length(eblcrimef17), ncol = length(eblwarf))
for (i in 1:length(eblcrimef17)) {
  for (j in 1:length(eblwarf)) {
    if(adjwarf[i,j] == 1){
      p_inter_LcrimeLwar[i,j] <- jacwarf[i,j] * sbwipwar[i,j]
    } else {
      p_inter_LcrimeLwar[i,j] <- (1 - jacwarf[i,j]) * (1 - sbwipwar[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLwar) <- names(eblwarf)
rownames(p_inter_LcrimeLwar) <- names(eblcrimef17)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLwar <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLwar) <- names(eblcrime)
colnames(zeroLcrimeLwar) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLwar), rownames(p_inter_LcrimeLwar))
dfcols <- setdiff(colnames(zeroLcrimeLwar), rownames(p_inter_LcrimeLwar))
rownames(zeroLcrimeLwar) <- c(rownames(p_inter_LcrimeLwar), dfrows)
colnames(zeroLcrimeLwar) <- c(colnames(p_inter_LcrimeLwar), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLwar[1:nrow(p_inter_LcrimeLwar), 1:ncol(p_inter_LcrimeLwar)] <- p_inter_LcrimeLwar

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLwar <- zeroLcrimeLwar[names(eblcrime), names(eblcrime)]

###################

#p inter -> link existence probabilities using interlayer features for crime layer using western layer
p_inter_LcrimeLwestern <- matrix(0, nrow = length(eblcrimef18), ncol = length(eblwesternf))
for (i in 1:length(eblcrimef18)) {
  for (j in 1:length(eblwesternf)) {
    if(adjwesternf[i,j] == 1){
      p_inter_LcrimeLwestern[i,j] <- jacwesternf[i,j] * sbwipwestern[i,j]
    } else {
      p_inter_LcrimeLwestern[i,j] <- (1 - jacwesternf[i,j]) * (1 - sbwipwestern[i,j])  
    }
  }
}

#with the "userId" as the matrix rows and columns names
colnames(p_inter_LcrimeLwestern) <- names(eblwesternf)
rownames(p_inter_LcrimeLwestern) <- names(eblcrimef18)

#matrix with zeros to project the matrix size to the size of layer LM with the respective rows and columns names
zeroLcrimeLwestern <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
rownames(zeroLcrimeLwestern) <- names(eblcrime)
colnames(zeroLcrimeLwestern) <- names(eblcrime)

#gives the names of the rows and columns according to Lm nodes
dfrows <- setdiff(rownames(zeroLcrimeLwestern), rownames(p_inter_LcrimeLwestern))
dfcols <- setdiff(colnames(zeroLcrimeLwestern), rownames(p_inter_LcrimeLwestern))
rownames(zeroLcrimeLwestern) <- c(rownames(p_inter_LcrimeLwestern), dfrows)
colnames(zeroLcrimeLwestern) <- c(colnames(p_inter_LcrimeLwestern), dfcols)

#expands the matrix with zeros for the nodes that Lm has that are not on Lk
zeroLcrimeLwestern[1:nrow(p_inter_LcrimeLwestern), 1:ncol(p_inter_LcrimeLwestern)] <- p_inter_LcrimeLwestern

#puts the values in the same order of matrix eblm, for the addition in the end
p_inter_LcrimeLwestern <- zeroLcrimeLwestern[names(eblcrime), names(eblcrime)]

###############

#adds the values from all the layers to have the final interlayer probability
p_inter_Lm <- p_inter_LcrimeLaction + p_inter_LcrimeLadventure + p_inter_LcrimeLanimation + p_inter_LcrimeLchildren + p_inter_LcrimeLcomedy + p_inter_LcrimeLdocumentary + p_inter_LcrimeLdrama + p_inter_LcrimeLfantasy + p_inter_LcrimeLfilmnoir + p_inter_LcrimeLhorror + p_inter_LcrimeLmusical + p_inter_LcrimeLmystery + p_inter_LcrimeLromance + p_inter_LcrimeLscifi + p_inter_LcrimeLthriller + p_inter_LcrimeLwar + p_inter_LcrimeLwestern

#################


#Normalization of the link existence probability using interlayer features
Np_inter_Lm <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
for (i in 1:length(eblcrime)) {
  for (j in 1:length(eblcrime)) {
    Np_inter_Lm[i,j] <- p_inter_Lm[i,j]/max(p_inter_Lm)
  }
}

#gives the same names as the final interlayer probability matrix
rownames(Np_inter_Lm) <- rownames(p_inter_Lm)
colnames(Np_inter_Lm) <- colnames(p_inter_Lm)

#Synthetizer to combine intralayer and interlayer information to produce meaningful information for the link prediction task
alfa <- 0.4
p_total_Lm <- matrix(0, nrow = length(eblcrime), ncol = length(eblcrime))
for (i in 1:length(eblcrime)) {
  for (j in 1:length(eblcrime)) {
    p_total_Lm[i,j] <- (1 - alfa) * jaccrime[i,j] + alfa * Np_inter_Lm[i,j]
  }
}

#gives the same names as the final interlayer probability matrix
rownames(p_total_Lm) <- rownames(p_inter_Lm)
colnames(p_total_Lm) <- colnames(p_inter_Lm)

###########
#test graph

testadjcrime <- adjcrime
#gives the indices to be removed from the adjacency matrix
ind <- sample(1:length(adjcrime[]==1), floor(0.3*length(adjcrime[]==1)))
#removes them setting the value to zero
testadjcrime[ind] <- 0

#plot(graph.adjacency(as.matrix(testadjm), mode = "undirected"))



#defines the link existence by setting the threshold on the 0.7 probability
for (i in 1:length(eblcrime)) {
  for (j in 1:length(eblcrime)) {
    if(p_total_Lm[i,j] >= 0.3){
      testadjcrime[i,j] <- 1}
  }
}


#final <- p_total_Lm[p_total_Lm>=0.7]
#write.csv(p_total_Lm, "p_total_lm0.7.csv")

#dadjcrime <- as.matrix(adjcrime)
#write.csv(dadjcrime, "dadjcrime.csv")
#dtestadjcrime <- as.matrix(testadjcrime)

#transforms the matrix into a numeric vector for the ROC function
prev <- c(as.numeric(testadjcrime))
obs <- c(as.numeric(adjcrime))

#creates the ROC plot
roc.lp <- plot.roc(obs, prev)
#computes the AUC value
areauc <- auc(obs, prev)
print(areauc)

#F1 score
xtab <- table(prev, obs)

#F1a <- F1_Score(prev,obs)

F1b <- f1(prev,obs)
print(F1b)

#prec <- Precision(prev, obs)

#rec <- recall(prev, obs)

#cm <- ConfusionMatrix(prev, obs)

confm <- confusionMatrix(as.factor(prev), as.factor(obs), positive = "1")
print(confm)

filter(dfcrime, userId == "68" & rating > 3.5)