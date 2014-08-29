############################################################################################
# Clear workspace 
rm(list = ls())

# working directory

setwd("/Users/Jessica/Documents/Data Vis/Final Project/final/")

# Load libraries
library("igraph")
require("igraph")
  #install.packages("HiveR")
library("HiveR")
require("HiveR")
library("RColorBrewer")

############################################################################################
rm(list = ls())

# data

data <- read.csv("output.csv")

library("dplyr")
require("dplyr")

df <- tbl_df(data)

# working with factors, just to be clear

df_factor <- df

# clean-up

df_factor$party <- NULL

df_factor$sourceID <- NULL

df_factor$id <- NULL

# creating the adjacency matrix from df_factor; this is from an igraph package

g <- get.adjacency(graph.edgelist(as.matrix(df_factor)))

g_adj <- graph.adjacency(g, mode="directed", weighted = TRUE, diag=TRUE)

dataSet <- cbind( get.edgelist(g_adj), round( E(g_adj)$weight, 3))

gD <- simplify(graph.data.frame(dataSet, directed=TRUE))

# Print number of nodes and edges
# vcount(gD)
# ecount(gD)

# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities

# Calculate degree for all nodes
degAll <- degree(gD, v = V(gD), mode = "all")

# Calculate betweenness for all nodes
betAll <- betweenness(gD, v = V(gD), directed = TRUE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))

gD <- set.vertex.attribute(gD, "degree", index = V(gD), value = degAll)
gD <- set.vertex.attribute(gD, "betweenness", index = V(gD), value = betAll.norm)

# Check the attributes
# summary(gD)

gD <- set.edge.attribute(gD, "weight", index = E(gD), value = 0)
gD <- set.edge.attribute(gD, "similarity", index = E(gD), value = 0)

# Calculate Dice similarities between all pairs of nodes
dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")

library("plyr")
require("plyr")

# Calculate edge weight based on the node similarity
F1 <- function(x) {data.frame(V4 = dsAll[which(V(gD)$name == as.character(x$1)), which(V(gD)$name == as.character(x$2))])}
dataSet.ext <- ddply(dataSet, .variables=c(1,2,3), function(x) data.frame(F1(x)))

for (i in 1:nrow(dataSet.ext))
{
  E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$weight <- as.numeric(dataSet.ext$V3)
  E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$similarity <- as.numeric(dataSet.ext$V4)
}

rm(degAll, betAll, betAll.norm, F1, dsAll, i)
############################################################################################
#Determine node/edge color based on the properties


gAdj <- get.adjacency(g, type = "upper", edges = FALSE, names = TRUE, sparse = FALSE)

hive1 <- adj2HPD(gAdj, type="2D")

hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")

hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")

hive4 <- mineHPD(hive3, option = "remove zero edge")

plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)

############################################################################################
# Have to put in some color



# First do nodes
nodes <- hive4$nodes

# Change the node color and size based on node degree and betweenness values
for (i in 1:nrow(nodes))
{
  nodes$color[i] <- nodes_col[which(nodes$lab[i] == V(gAdj)$name)]
  nodes$size[i] <- nodes_size[which(nodes$lab[i] == V(gAdj)$name)]
}

# Reassign these nodes to the hive(4) object
hive4$nodes <- nodes

# And plot it (Figure 2)
plotHive(hive4, method = "abs", bkgnd = "white",  axLab.pos = 1)

# Now do the edges
edges <- hive4$edges

# Change the edge color based on Dice similarity
for (i in 1:nrow(edges))
{
  index1 <- which(nodes$id == edges$id1[i])
  index2 <- which(nodes$id == edges$id2[i])
  
  edges$color[i] <- edges_col[which(E(gD)[as.character(nodes$lab[index1]) %--% as.character(nodes$lab[index2])] == E(gAdj))]
}

# Reassign these edges to the hive(4) object
hive4$edges <- edges

# And plot it (Figure 3)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)

# Some edges are too thick, so we will reduce the edge weight (thickness) by 25%
hive4$edges$weight <- hive4$edges$weight/4

# And plot it (Figure 5)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)




library("plyr")
require("plyr")

# Print number of nodes and edges
# vcount(gAdj)
# ecount(gAdj)

# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities

# Calculate degree for all nodes
degAll <- degree(gAdj, v = V(gAdj), mode = "all")

# Calculate betweenness for all nodes
betAll <- betweenness(gAdj, v = V(gAdj), directed = TRUE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))

node.list <- data.frame(name = V(gAdj)$name, degree = degAll, betw = betAll.norm)

# Calculate Dice similarities between all pairs of nodes
dsAll <- similarity.dice(gAdj, vids = V(gAdj), mode = "all")

# Calculate edge weight based on the node similarity
F1 <- function(x) {data.frame(V4 = dsAll[which(V(gAdj)$name == as.character(x$name)), which(V(gAdj)$name == as.character(x$description))])}
dataSet.ext <- ddply(, .variables=c(g$name, g$description, E(g)$weight), function(x) data.frame(F1(x)))

rm(degAll, betAll, betAll.norm, F1)
############################################################################################
#Determine node/edge color based on the properties

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(node.list$bet)))
nodes_size <- sapply(node.list$bet, function(x) approxVals$y[which(sort(unique(node.list$bet)) == x)])
node.list <- cbind(node.list, size = nodes_size)
rm(approxVals, nodes_size)

# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")
# This function returns a function corresponding to a color palete of "bias" number of elements
F2 <- colorRampPalette(c("#F5DEB3", "#999999"), bias = length(unique(V(gAdj)$degree)), space = "rgb", interpolate = "linear")
# Now we'll create a color for each degree
colCodes <- F2(length(unique(V(gAdj)$degree)))
# And we will assign a color for each node based on its degree
nodes_col <- sapply(V(gAdj)$degree, function(x) colCodes[which(sort(unique(V(gAdj)$degree)) == x)])
rm(F2, colCodes)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(E(gAdj)$similarity)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(gAdj)$similarity)))
edges_col <- sapply(E(gAdj)$similarity, function(x) colCodes[which(sort(unique(E(gAdj)$similarity)) == x)])
rm(F2, colCodes)

############################################################################################
# Now the new (HiveR) part

# Create a hive plot from the data frame
hive1 <- adj2HPD(gAdj, type="2D")
#sumHPD(hive1)

# Assign nodes to a radius based on their degree (number of edges they are touching)
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")

# Assign nodes to axes based on their position in the edge list 
# (this function assumes direct graphs, so it considers the first column to be a source and second column to be a sink )
hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")

# Removing zero edges for better visualization 
hive4 <- mineHPD(hive3, option = "remove zero edge")

# And finally, plotting our graph (Figure 1)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)
