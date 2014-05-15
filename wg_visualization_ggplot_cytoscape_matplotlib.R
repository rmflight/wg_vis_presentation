## ----setup, echo=FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(dev='CairoPNG')

saveCytoscapeSVG <- function(before, options, envir){
  heightStr <- ""
  widthStr <- ""
  if (!is.null(options$out.height)){
    heightStr <- paste('height="', options$out.height, '"', sep="")
  }
  if (!is.null(options$out.width)){
    widthStr <- paste('width="', options$out.width, '"', sep="")
  }
  if (!before){
    if (exists("cw")){
      svgFile <- paste(options$label, ".svg", sep="")
      imgURL <- paste("figure", svgFile, sep="/")
      savePath <- file.path(getwd(), "figure", svgFile)
      saveImage(cw, savePath, "svg")
      paste('<img src="', imgURL, '" title="', options$label, '" alt="', options$label, '" ', heightStr, ' ', widthStr, '/>', sep="")
    }
  }
}

knit_hooks$set(saveCytoscape=saveCytoscapeSVG)


## ----scatter2------------------------------------------------------------
var1 <- rnorm(100)
var2 <- rnorm(100, 1)
plot(var1, var2)


## ----histogram-----------------------------------------------------------
hist(var1)
hist(var2)


## ----tworeps-------------------------------------------------------------
varData <- data.frame(var1=c(rnorm(100), rnorm(100)), var2=c(rnorm(100, 1), rnorm(100,1)), sample=rep(c(1,2), each=100))


## ----plotRepsBase--------------------------------------------------------
plot(varData$var1, varData$var2)


## ----plotRepsBasebySample------------------------------------------------
plot(varData$var1[varData$sample==1], varData$var2[varData$sample==1], col="blue")
points(varData$var1[varData$sample==2], varData$var2[varData$sample==2], col="red")


## ----plotRepsSampleGGplot------------------------------------------------
library(ggplot2)
varData$sample <- factor(varData$sample)
ggplot(varData, aes(x=var1, y=var2, colour=sample)) + geom_point()


## ----plotsampleOut-------------------------------------------------------
ggplot(varData, aes(x=var1, y=var2)) + geom_point() + facet_grid(. ~ sample)


## ----plotHistggplot------------------------------------------------------
ggplot(varData, aes(x=var1, fill=sample)) + geom_bar(position="identity", alpha=0.5)
ggplot(varData, aes(x=var1, fill=sample)) + geom_density(alpha=0.5)
ggplot(varData, aes(x=var1)) + geom_bar() + facet_grid(sample ~ .)


## ----plotSummary---------------------------------------------------------
library(reshape2)
varData2 <- melt(varData, measure.vars=c("var1", "var2"))

ggplot(varData2, aes(x=value, fill=sample)) + geom_bar(position="identity", alpha=0.5) + facet_grid(. ~ variable)
ggplot(varData2, aes(x=value)) + geom_bar() + facet_grid(sample ~ variable)


## ----layers--------------------------------------------------------------
p <- ggplot(varData, aes(x=var1, y=var2))
p + geom_point()
p + stat_smooth()
p + geom_point() + stat_smooth()


## ----customizePoints-----------------------------------------------------
p + geom_point(color="red")


## ----axisLabels----------------------------------------------------------
p + geom_point() + labs(x="Important Data 1", y="Dependent Variable 1")


## ----theme---------------------------------------------------------------
pPoint <- p + geom_point()
pPoint + theme(axis.text = element_text(colour = "red"))
pPoint + theme(axis.text = element_text(size = 14))
pPoint + theme(axis.title = element_text(color = "blue", size = 20))


## ----loadData------------------------------------------------------------
metabData <- read.table("pumpkin_tomatillo_data.csv",
                        sep=",", header=T, stringsAsFactors=FALSE)
colnames(metabData)[4:151] <- seq(1, 148)

metab2 <- melt(metabData, id.vars=seq(1,3), measure.vars=seq(4, 151))
varNameData <- read.table("metabID.csv", sep=",", header=T, stringsAsFactors=FALSE)

metab2$metabName <- rep(varNameData$name, each=24)
metabData <- metab2
newTreat <- rep("fresh", nrow(metabData))
newTreat[grep("lyophilized", metabData$Treatment)] <- "lyoph"
metabData$treat <- newTreat
head(metabData)


## ----outputData, eval=FALSE----------------------------------------------
## write.table(metabData, file="metabolomics_reshapedData.csv", sep=",", row.names=FALSE, col.names=TRUE)


## ----viewHistograms------------------------------------------------------
ggplot(metabData, aes(x=value, fill=treat)) + geom_bar(alpha=0.8, position="identity") + facet_grid(Species ~ .) + xlim(0, 5000)

ggplot(metabData, aes(x=value, fill=Species)) + geom_bar(alpha=0.8, position="identity") + facet_grid(treat ~ .) + xlim(0, 5000)


## ----viewDensity---------------------------------------------------------
ggplot(metabData, aes(x=value, fill=treat)) + geom_density(alpha=0.8) + facet_grid(Species ~ ., scales="free_y") + xlim(0, 5000)
ggplot(metabData, aes(x=value, fill=Species)) + geom_density(alpha=0.8) + facet_grid(treat ~ ., scales="free_y") + xlim(0, 5000)


## ----plotAll-------------------------------------------------------------
ggplot(metabData, aes(x=metabName, y=value, fill=Species)) + geom_boxplot()


## ----topChangers---------------------------------------------------------
topChangers <- c(34, 62, 8, 17, 124, 100, 115, 9, 93, 123, 75, 71, 85, 33)
metabTop <- metabData[(metabData$variable %in% topChangers),]
ggplot(metabTop, aes(x=metabName, y=value, fill=Species)) + geom_boxplot()
ggplot(metabTop, aes(x=metabName, y=value, fill=Species)) + geom_boxplot() + scale_y_log10()


## ----topChangersPiecemeal------------------------------------------------
g <- ggplot(metabTop, aes(x=metabName, y=value, fill=Species)) + geom_boxplot()
print(g)
g + scale_y_log10()


## ----loadNetworkData-----------------------------------------------------
library(graph)
nodeEdges <- read.table("biochemEdgeList.csv", header=T, sep=",", stringsAsFactors=FALSE)

# need character for node IDs
nodeEdges[,1] <- as.character(nodeEdges[,1])
nodeEdges[,2] <- as.character(nodeEdges[,2])


## ----createGraph---------------------------------------------------------
allNodes <- unique(c(nodeEdges$source, nodeEdges$target))

metabNetwork <- graphNEL(nodes=allNodes, edgemode="directed")
metabNetwork <- addEdge(nodeEdges$source, nodeEdges$target, metabNetwork, nodeEdges$weight)


## ----nodeProperties------------------------------------------------------
nodeData <- read.table("node_attributes.csv", header=T, sep=",", stringsAsFactors=FALSE)
nodeData$Index <- as.character(nodeData$Index)

nodeDataDefaults(metabNetwork, "name") <- ""
attr(nodeDataDefaults(metabNetwork, "name"), "class") <- "STRING"
nodeDataDefaults(metabNetwork, "direction") <- ""
attr(nodeDataDefaults(metabNetwork, "direction"), "class") <- "STRING"
nodeDataDefaults(metabNetwork, "sig.dir") <- ""
attr(nodeDataDefaults(metabNetwork, "sig.dir"), "class") <- "STRING"
nodeDataDefaults(metabNetwork, "size.FC") <- 0
attr(nodeDataDefaults(metabNetwork, "size.FC"), "class") <- "DOUBLE"
nodeDataDefaults(metabNetwork, "size.log.FC") <- 0
attr(nodeDataDefaults(metabNetwork, "size.log.FC"), "class") <- "DOUBLE"
nodeDataDefaults(metabNetwork, "size.LV") <- 0
attr(nodeDataDefaults(metabNetwork, "size.LV"), "class") <- "DOUBLE"
nodeDataDefaults(metabNetwork, "log.mean.Pumpkin") <- 0
attr(nodeDataDefaults(metabNetwork, "log.mean.Pumpkin"), "class") <- "DOUBLE"
nodeDataDefaults(metabNetwork, "log.mean.Tomatillo") <- 0
attr(nodeDataDefaults(metabNetwork, "log.mean.Tomatillo"), "class") <- "DOUBLE"

edgeDataDefaults(metabNetwork, "weight") <- 0
attr(edgeDataDefaults(metabNetwork, "weight"), "class") <- "DOUBLE"
edgeDataDefaults(metabNetwork, "source") <- ""
attr(edgeDataDefaults(metabNetwork, "source"), "class") <- "STRING"

edgeData(metabNetwork, nodeEdges[,1], nodeEdges[,2], "source") <- nodeEdges$type


useNodes <- nodeData$Index %in% nodes(metabNetwork)
for (useAttr in c("name", "direction", "sig.dir", "size.FC", "size.log.FC", "size.LV", "log.mean.Pumpkin", "log.mean.Tomatillo")){
  
  nodeData(metabNetwork, nodeData$Index[useNodes], useAttr) <- nodeData[useNodes, useAttr]
}


## ----loadCytoscape, saveCytoscape=TRUE, out.width="800px"----------------
library(RCytoscape)
cw <- new.CytoscapeWindow("metabolite similarity", graph=metabNetwork)
displayGraph(cw)


## ----layoutNetwork, saveCytoscape=TRUE, out.width="800px"----------------
setLayoutProperties(cw, 'force-directed', list(edge_attribute='weight'))
layoutNetwork(cw, 'force-directed')


## ----changeEdgeColors, saveCytoscape=TRUE, out.width="800px"-------------
edgeColors <- c('#000000', '#999999')
setEdgeColorRule(cw, 'source', c('KEGG', 'Tanimoto'), edgeColors, mode='lookup')
redraw(cw)


## ----changeNodeColors, saveCytoscape=TRUE, out.width="800px"-------------
nodeColors <- c('#99FF00', '#FF9900')
setNodeColorRule(cw, 'direction', c('increase', 'decrease'), nodeColors, mode='lookup')
redraw(cw)


## ----changeLabel, saveCytoscape=TRUE, out.width="800px"------------------
setNodeLabelRule(cw, 'name')
redraw(cw)


## ----selectHiFC, saveCytoscape=TRUE, out.width="800px"-------------------
hiFCNodes <- nodeData$Index[abs(nodeData$size.log.FC) >= 1.2]
selectNodes(cw, hiFCNodes)


## ----selectHiFCNeighbors, saveCytoscape=TRUE, out.width="800px"----------
selectFirstNeighborsOfSelectedNodes(cw)
hiFCNodesNeighbors <- getSelectedNodes(cw)
hiFCNodesNeighborsData <- nodeData[nodeData$Index %in% hiFCNodesNeighbors,]


## ----graphNeighborhood---------------------------------------------------
library(igraph)

tmpNetwork <- metabNetwork
edgeDataDefaults(tmpNetwork, "source") <- NULL
edgeDataDefaults(tmpNetwork, "weight") <- NULL
nodeData(tmpNetwork, nodes(tmpNetwork), "name") <- nodes(tmpNetwork)
igNetwork <- igraph.from.graphNEL(tmpNetwork, name=TRUE)
igNetwork <- as.undirected(igNetwork)
eb <- edge.betweenness.community(igNetwork)

eb


## ----examineCommunities--------------------------------------------------
allComms <- split(eb$names, eb$membership)
allComms <- allComms[order(sapply(allComms, length), decreasing=T)]


## ----community1, saveCytoscape=TRUE, out.width="800px"-------------------
selectNodes(cw, allComms[[1]], preserve.current.selection=FALSE)


## ----community2, saveCytoscape=TRUE, out.width="800px"-------------------
selectNodes(cw, allComms[[2]], preserve.current.selection=FALSE)


## ----community3, saveCytoscape=TRUE, out.width="800px"-------------------
selectNodes(cw, allComms[[3]], preserve.current.selection=FALSE)


## ----community4, saveCytoscape=TRUE, out.width="800px"-------------------
selectNodes(cw, allComms[[4]], preserve.current.selection=FALSE)


## ------------------------------------------------------------------------
deleteWindow(cw)


## ------------------------------------------------------------------------
library(Cairo)


## ----, eval=FALSE--------------------------------------------------------
## CairoPNG(file="testPNG.png")
## plot(var1, var2)
## dev.off()


## ----saveCytoscape, eval=FALSE-------------------------------------------
## saveImage(cw, file.name=file.path(getwd(), "testNetwork.svg"), image.type="svg")


## ------------------------------------------------------------------------
sessionInfo()


## ------------------------------------------------------------------------
purl("wg_visualization_ggplot_cytoscape_matplotlib.Rmd", quiet=TRUE)


