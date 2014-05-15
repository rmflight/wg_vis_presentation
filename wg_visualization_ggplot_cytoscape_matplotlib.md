



# A Review of R, Cytoscape and Python Integration for Data Visualization and Exploration

[`R`](http://cran.r-project.org/) is a popular data analysis and visualization environment, with a large number of user contributed packages, and the ability for interested individuals to easily contribute their own packages. 

[`Cytoscape`](cytoscape.org) is a popular network visualization software.

[`Python`](python.org) is an alternative popular data analysis environment.

## Why Script Data Visualization and Exploration??

There are many tools people can use to interact with their data, with Microsoft Excel (and other spreadsheet packages) being very popular. However, the number of different ways that data can be easily plotted or manipulated is very limiting in spreadsheet packages. Other commercial visualization packages such as Tableau and Spotfire are more powerful than spreadsheet software, and enable greater control, but **reproducing** a plot later can be difficult if one does not record the exact set of options and tick boxes selected in producing a visualization.

I **believe** that scripting languages such as `R` and `Python` that allow rather easy data manipulation and visualization provide the best of both worlds.

## Why R, ggplot2

`R` is useful because of the various statistical utilities available. `R` has a set of base graphics utilities that allow one to do various types of plots. As an example, we can create a scatterplot of two variables:


```r
var1 <- rnorm(100)
var2 <- rnorm(100, 1)
plot(var1, var2)
```

![plot of chunk scatter2](figure/scatter2.png) 


Or do a histogram of one of the variables:


```r
hist(var1)
```

![plot of chunk histogram](figure/histogram1.png) 

```r
hist(var2)
```

![plot of chunk histogram](figure/histogram2.png) 


But doing much else in base graphics can get rather tedious and difficult, especially when you want multiple plots.

As a simple example, lets imagine that we have two replicates of each variable that we want to examine.


```r
varData <- data.frame(var1 = c(rnorm(100), rnorm(100)), var2 = c(rnorm(100, 
    1), rnorm(100, 1)), sample = rep(c(1, 2), each = 100))
```


Using base graphics, we can easily plot the two variables against each other:


```r
plot(varData$var1, varData$var2)
```

![plot of chunk plotRepsBase](figure/plotRepsBase.png) 


But if we want to color each sample differently on the same graphic (a common enough thing), things quickly get more complicated:


```r
plot(varData$var1[varData$sample == 1], varData$var2[varData$sample == 1], col = "blue")
points(varData$var1[varData$sample == 2], varData$var2[varData$sample == 2], 
    col = "red")
```

![plot of chunk plotRepsBasebySample](figure/plotRepsBasebySample.png) 


We are no longer using the base `plot` command, but a sub-part of it. And plumbing it's depths quickly becomes tedious.

## GGPlot2

Enter [`ggplot2`](http://ggplot2.org/). It is an `R` implementation of the [*grammar of graphics*](http://www.amazon.com/Grammar-Graphics-Statistics-Computing/dp/0387245448). To better understand it's design and how it treats entries and variables, I recommend reading Hadley Wickham's [Tidy Data](http://vita.had.co.nz/papers/tidy-data.pdf) manuscript.

Using `ggplot2`, our previous graph becomes rather trivial.


```r
library(ggplot2)
varData$sample <- factor(varData$sample)
ggplot(varData, aes(x = var1, y = var2, colour = sample)) + geom_point()
```

![plot of chunk plotRepsSampleGGplot](figure/plotRepsSampleGGplot.png) 


We can also easily create multiple sub-plots that are broken out the by **sample** variable:


```r
ggplot(varData, aes(x = var1, y = var2)) + geom_point() + facet_grid(. ~ sample)
```

![plot of chunk plotsampleOut](figure/plotsampleOut.png) 



In it's current form, we can easily plot summary histograms of each variable:


```r
ggplot(varData, aes(x = var1, fill = sample)) + geom_bar(position = "identity", 
    alpha = 0.5)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk plotHistggplot](figure/plotHistggplot1.png) 

```r
ggplot(varData, aes(x = var1, fill = sample)) + geom_density(alpha = 0.5)
```

![plot of chunk plotHistggplot](figure/plotHistggplot2.png) 

```r
ggplot(varData, aes(x = var1)) + geom_bar() + facet_grid(sample ~ .)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk plotHistggplot](figure/plotHistggplot3.png) 


If we want to do a bigger overview of summary statistics, we can just reshape our data a little bit:asdfasdf


```r
library(reshape2)
varData2 <- melt(varData, measure.vars = c("var1", "var2"))

ggplot(varData2, aes(x = value, fill = sample)) + geom_bar(position = "identity", 
    alpha = 0.5) + facet_grid(. ~ variable)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk plotSummary](figure/plotSummary1.png) 

```r
ggplot(varData2, aes(x = value)) + geom_bar() + facet_grid(sample ~ variable)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk plotSummary](figure/plotSummary2.png) 


## Layers & Customization

The other major strength of `ggplot` is the ability to layer different graphical on top of each other, as well as customizing plots.

### Layers

Layers are the individual plot elements that are added to a given plot. For example, we can do:


```r
p <- ggplot(varData, aes(x = var1, y = var2))
p + geom_point()
```

![plot of chunk layers](figure/layers1.png) 

```r
p + stat_smooth()
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk layers](figure/layers2.png) 

```r
p + geom_point() + stat_smooth()
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk layers](figure/layers3.png) 


### Customization

We can customize just about any aspect of a plot that we want.

From the color of the points plotted:


```r
p + geom_point(color = "red")
```

![plot of chunk customizePoints](figure/customizePoints.png) 


To the axis labels:


```r
p + geom_point() + labs(x = "Important Data 1", y = "Dependent Variable 1")
```

![plot of chunk axisLabels](figure/axisLabels.png) 


To the fonts, background, etc, or the elements of a particular `ggplot` **theme**:


```r
pPoint <- p + geom_point()
pPoint + theme(axis.text = element_text(colour = "red"))
```

![plot of chunk theme](figure/theme1.png) 

```r
pPoint + theme(axis.text = element_text(size = 14))
```

![plot of chunk theme](figure/theme2.png) 

```r
pPoint + theme(axis.title = element_text(color = "blue", size = 20))
```

![plot of chunk theme](figure/theme3.png) 


There are a couple of built-in themes such as `theme_bw` and `theme_classic` alongside the default, and there is also a collection of alternative themes in the [`ggthemes`](https://github.com/jrnold/ggthemes) package.

## With Metabolomics Data

Metabolomics and network data are courtesy of Dmitry Grapov's [Creative Data Solutions](http://imdevsoftware.wordpress.com/2014/02/17/tutorials-statistical-and-multivariate-analysis-for-metabolomics/) metabolomic tutorial.


```r
metabData <- read.table("pumpkin_tomatillo_data.csv", sep = ",", header = T, 
    stringsAsFactors = FALSE)
colnames(metabData)[4:151] <- seq(1, 148)

metab2 <- melt(metabData, id.vars = seq(1, 3), measure.vars = seq(4, 151))
varNameData <- read.table("metabID.csv", sep = ",", header = T, stringsAsFactors = FALSE)

metab2$metabName <- rep(varNameData$name, each = 24)
metabData <- metab2
newTreat <- rep("fresh", nrow(metabData))
newTreat[grep("lyophilized", metabData$Treatment)] <- "lyoph"
metabData$treat <- newTreat
head(metabData)
```

```
##    ID Species                             Treatment variable value
## 1 P07 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1    28
## 2 P08 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1    70
## 3 P09 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1    43
## 4 P10 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1   139
## 5 P11 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1    42
## 6 P12 pumpkin MeOH:CHCl3:H2O (5:2:2) - fresh frozen        1   112
##    metabName treat
## 1 zymosterol fresh
## 2 zymosterol fresh
## 3 zymosterol fresh
## 4 zymosterol fresh
## 5 zymosterol fresh
## 6 zymosterol fresh
```



```r
write.table(metabData, file = "metabolomics_reshapedData.csv", sep = ",", row.names = FALSE, 
    col.names = TRUE)
```



Let's summarize the values for each type of sample. We will stop at a value of **5000** because 


```r
ggplot(metabData, aes(x = value, fill = treat)) + geom_bar(alpha = 0.8, position = "identity") + 
    facet_grid(Species ~ .) + xlim(0, 5000)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk viewHistograms](figure/viewHistograms1.png) 

```r

ggplot(metabData, aes(x = value, fill = Species)) + geom_bar(alpha = 0.8, position = "identity") + 
    facet_grid(treat ~ .) + xlim(0, 5000)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk viewHistograms](figure/viewHistograms2.png) 


As an alternative to histograms, `ggplot` provides smoothed density estimates that can nicely capture the **shape** of a distribution.


```r
ggplot(metabData, aes(x = value, fill = treat)) + geom_density(alpha = 0.8) + 
    facet_grid(Species ~ ., scales = "free_y") + xlim(0, 5000)
```

```
## Warning: Removed 259 rows containing non-finite values (stat_density).
## Warning: Removed 286 rows containing non-finite values (stat_density).
## Warning: Removed 356 rows containing non-finite values (stat_density).
## Warning: Removed 362 rows containing non-finite values (stat_density).
```

![plot of chunk viewDensity](figure/viewDensity1.png) 

```r
ggplot(metabData, aes(x = value, fill = Species)) + geom_density(alpha = 0.8) + 
    facet_grid(treat ~ ., scales = "free_y") + xlim(0, 5000)
```

```
## Warning: Removed 259 rows containing non-finite values (stat_density).
## Warning: Removed 356 rows containing non-finite values (stat_density).
## Warning: Removed 286 rows containing non-finite values (stat_density).
## Warning: Removed 362 rows containing non-finite values (stat_density).
```

![plot of chunk viewDensity](figure/viewDensity2.png) 


We can also do the boxplot for all the metabolites, with a separate box for each species.


```r
ggplot(metabData, aes(x = metabName, y = value, fill = Species)) + geom_boxplot()
```

![plot of chunk plotAll](figure/plotAll.png) 


This is not that easily to decipher, but it was quite easy to do.


Based on the analysis presented in the tutorial, we can pick out some of the top changers:


```r
topChangers <- c(34, 62, 8, 17, 124, 100, 115, 9, 93, 123, 75, 71, 85, 33)
metabTop <- metabData[(metabData$variable %in% topChangers), ]
ggplot(metabTop, aes(x = metabName, y = value, fill = Species)) + geom_boxplot()
```

![plot of chunk topChangers](figure/topChangers1.png) 

```r
ggplot(metabTop, aes(x = metabName, y = value, fill = Species)) + geom_boxplot() + 
    scale_y_log10()
```

![plot of chunk topChangers](figure/topChangers2.png) 


Finally, what is also quite nice about using `ggplot2` is the ability to add incrementally to a graph. In our previous example, we could have achieved the same result piecemeal like this:


```r
g <- ggplot(metabTop, aes(x = metabName, y = value, fill = Species)) + geom_boxplot()
print(g)
```

![plot of chunk topChangersPiecemeal](figure/topChangersPiecemeal1.png) 

```r
g + scale_y_log10()
```

![plot of chunk topChangersPiecemeal](figure/topChangersPiecemeal2.png) 



# Networks

Networks are very useful things in many fields, due to the fact that we can easily come up with ways to define relationships between objects. These objects can be anything, but in the context of metabolomics they are often `genes`, `proteins`, and `metabolites`. The relationships between metabolites can be defined in a couple of different ways. This example we will define edges between metabolites by links in the `KEGG` database, and by their chemical similarity (tanimoto coefficient). All data was generated by `metamapR`.

The network graph will be constructed in `R`, and visualized in `Cytoscape`. This gives us the flexibility to manipulate the graph easily in `R` and save groups of relevant nodes, with the visual richness and interactive flexibility of `Cytoscape`.

This connection of `R` with `Cytoscape` uses the [`RCytoscape`](http://rcytoscape.systemsbiology.net/versions/current/index.html) `Bioconductor` package, and requires `Cytoscape` v2.8, and the `CytoscapeRPC` plugin. There is currently no way to do this with `Cytoscape` v3.0, however given that a `REST` api is being introduced to `Cytoscape`, it should be possible in the future (if we can persuade Paul Shannon to write a new `R` package).


```r
library(graph)
nodeEdges <- read.table("biochemEdgeList.csv", header = T, sep = ",", stringsAsFactors = FALSE)

# need character for node IDs
nodeEdges[, 1] <- as.character(nodeEdges[, 1])
nodeEdges[, 2] <- as.character(nodeEdges[, 2])
```



```r
allNodes <- unique(c(nodeEdges$source, nodeEdges$target))

metabNetwork <- graphNEL(nodes = allNodes, edgemode = "directed")
metabNetwork <- addEdge(nodeEdges$source, nodeEdges$target, metabNetwork, nodeEdges$weight)
```


We also need the node and edge attributes, and to decide how they will be represented in the graph so that the types can be properly passed to Cytoscape. 


```r
nodeData <- read.table("node_attributes.csv", header = T, sep = ",", stringsAsFactors = FALSE)
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

edgeData(metabNetwork, nodeEdges[, 1], nodeEdges[, 2], "source") <- nodeEdges$type


useNodes <- nodeData$Index %in% nodes(metabNetwork)
for (useAttr in c("name", "direction", "sig.dir", "size.FC", "size.log.FC", 
    "size.LV", "log.mean.Pumpkin", "log.mean.Tomatillo")) {
    
    nodeData(metabNetwork, nodeData$Index[useNodes], useAttr) <- nodeData[useNodes, 
        useAttr]
}
```


Now that we've added all the nitty gritty details to our network, we can pass it to Cytoscape and start doing some visualization.


```r
library(RCytoscape)
```

```
## Loading required package: XMLRPC
## Note: the specification for S3 class "AsIs" in package 'XMLRPC' seems equivalent to one from package 'BiocGenerics': not turning on duplicate class definitions for this class.
```

```r
cw <- new.CytoscapeWindow("metabolite similarity", graph = metabNetwork)
displayGraph(cw)
```

```
## [1] "name"
## [1] "direction"
## [1] "sig.dir"
## [1] "size.FC"
## [1] "size.log.FC"
## [1] "size.LV"
## [1] "log.mean.Pumpkin"
## [1] "log.mean.Tomatillo"
## [1] "label"
## [1] "weight"
## [1] "source"
```

<img src="figure/loadCytoscape.svg" title="loadCytoscape" alt="loadCytoscape"  width="800px"/>


Not much to look at yet, we have to tell `Cytoscape` to layout the network.


```r
setLayoutProperties(cw, "force-directed", list(edge_attribute = "weight"))
layoutNetwork(cw, "force-directed")
```

<img src="figure/layoutNetwork.svg" title="layoutNetwork" alt="layoutNetwork"  width="800px"/>


As it stands, we can't get a whole lot of information yet from this network. But lets start adding some visual properties to it and see what happens.

## Edges

Lets change the edge visuals based on the source. Maybe black for KEGG (biochemical) and gray for Tanimoto (similarity)?


```r
edgeColors <- c("#000000", "#999999")
setEdgeColorRule(cw, "source", c("KEGG", "Tanimoto"), edgeColors, mode = "lookup")
redraw(cw)
```

<img src="figure/changeEdgeColors.svg" title="changeEdgeColors" alt="changeEdgeColors"  width="800px"/>


So now we can see that most of the edges are from the similarity measure.

## Nodes

### Colors

We have metabolites that have changes either in the up or down direction. It would be nice to at a glance see which is which.


```r
nodeColors <- c("#99FF00", "#FF9900")
setNodeColorRule(cw, "direction", c("increase", "decrease"), nodeColors, mode = "lookup")
redraw(cw)
```

<img src="figure/changeNodeColors.svg" title="changeNodeColors" alt="changeNodeColors"  width="800px"/>


### Display Labels

Currently, we have to select a node to see it's metabolite name. We can use something else as the display label.


```r
setNodeLabelRule(cw, "name")
```

```
## [1] TRUE
```

```r
redraw(cw)
```

<img src="figure/changeLabel.svg" title="changeLabel" alt="changeLabel"  width="800px"/>



## Select Nodes and Neighbors

We can select nodes based on particular properties in our original data. Such as those nodes that had large fold-changes, and their immediate neighbors:


```r
hiFCNodes <- nodeData$Index[abs(nodeData$size.log.FC) >= 1.2]
selectNodes(cw, hiFCNodes)
```

<img src="figure/selectHiFC.svg" title="selectHiFC" alt="selectHiFC"  width="800px"/>



```r
selectFirstNeighborsOfSelectedNodes(cw)
hiFCNodesNeighbors <- getSelectedNodes(cw)
hiFCNodesNeighborsData <- nodeData[nodeData$Index %in% hiFCNodesNeighbors, ]
```

<img src="figure/selectHiFCNeighbors.svg" title="selectHiFCNeighbors" alt="selectHiFCNeighbors"  width="800px"/>


## Graph Properties

We can also use other `R` packages like `igraph` to analyze our network, and select particular groups of nodes based on the computed properties. In this example we will analyze the network for communities of nodes:


```r
library(igraph)
```

```
## 
## Attaching package: 'igraph'
## 
## The following objects are masked from 'package:graph':
## 
##     degree, edges
```

```r

tmpNetwork <- metabNetwork
edgeDataDefaults(tmpNetwork, "source") <- NULL
edgeDataDefaults(tmpNetwork, "weight") <- NULL
nodeData(tmpNetwork, nodes(tmpNetwork), "name") <- nodes(tmpNetwork)
igNetwork <- igraph.from.graphNEL(tmpNetwork, name = TRUE)
igNetwork <- as.undirected(igNetwork)
eb <- edge.betweenness.community(igNetwork)

eb
```

```
## Graph community structure calculated with the edge betweenness algorithm
## Number of communities (best split): 14 
## Modularity (best split): 0.655 
## Membership vector:
##   6  11   8  30  18  25  13  29  48  94  96   9  97  98  56  95  33 108 
##   1   2   3   4   4   5   6   6   6   6   4   4   4   7   7   6   4   7 
##  88 116 117  34  22  46 131 134  14  49 115   2   3   1  16  15  35  32 
##   6   6   6   7   8   6   6   6   7   9   7   7   7   8   7  10  11  10 
##  23  45  47  51  31  59  60  63  61  66  50  71  64  72  69  77  57  70 
##  12  12  12   6   7   4   4   4   7   7  12  12   4  12   7   7   6   6 
##  78  79  73  83  90  92  99 100 101 102  85 103 107  38  44  89 109 113 
##   7   6   4   7   4   7   7   7   7   7   6   7   7  10  10  10   7   7 
##   7 110  76 122 104  75 124 123 125 105  42 127 132 114 130 137  19 121 
##   1   1   8   1   4  12   1   8   4   6   1   1   1  10   4  10  13   7 
## 133  65 126 146 142  12  37  43 118 129 135 136 139 140 143  40  52  87 
##   1  14  12  12  10   2   5   1   6   8   6   9   7   6   1  11   6   9 
## 120 138 144 145 147 148 
##   6   4  14   7  12   4
```


What does this get us? A set of communities, with each node assigned to each community. We can examine them in `Cytoscape` by doing:


```r
allComms <- split(eb$names, eb$membership)
allComms <- allComms[order(sapply(allComms, length), decreasing = T)]
```



```r
selectNodes(cw, allComms[[1]], preserve.current.selection = FALSE)
```

<img src="figure/community1.svg" title="community1" alt="community1"  width="800px"/>



```r
selectNodes(cw, allComms[[2]], preserve.current.selection = FALSE)
```

<img src="figure/community2.svg" title="community2" alt="community2"  width="800px"/>



```r
selectNodes(cw, allComms[[3]], preserve.current.selection = FALSE)
```

<img src="figure/community3.svg" title="community3" alt="community3"  width="800px"/>



```r
selectNodes(cw, allComms[[4]], preserve.current.selection = FALSE)
```

<img src="figure/community4.svg" title="community4" alt="community4"  width="800px"/>



```r
deleteWindow(cw)
```

```
## [1] TRUE
```



## System Information


```r
sessionInfo()
```

```
## R version 3.0.3 (2014-03-06)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] igraph_0.7.1      RCytoscape_1.12.0 XMLRPC_0.3-0      graph_1.40.1     
## [5] reshape2_1.4      ggplot2_0.9.3.1   knitr_1.5        
## 
## loaded via a namespace (and not attached):
##  [1] BiocGenerics_0.8.0 Cairo_1.5-5        colorspace_1.2-4  
##  [4] digest_0.6.4       evaluate_0.5.3     formatR_0.10      
##  [7] grid_3.0.3         gtable_0.1.2       labeling_0.2      
## [10] MASS_7.3-32        munsell_0.4.2      parallel_3.0.3    
## [13] plyr_1.8.1         proto_0.3-10       Rcpp_0.11.1       
## [16] RCurl_1.95-4.1     scales_0.2.4       stats4_3.0.3      
## [19] stringr_0.6.2      tools_3.0.3        XML_3.98-1.1
```



```r
purl("wg_visualization_ggplot_cytoscape_matplotlib.Rmd", quiet = TRUE)
```

```
## [1] "wg_visualization_ggplot_cytoscape_matplotlib.R"
```


