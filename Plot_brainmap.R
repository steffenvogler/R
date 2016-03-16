#install.packages(c("rgeos", "gpclib", "maptools", "sp"))
library(plyr)
library(rgeos)
library(maptools)
library(gpclib) # may be needed, may not be
library(ggplot2)
library(RColorBrewer)
new_fill<- function(pal, lowerlim, upperlim){
  scale_fill_gradientn(colours= pal, limits=c(lowerlim, upperlim))
}



setwd("/Users/Vogler/Desktop")
#counts <- read.csv("counts.csv", stringsAsFactors = FALSE)
counts <- read.csv("counts.csv")


counts$anat_lvl1<-as.character(counts$anat_lvl1)
counts$anat_lvl2<-as.character(counts$anat_lvl2)
colnames(counts)<-c("cell_counts","id2","id")
counts$id<-as.factor(counts$id)
counts$id2<-as.factor(counts$id2)
counts.df <- as.data.frame(counts)
# WHAT COLUMNS DO WE HAVE?
#colwise(class)(counts)
# MAP
brain_map <- readShapeSpatial("BrainAnatomy.shp")
# VERIFY IT LOADED PROPERLY
plot(brain_map)
#names(brain_map)
brain_map <- fortify(brain_map, region="anat_lvl2")
brain_map[, 'id'] <- as.factor(brain_map[, 'id'])

keep <- c("long","lat","cell_counts","id","group")
clean_plotData<-brain_map[,(names(brain_map) %in% keep)]

ggplot(clean_plotData, aes(fill = value)) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions) + ylim(0, 3)



plotData_mrg<-merge(brain_map, counts, by=NULL)
#plotData<-plotData[order(plotData$order), ] 
plotData_mrg$cell_counts<-as.numeric(plotData_mrg$cell_counts)
ggplot(data = plotData_mrg, aes(x = long, y = lat)) + geom_polygon( aes(group = id.y, fill = cell_counts), color="white")+coord_quickmap()+new_fill(brewer.pal(7, "Blues"), 0, 100)


mtcars[, 'cyl'] <- as.factor(mtcars[, 'cyl'])


#plotData<-subset(brain_map, brain_map[, "id"] %in% c("sg"))


ggplot() + geom_map(data = counts, aes(map_id = id, fill = cell_counts), map = brain_map)

ggplot(plotData_mrg) + 
  aes(long,lat,group=group,fill=cell_counts) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal()+new_fill(brewer.pal(7, "Blues"), 0, 100)


