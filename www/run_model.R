

source("www/simcross_model.R")

library(shinyBS)
library(viridis)
library(deldir)
library(tidyverse)
library(plyr)

random <- 4
num_cortex <- 1
diam_cortex <- 0.4
size_stele <- 1
diam_stele <- 0.2
proportion_aerenchyma <- 0
n_aerenchyma_files <- 10
n_xylem_files <- 2
diam_xylem <- 0.1

source("www/simcross_model.R")

sim <- simcross(random, 
         num_cortex = num_cortex,
         diam_cortex = diam_cortex,
         size_stele = size_stele,
         diam_stele = diam_stele,
         proportion_aerenchyma = proportion_aerenchyma/100,
         n_aerenchyma_files = n_aerenchyma_files,
         n_xylem_files = n_xylem_files,
         diam_xylem = diam_xylem) 

write_sim_xml(sim, "~/Desktop/root.xml")


ggplot(sim$nodes) + 
  geom_polygon(aes(x, y, group=id_cell, fill=type), colour="white") + 
  theme_classic() + 
  coord_fixed() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 


ggplot(sim$nodes) + 
  geom_segment(aes(x, y, xend=xx, yend=yy, colour=id_wall)) + 
  theme_classic() + 
  coord_fixed() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 


cellgroups <- data.frame(name = unique(sim$cells$type), id = c(1:length(unique(sim$cells$type))))

xml <- '<?xml version="1.0" encoding="utf-8"?>\n'
xml <- paste0(xml, '<crosssimdata>\n')

# Metadata
xml <- paste0(xml, '\t<metadata>\n')
xml <- paste0(xml, '\t\t<parameters>\n')
xml <- paste0(xml, '\t\t\t<parameter name="num_cortex" value="',num_cortex,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="diam_cortex" value="',diam_cortex,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="size_stele" value="',size_stele,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="diam_stele" value="',diam_stele,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="proportion_aerenchyma" value="',proportion_aerenchyma/100,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="n_aerenchyma_files" value="',n_aerenchyma_files,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="n_xylem_files" value="',n_xylem_files,'"/>\n')
xml <- paste0(xml, '\t\t\t<parameter name="diam_xylem" value="',diam_xylem,'"/>\n')
xml <- paste0(xml, '\t\t</parameters>\n')
xml <- paste0(xml, '\t</metadata>\n"')

# Cells
xml <- paste0(xml, '\t<cells count="',nrow(sim$cells),'">\n')
for(i in c(1:nrow(sim$cells))){
  temp <- sim$nodes[sim$nodes$id_cell == sim$cells$id_cell[i],]
  xml <- paste0(xml, '\t\t<cell id="',sim$cells$id_cell[i],'" group="',cellgroups$id[groups$name == sim$cells$type[i]],'" truncated="false" >\n')
  xml <- paste0(xml, '\t\t\t<walls>\n')
  for(j in c(1:nrow(temp))){
    xml <- paste0(xml, '\t\t\t\t<wall id="',temp$id_wall[j],'"/>\n')    
    }
  xml <- paste0(xml, '\t\t\t</walls>\n')
  xml <- paste0(xml, '\t\t</cell>\n')
}
xml <- paste0(xml, '\t</cells>\n')


# Walls
xml <- paste0(xml, '\t<walls count="',nrow(sim$walls),'">\n')
for(i in c(1:nrow(sim$walls))){
  xml <- paste0(xml, '\t\t<wall id="',sim$walls$id_wall[i],'" group="0" edgewall="false" >\n')
  xml <- paste0(xml, '\t\t\t<points>\n')
  xml <- paste0(xml, '\t\t\t\t<points x="',sim$walls$x1[i],'" y="',sim$walls$y1[i],'"/>\n')    
  xml <- paste0(xml, '\t\t\t\t<points x="',sim$walls$x2[i],'" y="',sim$walls$y2[i],'"/>\n')    
  xml <- paste0(xml, '\t\t\t</points>\n')
  xml <- paste0(xml, '\t\t</wall>\n')
}
xml <- paste0(xml, '\t</walls>\n')


# Groups
xml <- paste0(xml, '\t<groups">\n')
xml <- paste0(xml, '\t\t<cellgroups">\n')
for(i in c(1:nrow(cellgroups))){
  xml <- paste0(xml, '\t\t\t<group id="',cellgroups$id[i],'" name="',cellgroups$name[i],'" />\n')
}
xml <- paste0(xml, '\t\t</cellgroups">\n')
xml <- paste0(xml, '\t\t<wallgroups">\n')
xml <- paste0(xml, '\t\t\t<group id="0" name="unassigned" />')
xml <- paste0(xml, '\t\t</wallgroups">\n')
xml <- paste0(xml, '\t</groups">\n')

xml <- paste0(xml, '<crosssimdata>')

cat(xml, file = "~/Desktop/rootsectionsim.xml")

