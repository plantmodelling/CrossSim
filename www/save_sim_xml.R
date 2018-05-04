

save_sim_xml <- function(sim = NULL, path = NULL){
  
  
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
  xml <- paste0(xml, '\t</metadata>\n')
  
  # Cells
  xml <- paste0(xml, '\t<cells count="',nrow(sim$cells),'">\n')
  for(i in c(1:nrow(sim$cells))){
    temp <- sim$nodes[sim$nodes$id_cell == sim$cells$id_cell[i],]
    xml <- paste0(xml, '\t\t<cell id="',sim$cells$id_cell[i]-1,'" group="',cellgroups$id[groups$name == sim$cells$type[i]],'" truncated="false" >\n')
    xml <- paste0(xml, '\t\t\t<walls>\n')
    for(j in c(1:nrow(temp))){
      xml <- paste0(xml, '\t\t\t\t<wall id="',temp$id_wall[j]-1,'"/>\n')    
    }
    xml <- paste0(xml, '\t\t\t</walls>\n')
    xml <- paste0(xml, '\t\t</cell>\n')
  }
  xml <- paste0(xml, '\t</cells>\n')
  
  
  # Walls
  xml <- paste0(xml, '\t<walls count="',nrow(sim$walls),'">\n')
  for(i in c(1:nrow(sim$walls))){
    xml <- paste0(xml, '\t\t<wall id="',sim$walls$id_wall[i]-1,'" group="0" edgewall="false" >\n')
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
  xml <- paste0(xml, '\t\t\t<group id="0" name="unassigned" />\n')
  xml <- paste0(xml, '\t\t</wallgroups">\n')
  xml <- paste0(xml, '\t</groups">\n')
  
  xml <- paste0(xml, '<crosssimdata>')
  
  cat(xml, file = path)
  
  
}