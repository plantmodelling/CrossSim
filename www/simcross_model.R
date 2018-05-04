





# RUN MODEL -----

simcross <- function(random,
                     num_cortex,
                     diam_cortex,
                     size_stele,
                     diam_stele,
                     proportion_aerenchyma,
                     n_aerenchyma_files,
                     n_xylem_files,
                     diam_xylem){
  
  # PARAMETERS -----
  random_fact <- random / 100
  
  
  
  # INITIALIZE LAYERS -----
  layers <- data.frame("name" = character(0), 
                       "n_layer" = numeric(0), 
                       "size" = numeric(0))
  
  layers <- rbind(layers, data.frame(name="stele", n_layer=1, size=size_stele))
  layers <- rbind(layers, data.frame(name="pericycle", n_layer=1, size=0.3))
  layers <- rbind(layers, data.frame(name="endodermis", n_layer=1, size=0.4))
  layers <- rbind(layers, data.frame(name="cortex", n_layer=num_cortex, size=diam_cortex))
  layers <- rbind(layers, data.frame(name="exodermis", n_layer=1, size=0.6))
  layers <- rbind(layers, data.frame(name="epidermis", n_layer=1, size=0.3))
  
  # Create and "outside" layer to serve as boundary for the voronoi algorithm.
  layers <- rbind(layers, data.frame(name="outside", n_layer=1, size=0.3))
  
  layers$n_layer[layers$name == "stele"] <- round((layers$size[layers$name == "stele"]/2) / diam_stele)
  layers$size[layers$name == "stele"] <- diam_stele
  

  # Get one row per actual cell layer
  all_layers <- NULL
  for(i in c(1:nrow(layers))){
    for(j in c(1:layers$n_layer[i])){
      all_layers <- rbind(all_layers, layers[i,])
    }
  }
  
  all_layers$radius <- all_layers$size/2 # Base. we update that just now
  all_layers$perim <- all_layers$radius * 2 * pi
  all_layers$n_cell <- 1
  all_layers$angle_inc <- 0
  for(i in c(2:nrow(all_layers))){
    # Update radius
    all_layers$radius[i] <- all_layers$radius[i-1] +  
      all_layers$size[i-1] / 2 + 
      all_layers$size[i] / 2
    if(all_layers$name == "outside"){
      all_layers$radius[i] <- all_layers$radius[i-1] +  
        all_layers$size[i-1] / 2 + 
        all_layers$size[i] *2
    }
    
    # Update perimeter
    all_layers$perim[i] <- all_layers$radius[i] * 2 * pi
    
    # Update number of cells in the layers
    all_layers$n_cell[i] <- round(all_layers$perim[i] / all_layers$size[i])
    
    # Update the mean angle between cells
    all_layers$angle_inc[i] <- 2 * pi / all_layers$n_cell[i]
  }
  
  
  
  # CREATE CELLS ------
  center <- max(all_layers$radius)
  all_cells <- NULL
  k <- 1
  for(i in c(1:nrow(all_layers))){
    radius <- all_layers$radius[i]
    if(all_layers$angle_inc[i] > 0){
      angles <- seq(from = 0, to = (2*pi), by = all_layers$angle_inc[i])[-1]
    }else{
      angles <- 0
    }
    k1 <- k+all_layers$n_cell[i]-1
    ks <- c(k:k1)
    k <- k1+1

    if(all_layers$name[i] == "outside"){
      x <- center + (radius * cos(angles))
      y <- center + (radius * sin(angles))
    }else if(all_layers$name[i] == "stele"){
      x <- center + (radius * cos(angles)) * runif(all_layers$n_cell[i], 1-(random_fact*2), 1+(random_fact*2))
      y <- center + (radius * sin(angles)) * runif(all_layers$n_cell[i], 1-(random_fact*2), 1+(random_fact*2))
    }else{
      x <- center + (radius * cos(angles)) * runif(all_layers$n_cell[i], 1-random_fact, 1+random_fact)
      y <- center + (radius * sin(angles)) * runif(all_layers$n_cell[i], 1-random_fact, 1+random_fact)
    }

    all_cells <- rbind(all_cells, data.frame(
      angle = angles,
      radius = radius,
      x = x,
      y = y,
      id_layer = i,
      id_cell = ks,
      type = all_layers$name[i]
      )
    )
  }

  # all_cells <- all_cells_bis
  summary_cells <- ddply(all_cells, .(type), summarise, n_cells = length(angle))
  
  
  
  
  # CREATE XYLEM VESSELS -----
  # Create the xylem files
  # Get the extremes
  xyl <- data.frame(r=numeric(2), d=numeric(2))
  xyl$r <- c(0, max(all_cells$radius[all_cells$type == "stele"]))
  xyl$d <- c(diam_xylem, diam_stele)
  
  # Get the cells in between
  fit <- lm(d ~ r, data=xyl)$coefficients
  rnew <- xyl$r[1]
  i <- 1
  rmax <- xyl$r[2]
  dmin <- xyl$d[2]
  keep_going <- T
  while(keep_going){
    xyl <- xyl %>% arrange(r)
    
    rnew <- xyl$r[i] + xyl$d[i] #+ (xyl$r[2]/10)
    dnew <- fit[1] + rnew*fit[2]
    while(rnew+(dnew/2) > rmax-(dmin/2)){
      rnew <- rnew - 0.05
      dnew <- dnew - 0.05
      keep_going = F
    }
    xyl <- rbind(xyl, data.frame(r = rnew,d = dnew))
    i <- i+1
    
  }
  xyl <- xyl %>% arrange(r)  
  while(xyl$d[nrow(xyl)] >= xyl$d[nrow(xyl)-1]){
    xyl$d[nrow(xyl)] <- xyl$d[nrow(xyl)] - 0.04
    xyl$r[nrow(xyl)] <- xyl$r[nrow(xyl)] - 0.02
  }
  
  # Create the Xylem cells
  all_xylem <- NULL
  angle_seq <- seq(from = 0, to = (2*pi), by = (2 * pi) / n_xylem_files)
  x <- center + (xyl$r[1] * cos(angle_seq[1]))
  y <- center + (xyl$r[1] * sin(angle_seq[1]))
  all_xylem <- rbind(all_xylem, data.frame(x = x,
                                           y = y,
                                           d = xyl$d[1],
                                           angle = angle_seq[1]))
  all_cells <- rbind(all_cells, data.frame(
    angle = angle_seq[1],
    radius = xyl$r[1],
    x = x,
    y = y,
    id_layer = 20,
    id_cell = 1,
    type = "xylem"
  ))
  for(angle in angle_seq){
    x <- center + (xyl$r[-1] * cos(angle))
    y <- center + (xyl$r[-1] * sin(angle))
    all_xylem <- rbind(all_xylem, data.frame(x = x,
                                             y = y,
                                             d = xyl$d[-1],
                                             angle = angle))
    all_cells <- rbind(all_cells, data.frame(
      angle = angle,
      radius = xyl$r[-1],
      x = x,
      y = y,
      id_layer = 20,
      id_cell = 1,
      type = "xylem"
    )
    )
  }

  # remove stele cells to be replaced by xylem cells
  for(i in c(1:nrow(all_xylem))){
    all_cells <- all_cells %>% 
      filter((x-all_xylem$x[i])^2 + (y - all_xylem$y[i])^2 > (all_xylem$d[i]/1)^2 | type == "xylem") # find the cells inside the xylem poles and remove them
  }
  

  
  # # Create the xylem cells
  # for(i in c(1:nrow(all_xylem))){
  #   cx = all_xylem$x[i]
  #   cy = all_xylem$y[i]
  #   radius <- all_xylem$d[i]/2
  #   angles <- seq(from = 0, to = (2*pi), by = (2*pi)/30)
  #   x <- cx + (radius * cos(angles))
  #   y <- cy + (radius * sin(angles))
  # 
  #   all_cells <- rbind(all_cells, data.frame(
  #       angle = angles,
  #       radius = radius,
  #       x = x,
  #       y = y,
  #       id_layer = 20+i,
  #       id_cell = 1,
  #       type = "xylem"
  #       )
  #     )
  #   }

  all_cells$id_cell <- c(1:nrow(all_cells))
  
  
  # CREATE GEOMETRY ------
  # Get the voronio data
  vtess <- deldir(all_cells$x, all_cells$y)
  
  # Remove the ouside cells, to get the voronoi data straight
  all_cells <- all_cells  %>%
    filter(type != "outside")
  
  # Get the size of the cells
  cell_size <- vtess$summary
  cell_size$id_cell <- c(1:nrow(cell_size))
  all_cells <- merge(all_cells, cell_size[,c("id_cell", "dir.area")], by="id_cell")
  all_cells$dist <- sqrt((all_cells$x - center)^2 + (all_cells$y - center)^2 )
  
  ids <- all_cells$id_cell
  
  rs <- vtess$dirsgs[vtess$dirsgs$ind1 %in% ids | 
                       vtess$dirsgs$ind2 %in% ids,]
  
  
  # Get the cooridnates for every cells
  rs <- rs %>% 
    arrange(ind1)
  rs2 <- data.frame(x = rs$x1, y=rs$y1, id_cell = rs$ind1)
  rs2 <- rbind(rs2, data.frame(x = rs$x2, y=rs$y2, id_cell = rs$ind1))
  rs2 <- rbind(rs2, data.frame(x = rs$x2, y=rs$y2, id_cell = rs$ind2))
  rs2 <- rbind(rs2, data.frame(x = rs$x1, y=rs$y1, id_cell = rs$ind2))
  rs2 <- merge(rs2, all_cells[,c("id_cell", "type", "dir.area", "dist", "angle", "radius", "id_layer")], by="id_cell")
  
  
  # CREATE AERENCHYMA -----
  angle_inc <- (2 * pi) / n_aerenchyma_files
  angle_range_inc <- (2 * pi * proportion_aerenchyma / 2) / n_aerenchyma_files
  safe_cortex_layer <- min(rs2$id_layer[rs2$type == "cortex"])
  
  rs2$type <- as.character(rs2$type)
  angle <- runif(1, 0.8, 1) * pi/n_aerenchyma_files
  for(j in c(1:n_aerenchyma_files)){
    angle_range <- c(angle - angle_range_inc, angle + angle_range_inc)
    rs2 <- rs2 %>% 
      filter(!(id_layer != safe_cortex_layer & type == "cortex" & angle > angle_range[1] & angle < angle_range[2]))
    angle <- angle + angle_inc
  }
  
  
  
  # # TIDY XYLEM ------
  # #rs3 <- rs2
  # #rs2 <- rs3
  # for(i in c(1:nrow(all_xylem))){
  #   temp <- rs2 %>% 
  #     filter((x-all_xylem$x[i])^2 + (y - all_xylem$y[i])^2 < (all_xylem$d[i]/2)^2)
  #   minid <- min(temp$id_cell)
  #   unid <- unique(temp$id_cell)
  #   rs2$id_cell[rs2$id_cell %in% unid] <- minid
  #     # filter((x-all_xylem$x[i])^2 + (y - all_xylem$y[i])^2 < (all_xylem$d[i]/2)^2)
  # #   rs2$id_cell[rs2$type == "xylem" & rs2$id_layer == i] <- min(rs2$id_cell[rs2$type == "xylem" & rs2$id_layer == i])
  # }
  # 
  # REORDER ------
  
  rs1 <- rs2 %>% 
    dplyr::group_by(id_cell) %>% 
    dplyr::mutate(my = mean(y)) %>% 
    dplyr::mutate(mx = mean(x)) %>% 
    dplyr::mutate(atan = atan2(y-my, x - mx)) %>% 
    arrange(id_cell, atan) %>% 
    filter(!duplicated(atan))
  
  # Reset theids of the cells to be continuous
  ids <- data.frame(id_cell = unique(rs1$id_cell))
  ids$new <- c(1:nrow(ids))
  rs1 <- merge(rs1, ids, by="id_cell")
  rs1$id_cell <- rs1$new
  
  all_cells <- merge(all_cells, ids, by="id_cell")
  all_cells$id_cell <- all_cells$new
  
  
  section <- data.frame(n_cells = nrow(all_cells),
                        n_xylem = nrow(all_cells[all_cells$type == "xylem",]),
                        n_phloem = nrow(all_cells[all_cells$type == "phloem",]),
                        n_epidermis = nrow(all_cells[all_cells$type == "epidermis",]),
                        n_endodermis = nrow(all_cells[all_cells$type == "endodermis",]),
                        n_exodermis = nrow(all_cells[all_cells$type == "exodermis",]),
                        n_pericycle = nrow(all_cells[all_cells$type == "pericycle",]),
                        n_stele = nrow(all_cells[all_cells$type == "stele",]),
                        n_cortex = nrow(all_cells[all_cells$type == "cortex",]),
                        diameter = max(rs1$x) - min(rs1$x),
                        diameter_stele = max(rs1$x[rs1$type == "stele"]) - min(rs1$x[rs1$type == "stele"]),
                        thickness_cortex = max(rs1$x[rs1$type == "cortex" & rs1$angle > 0 & rs1$angle < 1]) - 
                          min(rs1$x[rs1$type == "cortex" & rs1$angle > 0 & rs1$angle < 1]))
  
  
  rs1$sorting <- c(1:nrow(rs1))
  
  nodes <- NULL
  for(i in unique(rs1$id_cell)){
    temp <-rs1[rs1$id_cell == i,] %>% 
      mutate(xx = c(x[-1],x[1])) %>% 
      mutate(yy = c(y[-1],y[1]))
    nodes <- rbind(nodes, temp)
  }
  
  nodes <- nodes %>% 
    ungroup() %>% 
    mutate(x1 = ifelse(x > xx, x, xx)) %>% 
    mutate(x2 = ifelse(x < xx, x, xx)) %>% 
    mutate(y1 = ifelse(y > yy, y, yy)) %>% 
    mutate(y2 = ifelse(y < yy, y, yy)) %>% 
    arrange(sorting)
  
  walls <- nodes[!duplicated(nodes[,c('x1', 'x2', 'y1', 'y2')]),] %>% 
    select(c(x1, x2, y1, y2))
  
  walls$id_wall <- c(1:nrow(walls))
  
  nodes <- merge(nodes, walls, by=c("x1", "x2", "y1", "y2"))
  nodes <- nodes %>% 
    arrange(sorting)
  
 return(list(nodes = nodes, 
             walls = walls, 
             cells=all_cells, 
             section = section))
}




# SAVE XML -----

write_sim_xml <- function(sim = NULL, path = NULL){
  
  cellgroups <- data.frame(id_group = c(1, 2, 3, 4, 5, 13, 16),
                       type = c("exodermis", "epidermis", "endodermis", "cortex", "stele", "xylem", "pericycle"))
  
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
  sim$nodes <-merge(sim$nodes, cellgroups, by="type")
  temp_wall <- ddply(sim$nodes, .(id_cell, id_group), summarise, walls = paste0('\t\t\t\t<wall id="', 
                                                                                paste(id_wall-1, collapse='"/>\n\t\t\t\t<wall id="'),
                                                                                '"/>\n'))
  xml <- paste0(xml, paste0('\t\t<cell id="',temp_wall$id_cell-1, '" group="', temp_wall$id_group, '" truncated="false" >\n',
            '\t\t\t<walls>\n', temp_wall$walls, '\t\t\t</walls>\n',
            '\t\t</cell>\n', collapse=""))
  xml <- paste0(xml, '\t</cells>\n')
  

  # Walls
  xml <- paste0(xml, '\t<walls count="',nrow(sim$walls),'">\n')
  xml <- paste0(xml,paste0('\t\t<wall id="',sim$walls$id_wall-1,'" group="0" edgewall="false" >\n',
         '\t\t\t<points>\n',
         '\t\t\t\t<point x="',sim$walls$x1,'" y="',sim$walls$y1,'"/>\n',
         '\t\t\t\t<point x="',sim$walls$x2,'" y="',sim$walls$y2,'"/>\n',
         '\t\t\t</points>\n',
         '\t\t</wall>\n', collapse = ""))
  xml <- paste0(xml, '\t</walls>\n')
  
  # Groups
  xml <- paste0(xml, '\t<groups>\n')
  xml <- paste0(xml, '\t\t<cellgroups>\n')
  for(i in c(1:nrow(cellgroups))){
    xml <- paste0(xml, '\t\t\t<group id="',cellgroups$id[i],'" name="',cellgroups$name[i],'" />\n')
  }
  xml <- paste0(xml, '\t\t</cellgroups>\n')
  xml <- paste0(xml, '\t\t<wallgroups>\n')
  xml <- paste0(xml, '\t\t\t<group id="0" name="unassigned" />\n')
  xml <- paste0(xml, '\t\t</wallgroups>\n')
  xml <- paste0(xml, '\t</groups>\n')
  
  xml <- paste0(xml, '</crosssimdata>')
  
  if(!is.null(path)){
    cat(xml, file = path)
    return(TRUE)
  }else{
    return(xml)
  }
  
  
}


