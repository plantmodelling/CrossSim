
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output, clientData, session) {  
  
    withProgress(message = "Computing cells", {
      
      dat <- reactiveValues(cell_data = simcross(random, 
                                               num_cortex = num_cortex,
                                               diam_cortex = diam_cortex,
                                               size_stele = size_stele,
                                               diam_stele = diam_stele,
                                               proportion_aerenchyma = proportion_aerenchyma/100,
                                               n_aerenchyma_files = n_aerenchyma_files,
                                               n_xylem_files = n_xylem_files,
                                               diam_xylem = diam_xylem))
    })
  
    ## MODEL -----
    observeEvent(input$refresh, {
      
      withProgress(message = "Computing cells", {
        
          dat$cell_data <- simcross(input$random, 
                                num_cortex = input$n_cortex,
                                diam_cortex = input$s_cortex,
                                size_stele = input$s_stele,
                                diam_stele = input$ss_stele,
                                proportion_aerenchyma = input$aerenchyma/100,
                                n_aerenchyma_files = input$n_aerenchyma,
                                n_xylem_files = input$n_xylem,
                                diam_xylem = input$s_xylem)  
      })
    })
    
    ## PLOTS ---------
    
    ## > Cell plot -------
    cellPlot <- function(cell){
      pl <- ggplot(cell) + 
        geom_polygon(aes_string("x", "y", group="id_cell", fill=input$toplot), colour="white") + 
        theme_classic() + 
        coord_fixed() +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
      
      if(input$toplot != "type"){
        pl <- pl + scale_fill_viridis()
      }else{
        #pl <- pl + scale_fill_brewer(palette = "BrBG") #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

      }
      return(pl)
    }
    
    output$distPlot <- renderPlot({
      if(is.null(dat$cell_data$cells)){return ()}
      cellPlot(dat$cell_data$cells)
    })
    
    ## > Connect plot -------
    connectPlot <- function(cell){
      pl <-   ggplot(cell, aes_string("x1", "y1", colour=input$toplot)) +
        geom_segment(aes(xend=x2, yend=y2))+
        geom_point()+
        theme_classic() +
        coord_fixed() +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
      
      if(input$toplot != "type"){
        pl <- pl + scale_colour_viridis()
      }else{
        #pl <- pl + scale_fill_brewer(palette = "BrBG") #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
        
      }
      return(pl)
    }
    
    output$connectPlot <- renderPlot({
      if(is.null(dat$cell_data$nodes)){return ()}
      connectPlot(dat$cell_data$nodes)
    })    

     
    ## OUTPUTS ---------
  
    ## > PNG -------------
    output$downloadPNG <- downloadHandler(
      filename = function() {
        "crosssection.png"
      },
      content = function(file) {
        if(is.null(dat$cell_data$cells)){return ()}
        plot <- cellPlot(dat$cell_data$cells)
        plot + ggsave(file, height = 10, width = 10, bg="transparent")
      }
    )   
    
    
    ## > SVG -------------
    output$downloadSVG <- downloadHandler(
      filename = function() {
        "crosssection.svg"
      },
      content = function(file) {
        if(is.null(dat$cell_data$cells)){return ()}
        plot <- cellPlot(dat$cell_data$cells)
        plot + ggsave(file, height = 10, width = 10, bg="transparent")
      }
    )   
    
    
  
})
