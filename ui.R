
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  navbarPage(title="Root Cross-Section Simulator",
             
     ## Load data ----              
     
     tabPanel("The model", id="tab1", icon = icon("bullseye"),

        # Sidebar with a slider input for number of bins
        fluidRow(
          column(4,
                 tabsetPanel(type = "tabs",
                             tabPanel("Cortex", 
                                      tags$hr(),
                                      helpText("Parameters for the cortex. You can adjust the number of cell layers and the size of individual cells"),
                                      tags$hr(),
                                      numericInput("n_cortex", "Layers:", num_cortex, min = 1, max = 100, step = 1),
                                      numericInput("s_cortex", "Diameter:", diam_cortex, min = 0.1, max = 1, step = 0.1)),
                             tabPanel("Stele",
                                      tags$hr(),
                                      helpText("Parameters for the stele. You can adjust the total diameter of the stele and the size of individual cells"),
                                      tags$hr(),
                                      numericInput("s_stele", "Diameter of stele:", size_stele, min = 0.5, max = 5, step = 0.1),
                                      numericInput("ss_stele", "Diameter of cells:", diam_stele, min = 0.1, max = 1, step = 0.1)),
                             tabPanel("Xylem",
                                      tags$hr(),
                                      helpText("Parameters for the xylem vessels. You can adjust the number of xylem vessel files"),
                                      tags$hr(),
                                      numericInput("n_xylem", "Number of files:", n_xylem_files, min = 2, max = 6, step = 1),
                                      numericInput("s_xylem", "Max size of vessels:", diam_xylem, min = 0.1, max = 0.8, step = 0.1)),
                             tabPanel("Aerenchyma",
                                      tags$hr(),
                                      helpText("Parameters for the production of aerenchyma. You can adjust the number of aerenchyma files and the total proportion of aerenchyma within the cortex."),
                                      tags$hr(),
                                      sliderInput("aerenchyma",
                                         "Proportion of aerenchyma [%]",
                                         min = 0,
                                         max = 90,
                                         value = proportion_aerenchyma),
                                      numericInput("n_aerenchyma", "Number of files:", n_aerenchyma_files, min = 1, max = 20, step = 1))
                 ),
                 tags$hr(),
                 sliderInput("random",
                      "Randomness of cell positions [%]",
                      min = 0,
                      max = 5,
                      value = random),
                 tags$hr(),
                 bsButton(inputId = "refresh", type = "action", style="primary", label="Refresh",icon("recycle"))
          ),
    
          # Show a plot of the generated distribution
          column(8,    
            selectInput("toplot", "Variable to display", choices=c("Type"="type", 
                                                                   "Area" = "dir.area", 
                                                                   "Distance from center" = "dist",
                                                                   "Cell ID" = "id_cell",
                                                                   "Angle" = "angle")),
            tags$hr(),
            # fluidRow(
            #   column(6,
              plotOutput("distPlot", width="700", height="700"),
              tags$hr(),   
              downloadButton("downloadSVG", "SVG"),
              downloadButton("downloadCSV", "TXT"),
              downloadButton("downloadVTP", "XML"),
              downloadButton("downloadPNG", "PNG"),
              # ),
              # column(6,
              #        plotOutput("connectPlot")
              # )
            # ),
            tags$hr()
          )
        )
     )
  )
))
