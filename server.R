
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)
library(scater)
library(googleVis)
library(scmap)

options(shiny.maxRequestSize=200*1024^2)

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_sankey <- function() {
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        scmap_map <- readRDS(inFile$datapath)
        
        scmap_ref <- values$scmap_ref
        scmap_ref <- getFeatures(scmap_ref)

        # find and select only common features
        scmap_map <- setFeatures(scmap_map, fData(scmap_ref)$feature_symbol[fData(scmap_ref)$scmap_features])
        scmap_ref <- setFeatures(scmap_ref, fData(scmap_map)$feature_symbol[fData(scmap_map)$scmap_features])
        
        # main scmap function
        scmap_map <- mapData(
            scmap_map,
            scmap_ref
        )
        
        # quantify the mapping
        labs_orig <- as.character(pData(scmap_map)$cell_type1)
        labs_new <- pData(scmap_map)$scmap_labs
        # Cohen's Kappa coefficient
        # kappa2(cbind(labs_orig, labs_new))$value
        # Sankey diagram
        sankey <- getSankey(labs_orig, labs_new)
        return(sankey)
    }
    
    output$sankey <- renderGvis({
        get_sankey()
    })
    
    output$ref_features <- renderPlot({
        withProgress(message = 'Making plot', {
            incProgress()
            if(input$data_type == "pancreas") {
                values$scmap_ref <- readRDS(paste0("refs/", input$refs_pancreas))
            }
            
            if(input$data_type == "embryo") {
                values$scmap_ref <- readRDS(paste0("refs/", input$refs_embryo))
            }
            getFeatures(values$scmap_ref, n_features = as.numeric(input$n_features), suppress_plot = FALSE)
        })
    })

}
