
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

pancreas_datasets <- c(
    "baron-human.rds",
    "muraro.rds",
    "segerstolpe.rds",
    "xin.rds"
)

embryo_datasets <- c(
    "bias.rds",
    "deng-rpkms.rds",
    "deng-reads.rds",
    "fan.rds",
    "goolam.rds"
)

## define server global variables
values <- reactiveValues()
values$reference <- FALSE

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_sankey <- function() {
        validate(need(
            values$scmap_ref,
            "\nPlease upload your Reference dataset first!"
        ))
        inFile <- input$to_project
        validate(need(
            inFile,
            "\nPlease upload your Projection dataset first!"
        ))
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
        
        values$mapping <- data.frame(cell_ids = rownames(pData(scmap_map)), scmap_assignments = labs_new)
        # Cohen's Kappa coefficient
        # kappa2(cbind(labs_orig, labs_new))$value
        # Sankey diagram
        sankey <- getSankey(labs_orig, labs_new)
        return(sankey)
    }
    
    output$ui <- renderUI({
        if (is.null(input$data_type))
            return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$data_type,
               "own" = list(
                   HTML("2. Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater'>scater</a> format<br><br>"),
                   fileInput('reference', NULL, accept=c('.rds'))
                   ),
               "existing" = list(
                   HTML("2. Our own Reference "),
                   HTML("(more information <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/'>here</a>)<br><br>"),
                   HTML("<em>Choose a data type</em>"),
                   radioButtons("ref_type", NULL,
                                c("Human Pancreas" = "pancreas",
                                  "Mouse Embryo" = "embryo")),
                   HTML("<em>Choose a dataset</em><br><br>"),
                   conditionalPanel(
                       condition = "input.ref_type == 'pancreas'",
                       selectInput(inputId = "refs_pancreas",
                                   label = NULL,
                                   pancreas_datasets)
                   ),
                   conditionalPanel(
                       condition = "input.ref_type == 'embryo'",
                       selectInput(inputId = "refs_embryo",
                                   label = NULL,
                                   embryo_datasets)
                   )
               )
        )
    })
    
    observe({
        if(input$data_type == "existing") {
            values$reference <- TRUE
        } else {
            values$reference <- FALSE
        }
    })
    
    output$sankey <- renderGvis({
        withProgress(message = 'Making plot', {
            incProgress()
            get_sankey()
        })
    })
    
    output$ref_features <- renderPlot({
        withProgress(message = 'Making plot', {
            incProgress()
            if (values$reference) {
                if(input$ref_type == "pancreas") {
                    values$scmap_ref <- readRDS(paste0("refs/", input$refs_pancreas))
                }
                
                if(input$ref_type == "embryo") {
                    values$scmap_ref <- readRDS(paste0("refs/", input$refs_embryo))
                }
            } else {
                validate(need(
                    input$reference$datapath,
                    "\nPlease upload your Reference dataset first!"
                ))
                values$scmap_ref <- readRDS(input$reference$datapath)
            }
            getFeatures(values$scmap_ref, n_features = as.numeric(input$n_features), suppress_plot = FALSE)
        })
    })
    
    output$download_mapping <- downloadHandler(
      filename = function() {
        paste('scmap_mapping.csv', sep='')
      },
      content = function(con) {
        write.csv(values$mapping, con, quote = FALSE, row.names = FALSE)
      }
    )

}
