
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)
library(scater)
library(googleVis)
library(plotly)
library(scmap)

options(shiny.maxRequestSize=200*1024^2)

pancreas_datasets <- c(
    "baron-human.rds",
    "muraro.rds",
    "segerstolpe.rds",
    "xin.rds"
)

embryo_datasets <- c(
    "biase.rds",
    "deng-rpkms.rds",
    "deng-reads.rds",
    "fan.rds",
    "goolam.rds"
)

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_sankey <- function() {
        validate(need(
            values$reference_file,
            "\nPlease upload your Reference dataset first!"
        ))
        validate(need(
            input$to_project$datapath,
            "\nPlease upload your Projection dataset first!"
        ))
        scmap_map <- readRDS(input$to_project$datapath)
        
        scmap_ref <- readRDS(values$reference_file)
        scmap_ref <- getFeatures(scmap_ref, n_features = as.numeric(input$n_features))

        # find and select only common features
        scmap_map <- setFeatures(scmap_map, fData(scmap_ref)$feature_symbol[fData(scmap_ref)$scmap_features])
        scmap_ref <- setFeatures(scmap_ref, fData(scmap_map)$feature_symbol[fData(scmap_map)$scmap_features])
        
        validate(need(
            length(which(scmap_ref@featureData@data$scmap_features)) >= 2,
            "\nThere are no common features between the Reference and Projection datasets! Either check that both datasets are from the same organism or increase the number of selected features (>100)."
        ))
        
        # main scmap function
        scmap_map <- projectData(
            scmap_map,
            scmap_ref
        )
        
        # quantify the mapping
        validate(need(
            pData(scmap_map)$scmap_labs,
            "\nMapping didn't work! Please check your datasets."
        ))
        
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
                   HTML("<div class='panel panel-primary'>
                            <div class='panel-heading'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/muraro.rds'>example</a>)</div>
                            <div class='panel-body'>
                        "),
                   fileInput('reference', NULL, accept=c('.rds')),
                   HTML("</div></div>")
                   ),
               "existing" = list(
                   HTML("<div class='panel panel-primary'>
                            <div class='panel-heading'>Choose a data type</div>
                            <div class='panel-body'>
                        "),
                   radioButtons("ref_type", NULL,
                                c("Human Pancreas" = "pancreas",
                                  "Mouse Embryo" = "embryo")),
                   HTML("</div></div>"),
                   HTML("<div class='panel panel-primary'>
                            <div class='panel-heading'>Choose a Reference</div>
                            <div class='panel-body'>"),
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
                   ),
                   HTML("</div></div>")
               )
        )
    })
    
    observe({
        if(input$data_type == "existing") {
            if(!is.null(input$ref_type)) {
                if(input$ref_type == "pancreas") {
                    values$reference_file <- paste0("refs/", input$refs_pancreas)
                } else {
                    values$reference_file <- paste0("refs/", input$refs_embryo)
                }
            }
        } else {
            values$reference_file <- input$reference$datapath
        }
    })
    
    output$sankey <- renderGvis({
        withProgress(message = 'Making plot', {
            incProgress()
            get_sankey()
        })
    })
    
    output$ref_features <- renderPlotly({
        withProgress(message = 'Making plot', {
            incProgress()
            if (input$data_type == "existing") {
                scmap_ref <- readRDS(values$reference_file)
            } else {
                validate(need(
                    values$reference_file,
                    "\nPlease upload your Reference dataset first!"
                ))
                scmap_ref <- readRDS(values$reference_file)
            }
            scmap:::plotFeatures(scmap_ref, n_features = as.numeric(input$n_features))
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
