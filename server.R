
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
library(htmlTable)

options(shiny.maxRequestSize=200*1024^2)

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    projectAll <- function() {
        for (f in values$refs) {
            name <- strsplit(f, "\\.")[[1]][1]
            withProgress(message = paste0('Projecting to ', name), {
                incProgress()
                projectOne(f)
            })
        }
    }
    
    summariseAll <- function() {
        withProgress(message = 'Summarising results... ', {
            d <- NULL
            res_assign <- NULL
            res_siml <- NULL
            names <- NULL
            for (f in values$refs) {
                incProgress()
                name <- strsplit(f, "\\.")[[1]][1]
                if (!is.null(values[[f]])) {
                    tmp <- values[[paste0(f, "-ref-cell-types")]]$covered_cell_types
                    tmp1 <- paste(values[[paste0(f, "-ref-cell-types")]]$not_covered_cell_types, collapse = ", ")
                    d <- rbind(d, c(name, unique(values[[f]]$scmap_assign_rate), htmlTable(tmp), tmp1))
                    res_assign <- cbind(res_assign, as.character(values[[f]]$scmap_assignments))
                    res_siml <- cbind(res_siml, round(values[[f]]$scmap_similarity, 2))
                    names <- c(names, name)
                } else {
                    d <- rbind(d, c(name, 0, "There are less than ten features in common between the Reference and Projection datasets. Most probably they come from different organisms!", ""))
                }
            }
            res_assign <- as.data.frame(res_assign)
            colnames(res_assign) <- names
            rownames(res_assign) <- values[[f]]$cell_ids
            values$all_results_assignments <- res_assign
            
            res_siml <- as.data.frame(res_siml)
            colnames(res_siml) <- names
            rownames(res_siml) <- values[[f]]$cell_ids
            values$all_results_similarities <- res_siml
            
            d <- as.data.frame(d, stringsAsFactors = FALSE)
            d[,2] <- round(as.numeric(d[,2]), 1)
            d <- d[order(d[,2], decreasing = TRUE), ]
            colnames(d) <- c("Reference", "% of assigned cells", "% of cells per Cell Type", "Missed Cell Types")
            return(d)
        })
    }
    
    get_sankey <- function() {
        scmap_map <- readRDS(input$to_project$datapath)
        
        scmap_ref <- readRDS(input$reference$datapath)
        scmap_ref <- getFeatures(scmap_ref)
        
        # main scmap function
        scmap_map <- projectData(
            scmap_map,
            scmap_ref
        )
        
        # quantify the mapping
        validate(need(
            pData(scmap_map)$scmap_labs,
            "There are less than ten features in common between the Reference and Projection datasets. Most probably they come from different organisms!"
        ))
        
        labs_new <- pData(scmap_map)$scmap_labs
        
        uns_rate <- length(labs_new[labs_new == "unassigned"])/length(labs_new) * 100
        
        values$mapping <- data.frame(cell_ids = rownames(pData(scmap_map)), scmap_assignments = labs_new)
        if (uns_rate > 50) {
            values$mapping_uns <- paste0("<p class='text-danger'><b>", round(uns_rate, 1), "% of the cells were unassigned.</b></p>")
        } else {
            values$mapping_uns <- paste0("<p class='text-success'><b>", round(uns_rate, 1), "% of the cells were unassigned.</b></p>")
        }
        # Sankey diagram
        if (!is.null(pData(scmap_map)$cell_type1)) {
            values$mapping_sank <- NULL
            labs_orig <- as.character(pData(scmap_map)$cell_type1)
            sankey <- getSankey(labs_orig, labs_new)
            return(sankey)
        } else {
            values$mapping_sank <- "<p class='text-danger'>There is no <b><em>cell_type1</em></b> column in the <b><em>phenoData</em></b> slot of your dataset, therefore Sankey diagram cannot be plotted.</p>"
        }
        return(NULL)
    }
    
    projectOne <- function(ref) {
        # reset the unassigned rate message
        values[[paste0(ref, "-uns")]] <- NULL
        
        ref_data <- read.table(paste0("www/refs/", input$organism, "/", ref), check.names = FALSE, sep = "\t")
        scmap_map <- readRDS(input$to_project$datapath)
        
        # main scmap function
        scmap_map <- projectData(
            scmap_map,
            ref_data
        )
        
        # quantify the mapping
        
        # validate(need(
        #     pData(scmap_map)$scmap_labs,
        #     "There are less than ten features in common between the Reference and Projection datasets. Most probably they come from different organisms!"
        # ))
        if (!is.null(pData(scmap_map)$scmap_labs)) {
            scmap_labs <- pData(scmap_map)$scmap_labs
            assign_rate <- 
                length(scmap_labs[scmap_labs != "unassigned"])/length(scmap_labs) * 100
            values[[ref]] <- data.frame(cell_ids = rownames(pData(scmap_map)), 
                                        scmap_assignments = scmap_labs,
                                        scmap_similarity = pData(scmap_map)$scmap_siml,
                                        scmap_assign_rate = assign_rate)
            covered_cell_types <- as.data.frame(table(pData(scmap_map)$scmap_labs))
            covered_cell_types <- 
                covered_cell_types[order(covered_cell_types[,2], decreasing = TRUE), ]
            covered_cell_types <- covered_cell_types[covered_cell_types[,1] != "unassigned", ]
            covered_cell_types[,2] <- round(covered_cell_types[,2]/length(scmap_labs) * 100, 1)
            rownames(covered_cell_types) <- NULL
            colnames(covered_cell_types) <- NULL
            not_covered_cell_types <- colnames(ref_data)[!colnames(ref_data) %in% unique(as.character(pData(scmap_map)$scmap_labs))]
            values[[paste0(ref, "-ref-cell-types")]] <- list(covered_cell_types = covered_cell_types, not_covered_cell_types = not_covered_cell_types)
        } else {
            values[[paste0(ref)]] <- NULL
            values[[paste0(ref, "-ref-cell-types")]] <- NULL
        }
        return(NULL)
    }
    
    output$datasets <- renderUI({
        if (is.null(input$data_type))
            return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$data_type,
               "own" = list(
                   box(width = 12,
                       HTML("<p class='lead'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater' target='_blank'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/muraro.rds'>example-muraro</a> - a human pancreatic <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/' target='_blank'>dataset</a> with 2126 cells)</p>"),
                        fileInput('reference', NULL, accept=c('.rds')),
                       solidHeader = TRUE
                       # status = "primary"
                   )
               ),
               "existing" = list(
                   box(width = 12,
                       HTML("<p class='lead text-warning'>Your data will be projected to all datasets in our Reference.
                             For more details of the Reference please visit
                             our <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/' target='_blank'>collection of scRNA-seq datasets</a>.</p>"),
                       solidHeader = TRUE
                       # status = "warning"
                   )
               )
        )
    })
    
    output$results <- renderUI({
        if (is.null(input$data_type))
            return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$data_type,
               "own" = list(
                   conditionalPanel("output.reference_dataset",
                       fluidRow(
                           box(width = 6,
                               title = "Projection Results",
                               HTML("<br>"),
                               uiOutput('mapping_uns'),
                               uiOutput('mapping_sank'),
                               htmlOutput("sankey"),
                               HTML("<br>"),
                               downloadButton('download_mapping', 'Download Results'),
                               solidHeader = TRUE,
                               status = "primary"
                           )
                       )
                   ),
                   conditionalPanel("!output.reference_dataset",
                                    fluidRow(
                                        column(width = 10,
                                               HTML("
<br><br>
<div class='alert alert-dismissible alert-warning'>
<p class = 'lead'>Looks like you forgot to upload a dataset that you want to 
use as a <b>Reference</b>. Please go back to the <em>Datasets</em> tab and upload 
your dataset.</p>
</div>"),
                                               offset = 1
                                        )
                                    )
                   )
               ),
               "existing" = list(
                   fluidRow(
                       box(width = 12,
                           title = "Results",
                           DT::dataTableOutput('results_table'),
                           downloadButton("all_results_assignments", 'Download All Assignments'),
                           downloadButton("all_results_similarities", 'Download All Similarities'),
                           solidHeader = TRUE,
                           status = "success"
                       )
                   )
               )
          )
    })
    
    # output$human_pancreas_baron <- renderGvis({
    #     withProgress(message = 'Projecting to Baron (human)', {
    #         incProgress()
    #         get_sankey_ref("baron-human.txt")
    #     })
    # })

    output$results_table <- DT::renderDataTable({
        values$refs <- list.files(paste0("www/refs/", input$organism), pattern = ".txt")
        projectAll()
        DT::datatable(summariseAll(), escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:3)))
                      ),
                      extensions = 'Buttons')
    })
    
    observe({
        # download buttons
        lapply(values$refs, function(i) {
            output[[i]] <- downloadHandler(
                filename = function() {
                    paste0('scmap_projection_to_', strsplit(i, "\\.")[[1]][1], '.csv')
                },
                content = function(con) {
                    write.csv(values[[i]], con, quote = FALSE, row.names = FALSE)
                }
            )
            output[[paste0(i, "-uns")]] <- renderUI(
                if (!is.null(values[[paste0(i, "-uns")]])) {
                    HTML(values[[paste0(i, "-uns")]])
                }
            )
            output[[paste0(i, "-sank")]] <- renderUI(
                if (!is.null(values[[paste0(i, "-uns")]])) {
                    HTML(values[[paste0(i, "-sank")]])
                }
            )
        })
    })
    
    output$mytable <- DT::renderDataTable({
        DT::datatable(values$feature_table, escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:1)))
                      ),
                      extensions = 'Buttons')
    })
    
    output$reference_dataset <- reactive({
        !is.null(input$reference$datapath)
    })
    
    output$projection_dataset <- reactive({
        !is.null(input$to_project$datapath)
    })
    
    output$mapping_uns <- renderUI(
        HTML(values$mapping_uns)
    )
    output$mapping_sank <- renderUI(
        HTML(values$mapping_sank)
    )
    
    output$sankey <- renderGvis({
            get_sankey()
    })
    
    output$ref_features <- renderPlot({
            scmap_ref <- readRDS(input$reference$datapath)
            scmap_ref <- getFeatures(scmap_ref, suppress_plot = FALSE)
            f_data <- fData(scmap_ref)
            f_data <- f_data[, colnames(f_data) %in% c("feature_symbol", "scmap_features", "scmap_scores")]
            f_data <- f_data[f_data$scmap_features,]
            f_data <- f_data[order(f_data$scmap_scores, decreasing = TRUE), ]
            f_data$scmap_scores <- round(f_data$scmap_scores, 1)
            rownames(f_data) <- NULL
            f_data <- f_data[,c(1,3)]
            colnames(f_data) <- c("Feature Name", "scmap score")
            values$feature_table <- f_data
    })
    
    output$download_mapping <- downloadHandler(
      filename = function() {
        paste('scmap_projection.csv', sep='')
      },
      content = function(con) {
        write.csv(values$mapping, con, quote = FALSE, row.names = FALSE)
      }
    )
    
    output$all_results_assignments <- downloadHandler(
        filename = function() {
            paste('scmap_results_assignments.csv', sep='')
        },
        content = function(con) {
            write.csv(values$all_results_assignments, con, quote = FALSE)
        }
    )
    
    output$all_results_similarities <- downloadHandler(
        filename = function() {
            paste('scmap_results_similarities.csv', sep='')
        },
        content = function(con) {
            write.csv(values$all_results_similarities, con, quote = FALSE)
        }
    )

    outputOptions(output, "projection_dataset", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_dataset", suspendWhenHidden = FALSE)
}
