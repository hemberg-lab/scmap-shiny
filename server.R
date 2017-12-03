
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)
library(SingleCellExperiment)
library(scmap)
library(htmlTable)
library(DT)

options(shiny.maxRequestSize=200*1024^2)

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    scmapClusterResults2table <- function(index, labs) {
        # compute assignement rate
        assign_rate <- apply(
            labs, 
            2, 
            function(x) {
                round(length(x[x != "unassigned"])/length(x) * 100, 1)
            }
        )
        # compute which clusters have been covered
        clusters_covered <- apply(
            labs,
            2,
            function(x) {
                y <- as.data.frame(table(x))
                y <- y[order(y[,2], decreasing = TRUE), ]
                y <- y[y[,1] != "unassigned", ]
                y[,2] <- round(y[,2] / length(x) * 100, 1)
                rownames(y) <- NULL
                colnames(y) <- NULL
                return(y)
            }
        )
        # compute which clusters have not been covered
        clusters_all <- lapply(index, colnames)
        if(length(clusters_covered) > 1) {
            clusters_all <- clusters_all[names(clusters_covered)]
        }
        clusters_notcovered <- lapply(
            seq_along(clusters_covered), 
            function(i) {
                clusters_all_labs <- clusters_all[[i]]
                paste(
                    clusters_all_labs[!clusters_all_labs %in% clusters_covered[[i]][,1]],
                    collapse = ", "
                )
            }
        )
        names(clusters_notcovered) <- names(clusters_covered)
        # convert matrixes to htmlTables
        clusters_covered <- lapply(clusters_covered, function(x) {htmlTable(x)})
        # create a table containing all results
        if(length(clusters_covered) > 1) {
            ref_names <- names(clusters_covered)
        } else {
            ref_names <- "Uploaded"
        }
        # return all scmapCluster results to values
        res <- data.frame(
            ref_names,
            unlist(assign_rate),
            unlist(clusters_covered),
            unlist(clusters_notcovered)
        )
        colnames(res) <- c("Reference", "% of assigned cells", 
                           "% of cells per Reference Cluster", 
                           "Missed Reference Clusters")
        return(res)
    }
    
    getIndex <- function() {
        if (input$data_type == "existing") {
            # load indexes
            files <- list.files(paste0("www/refs/", input$organism), pattern = ".csv")
            index <- list()
            for (f in files) {
                name <- strsplit(f, "\\.")[[1]][1]
                tmp <- read.csv(
                    paste0(
                        "www/refs/", 
                        input$organism, "/", f
                    )
                )
                rownames(tmp) <- tmp[,1]
                tmp <- tmp[,-1]
                index[[name]] <- tmp
            }
        } else {
            # compute the index
            dataset <- values$reference_data()
            if(values$features) {
                rowData(dataset)$scmap_features <- values$scmap_features
                rowData(dataset)$scmap_scores <- values$scmap_scores
            } else {
                dataset <- selectFeatures(dataset)
            }
            index <- indexCluster(
                dataset, 
                cluster_col = input$pdata_cell_types
            )
            index <- list(metadata(index)$scmap_cluster_index)
        }
        return(index)
    }
    
    scmap <- function() {
        
        # compute index
        index <- getIndex()

        # run scmap-cluster
        scmapCluster_results <- scmapCluster(
            values$projection_data(),
            index_list = index
        )
        
        if(!"SingleCellExperiment" %in% is(scmapCluster_results)) {
            # summarise results of scmap-cluster
            values$scmap_cluster_siml <- scmapCluster_results$scmap_cluster_siml
            values$scmap_cluster_labs <- scmapCluster_results$scmap_cluster_labs
            values$scmap_cluster_comb <- scmapCluster_results$combined_labs
            values$scmap_cluster_all <- 
                scmapClusterResults2table(index, scmapCluster_results$scmap_cluster_labs)
            values$scmap_cluster_combined <- 
                scmapClusterResults2table(index, data.frame(scmapCluster_results$combined_labs))
        } else {
            values$scmap_cluster_worked <- FALSE
        }
        return()
    }
    
    output$datasets <- renderUI({
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$data_type,
               "own" = list(
                   box(width = 12,
                       HTML("<p class='lead'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/SingleCellExperiment' target='_blank'>SingleCellExperiment</a> format</p>
                            <p>(<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/muraro.rds'>example-muraro</a> - a human pancreatic <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/' target='_blank'>dataset</a> with 2126 cells)</p><br>"),
                       fileInput('reference', NULL, accept=c('.rds')),
                       solidHeader = TRUE
                       # status = "primary"
                   ),
                   conditionalPanel("output.reference_file & !output.reference_data",
                        box(width = 12,
                            HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'>Your data is not in <b>SingleCellExperiment</b> format!
                                 Please upload a data in the correct format.</p>
                                 </div>"),
                            solidHeader = TRUE
                        )
                    ),
                   conditionalPanel("output.reference_file & output.reference_data & !output.reference_feature_symbol",
                        box(width = 12,
                            HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'><em>rowData</em> slot of your <b>SingleCellExperiment</b> object does not contain a 
                                 <em>feature_symbol</em> column. Please add it to your object and re-upload it.</p>
                                 </div>"),
                            solidHeader = TRUE
                        )
                    ),
                   conditionalPanel("output.reference_file & output.reference_data & output.reference_feature_symbol", 
                       box(width = 12,
                           HTML("<p class='lead'>Select a column of the <em>colData</em> data slot, containing the cell type annotations:</p>"),
                           selectInput('pdata_cell_types', "", values$ref_pdata_colnames()),
                           solidHeader = TRUE
                           # status = "primary"
                       )
                   ),
                   conditionalPanel("output.reference_file & output.reference_data & output.reference_feature_symbol & !output.pdata_cell_types", 
                        box(width = 12,
                            HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'>The chosen column does not seem to contain cell types annotations.
                                    Please select a correct column of the <em>colData</em> slot with the cell type annotations!</p>
                                 </div>"),
                            solidHeader = TRUE
                        )
                   )
               ),
               "existing" = list(
                   box(width = 12,
                       HTML("<div class='alert alert-success'>
                            <p class='lead'>Your data will be projected to all datasets in our Reference.
                             For more details of the Reference please visit
                             our <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/' target='_blank'>collection of scRNA-seq datasets</a>.</p>
                            </div>"),
                       solidHeader = TRUE
                       # status = "warning"
                   )
               )
        )
    })
    
    output$results_cluster <- renderUI({
        switch(input$data_type,
               "own" = list(
                        box(width = 12,
                            title = "Results",
                            DT::dataTableOutput('results_table'),
                            downloadButton("scmap_cluster_labs", 'Download All Assignments'),
                            downloadButton("scmap_cluster_siml", 'Download All Similarities'),
                            solidHeader = TRUE,
                            status = "success"
                        )
               ),
               "existing" = list(
                   fluidRow(
                       box(width = 12,
                           title = "Individual Results",
                           DT::dataTableOutput('results_table'),
                           downloadButton("scmap_cluster_labs", 'Download All Assignments'),
                           downloadButton("scmap_cluster_siml", 'Download All Similarities'),
                           solidHeader = TRUE,
                           status = "success"
                       ),
                       box(width = 12,
                           title = "Consensus Results",
                           DT::dataTableOutput('consensus_results_table'),
                           downloadButton("consensus_results_assignments", 'Download Consensus Assignments'),
                           solidHeader = TRUE,
                           status = "success"
                       )
                   )
               )
        )
    })

    output$consensus_results_table <- DT::renderDataTable({
        DT::datatable(values$scmap_cluster_combined, escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:2)))
                      ),
                      extensions = 'Buttons')
    })
    
    output$results_table <- DT::renderDataTable({
        scmap()
        DT::datatable(values$scmap_cluster_all, escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:3)))
                      ),
                      extensions = 'Buttons')
    })

    output$feature_table <- DT::renderDataTable({
        DT::datatable(values$feature_table, escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:1)))
                      ),
                      extensions = 'Buttons')
    })

    values$reference_data <- reactive({
        withProgress(message = 'Loading Reference dataset... Please wait...', {
            if (!is.null(input$reference$datapath)) {
                scmap_ref <- readRDS(input$reference$datapath)
                if (class(scmap_ref) == "SingleCellExperiment") {
                    values$features <- FALSE
                    return(scmap_ref)
                } else {
                    return(NULL)
                }
            } else {
                return(NULL)
            }
        })
    })
    
    values$ref_pdata_colnames <- reactive({
        if (!is.null(values$reference_data())) {
            return(as.character(colnames(colData(values$reference_data()))))
        } else {
            return(NULL)
        }
    })
    
    values$projection_data <- reactive({
        withProgress(message = 'Loading Projection dataset... Please wait...', {
            if (!is.null(input$to_project$datapath)) {
                scmap_ref <- readRDS(input$to_project$datapath)
                if (class(scmap_ref) == "SingleCellExperiment") {
                    return(scmap_ref)
                } else {
                    return(NULL)
                }
            } else {
                return(NULL)
            }
        })
    })
    
    output$ref_features <- renderPlot({
        scmap_ref <- selectFeatures(values$reference_data(), suppress_plot = FALSE)
        
        # update reactive variables
        values$features <- TRUE
        values$scmap_features <- rowData(scmap_ref)$scmap_features
        values$scmap_scores <- rowData(scmap_ref)$scmap_scores
        
        row_data <- as.data.frame(rowData(scmap_ref))
        row_data <- row_data[, colnames(row_data) %in% c("feature_symbol", "scmap_features", "scmap_scores")]
        row_data <- row_data[row_data$scmap_features,]
        row_data <- row_data[order(row_data$scmap_scores, decreasing = TRUE), ]
        row_data$scmap_scores <- round(row_data$scmap_scores, 1)
        rownames(row_data) <- NULL
        row_data <- row_data[,c(1,3)]
        colnames(row_data) <- c("Feature Name", "scmap score")
        values$feature_table <- row_data
    })
    
    output$scmap_cluster_labs <- downloadHandler(
        filename = function() {
            paste('scmap_results_assignments.csv', sep='')
        },
        content = function(con) {
            write.csv(values$scmap_cluster_labs, con, quote = FALSE)
        }
    )
    
    output$scmap_cluster_siml <- downloadHandler(
        filename = function() {
            paste('scmap_results_similarities.csv', sep='')
        },
        content = function(con) {
            write.csv(values$scmap_cluster_siml, con, quote = FALSE)
        }
    )
    
    output$consensus_results_assignments <- downloadHandler(
        filename = function() {
            paste('scmap_results_consensus.csv', sep='')
        },
        content = function(con) {
            write.csv(values$scmap_cluster_comb, con, quote = FALSE)
        }
    )
    
    # reactive values needed for the client side
    output$reference_file <- reactive({
        return(!is.null(input$reference$datapath))
    })
    output$projection_file <- reactive({
        return(!is.null(input$to_project$datapath))
    })
    output$reference_data <- reactive({
        values$scmap_cluster_worked <- TRUE
        return(!is.null(values$reference_data()))
    })
    output$projection_data <- reactive({
        values$scmap_cluster_worked <- TRUE
        return(!is.null(values$projection_data()))
    })
    output$reference_feature_symbol <- reactive({
        if (!is.null(values$reference_data())) {
            return(!is.null(rowData(values$reference_data())$feature_symbol))
        } else {
            return(TRUE)
        }
    })
    output$projection_feature_symbol <- reactive({
        if (!is.null(values$projection_data())) {
            return(!is.null(rowData(values$projection_data())$feature_symbol))
        } else {
            return(TRUE)
        }
    })
    output$pdata_cell_types <- reactive({
        if (!is.null(values$reference_data())) {
            return(!(class(colData(values$reference_data())[[input$pdata_cell_types]]) 
                     %in% c("numeric", "integer")))
        } else {
            return(TRUE)
        }
    })
    
    output$features <- reactive({
        return(values$features)
    })
    
    output$scmap_cluster_worked <- reactive({
        return(values$scmap_cluster_worked)
    })
    
    # make output variables visible for the client side
    outputOptions(output, "projection_file", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_file", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_data", suspendWhenHidden = FALSE)
    outputOptions(output, "projection_data", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_feature_symbol", suspendWhenHidden = FALSE)
    outputOptions(output, "projection_feature_symbol", suspendWhenHidden = FALSE)
    outputOptions(output, "pdata_cell_types", suspendWhenHidden = FALSE)
    outputOptions(output, "features", suspendWhenHidden = FALSE)
    outputOptions(output, "scmap_cluster_worked", suspendWhenHidden = FALSE)
}
