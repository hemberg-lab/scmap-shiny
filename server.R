
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

options(shiny.maxRequestSize=200*1024^2)

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    scmap <- function() {
        # run scmap
        if (input$data_type == "existing") {
            # load indexes
            index <- list()
            for (f in values$refs) {
                name <- strsplit(f, "\\.")[[1]][1]
                index[[name]] <- read.table(
                    paste0(
                        "www/refs/", 
                        input$organism, "/", f
                    ), 
                    check.names = FALSE, 
                    sep = "\t"
                )
            }
        } else {
            # compute the index
            index <- indexCluster(
                selectFeatures(values$reference_data()), 
                cluster_col = input$pdata_cell_types
            )
            index <- list(metadata(index)$scmap_cluster_index)
        }
        # main scmap function
        scmapCluster_results <- scmapCluster(
            values$projection_data(),
            index_list = index
        )
        values$all_results_similarities <- scmapCluster_results$scmap_cluster_siml
        values$all_results_assignments <- scmapCluster_results$scmap_cluster_labs
        # compute assignement rate
        assign_rate <- apply(
            scmapCluster_results$scmap_cluster_labs, 
            2, 
            function(x) {
                round(length(x[x != "unassigned"])/length(x) * 100, 1)
            }
        )
        # compute which clusters have been covered
        clusters_covered <- apply(
            scmapCluster_results$scmap_cluster_labs,
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
        clusters_notcovered <- lapply(
            seq_along(clusters_covered), 
            function(i) {
                clusters_all_labs <- clusters_all[[names(clusters_covered)[[i]]]]
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
        to_show <- data.frame(
            names(clusters_covered),
            unlist(assign_rate),
            unlist(clusters_covered),
            unlist(clusters_notcovered)
        )
        colnames(to_show) <- c("Reference", "% of assigned cells", "% of cells per Reference Cluster", "Missed Reference Clusters")
        return(to_show)
    }
    
    output$datasets <- renderUI({
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$data_type,
               "own" = list(
                   box(width = 12,
                       HTML("<p class='lead'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater' target='_blank'>scater</a> format</p>
                            <p>(<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/muraro.rds'>example-muraro</a> - a human pancreatic <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/' target='_blank'>dataset</a> with 2126 cells)</p><br>"),
                       fileInput('reference', NULL, accept=c('.rds')),
                       solidHeader = TRUE
                       # status = "primary"
                   ),
                   conditionalPanel("output.reference_file & !output.reference_data",
                        box(width = 12,
                            HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'>Your data is not in <b>scater</b> format!
                                 Please upload a data in the correct format.</p>
                                 </div>"),
                            solidHeader = TRUE
                        )
                    ),
                   conditionalPanel("output.reference_file & output.reference_data & !output.reference_feature_symbol",
                        box(width = 12,
                            HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'><em>featureData</em> slot of your <b>scater</b> object does not contain a 
                                 <em>feature_symbol</em> column. Please add it to your object and re-upload it.</p>
                                 </div>"),
                            solidHeader = TRUE
                        )
                    ),
                   conditionalPanel("output.reference_file & output.reference_data & output.reference_feature_symbol", 
                       box(width = 12,
                           HTML("<p class='lead'>Select a column of the <em>phenoData</em> data slot, containing the cell type annotations:</p>"),
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
                                    Please select a correct column of the <em>phenoData</em> slot with the cell type annotations!</p>
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
    
    output$results <- renderUI({
        switch(input$data_type,
               "own" = list(
                        box(width = 12,
                            title = "Results",
                            DT::dataTableOutput('results_table'),
                            downloadButton("all_results_assignments", 'Download All Assignments'),
                            downloadButton("all_results_similarities", 'Download All Similarities'),
                            solidHeader = TRUE,
                            status = "success"
                        )
               ),
               "existing" = list(
                   fluidRow(
                       box(width = 12,
                           title = "Individual Results",
                           DT::dataTableOutput('results_table'),
                           downloadButton("all_results_assignments", 'Download All Assignments'),
                           downloadButton("all_results_similarities", 'Download All Similarities'),
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
        DT::datatable(values$consensus_data_table, escape = FALSE, rownames = FALSE,
                      class = 'cell-border stripe',
                      options = list(
                          dom = 'Bfrtip', 
                          buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
                          columnDefs = list(list(className = 'dt-center', targets = c(0:2)))
                      ),
                      extensions = 'Buttons')
    })
    
    output$results_table <- DT::renderDataTable({
        if (input$data_type == "existing") {
            values$refs <- list.files(paste0("www/refs/", input$organism), pattern = ".txt")
        } else {
            values$refs <- "own"
        }
        DT::datatable(scmap(), escape = FALSE, rownames = FALSE,
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
        scmap_ref <- getFeatures(values$reference_data(), suppress_plot = FALSE)
        f_data <- rowData(scmap_ref)
        f_data <- f_data[, colnames(f_data) %in% c("feature_symbol", "scmap_features", "scmap_scores")]
        f_data <- f_data[f_data$scmap_features,]
        f_data <- f_data[order(f_data$scmap_scores, decreasing = TRUE), ]
        f_data$scmap_scores <- round(f_data$scmap_scores, 1)
        rownames(f_data) <- NULL
        f_data <- f_data[,c(1,3)]
        colnames(f_data) <- c("Feature Name", "scmap score")
        values$feature_table <- f_data
    })
    
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
    
    output$consensus_results_assignments <- downloadHandler(
        filename = function() {
            paste('scmap_results_consensus.csv', sep='')
        },
        content = function(con) {
            write.csv(values$consensus_results, con, quote = FALSE)
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
        return(!is.null(values$reference_data()))
    })
    output$projection_data <- reactive({
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
    
    # make output variables visible for the client side
    outputOptions(output, "projection_file", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_file", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_data", suspendWhenHidden = FALSE)
    outputOptions(output, "projection_data", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_feature_symbol", suspendWhenHidden = FALSE)
    outputOptions(output, "projection_feature_symbol", suspendWhenHidden = FALSE)
    outputOptions(output, "pdata_cell_types", suspendWhenHidden = FALSE)
}
