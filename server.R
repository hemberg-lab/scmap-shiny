
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)
library(scater)
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

            d <- as.data.frame(d, stringsAsFactors = FALSE)
            d[,2] <- round(as.numeric(d[,2]), 1)
            d <- d[order(d[,2], decreasing = TRUE), ]
            colnames(d) <- c("Reference", "% of assigned cells", "% of cells per Reference Cell Type", "Missed Reference Cell Types")
            
            res_assign <- as.data.frame(res_assign)
            colnames(res_assign) <- names
            rownames(res_assign) <- values[[f]]$cell_ids
            res_assign <- res_assign[ , d$Reference, drop = FALSE]
            values$all_results_assignments <- res_assign
            
            res_siml <- as.data.frame(res_siml)
            colnames(res_siml) <- names
            rownames(res_siml) <- values[[f]]$cell_ids
            res_siml <- res_siml[ , d$Reference, drop = FALSE]
            values$all_results_similarities <- res_siml
            
            # calculate consensus projection
            if (nrow(res_assign) > 0) {
                logicals <- lapply(as.list(as.data.frame(t(res_siml))), function(x){
                    # !is.na(x) & x > 0.7
                    tmp <- which.max(x)
                    if (length(tmp) != 0) {
                        if (x[tmp] >= 0.7) {
                            return(tmp)
                        } else {
                            return(NA)
                        }
                    } else {
                        return(NA)
                    }
                })
                
                consensus_labs1 <- mapply(function(x, y) {
                        if (!is.na(y)) {
                            tmp <- as.character(x)[y]
                            names(tmp) <- names(x)[y]
                            return(tmp)
                        } else {
                            return(NA)
                        }
                    },
                    as.list(as.data.frame(t(res_assign))),
                    logicals
                )
                
                # consensus_labs2 <- lapply(consensus_labs1, function(x) {
                #         if (length(unique(x)) == 1) {
                #             return(x[1])
                #         } else {
                #             return("unassigned")
                #         }
                #     }
                # )
                consensus_labs1 <- unlist(consensus_labs1)
                consensus_labs1[is.na(consensus_labs1)] <- "unassigned"
                tmp <- as.data.frame(unlist(consensus_labs1))
                colnames(tmp) <- "scmap_consensus_cell_types"
                values$consensus_results <- tmp
                
                # visualise consensus results in a data table
                scmap_labs <- unlist(consensus_labs1)
                assign_rate <- 
                    length(scmap_labs[scmap_labs != "unassigned"])/length(scmap_labs) * 100
                assign_rate <- round(as.numeric(assign_rate), 1)
                covered_cell_types <- as.data.frame(table(scmap_labs))
                covered_cell_types <- 
                    covered_cell_types[order(covered_cell_types[,2], decreasing = TRUE), ]
                covered_cell_types <- covered_cell_types[covered_cell_types[,1] != "unassigned", ]
                covered_cell_types[,2] <- round(covered_cell_types[,2]/length(scmap_labs) * 100, 1)
                rownames(covered_cell_types) <- NULL
                colnames(covered_cell_types) <- NULL
                
                tmp <- NULL
                tmp <- rbind(tmp, c("Consensus", assign_rate, htmlTable(covered_cell_types)))
                colnames(tmp) <- c("Reference", "% of assigned cells", "% of cells per Reference Cell Type")
                values$consensus_data_table <- as.data.frame(tmp)
            } else {
                tmp <- NULL
                values$consensus_data_table <- as.data.frame(rbind(tmp, c("Consensus", 0, "There are less than ten features in common between the Reference and Projection datasets. Most probably they come from different organisms!")))
            }
            return(d)
        })
    }
    
    projectOne <- function(ref) {
        if (input$data_type == "existing") {
            ref_data <- read.table(paste0("www/refs/", input$organism, "/", ref), check.names = FALSE, sep = "\t")
            
            # main scmap function
            scmap_map <- projectData(
                values$projection_data(),
                ref_data
            )
        } else {
            # main scmap function
            scmap_map <- projectData(
                values$projection_data(),
                getFeatures(values$reference_data()),
                cell_type_column = input$pdata_cell_types
            )
            ref <- "own"
            ref_data <- unique(as.character(pData(values$reference_data())[[input$pdata_cell_types]]))
        }
        
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
            if (input$data_type == "existing") {
                not_covered_cell_types <- colnames(ref_data)[!colnames(ref_data) %in% unique(as.character(pData(scmap_map)$scmap_labs))]
            } else {
                not_covered_cell_types <- ref_data[!ref_data %in% unique(as.character(pData(scmap_map)$scmap_labs))]
            }
            values[[paste0(ref, "-ref-cell-types")]] <- list(covered_cell_types = covered_cell_types, not_covered_cell_types = not_covered_cell_types)
        } else {
            values[[paste0(ref)]] <- NULL
            values[[paste0(ref, "-ref-cell-types")]] <- NULL
        }
        return(NULL)
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
                if (class(scmap_ref) == "SCESet") {
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
            return(as.character(colnames(pData(values$reference_data()))))
        } else {
            return(NULL)
        }
    })
    
    values$projection_data <- reactive({
        withProgress(message = 'Loading Projection dataset... Please wait...', {
            if (!is.null(input$to_project$datapath)) {
                scmap_ref <- readRDS(input$to_project$datapath)
                if (class(scmap_ref) == "SCESet") {
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
            return(!is.null(fData(values$reference_data())$feature_symbol))
        } else {
            return(TRUE)
        }
    })
    output$projection_feature_symbol <- reactive({
        if (!is.null(values$projection_data())) {
            return(!is.null(fData(values$projection_data())$feature_symbol))
        } else {
            return(TRUE)
        }
    })
    output$pdata_cell_types <- reactive({
        if (!is.null(values$reference_data())) {
            return(!(class(pData(values$reference_data())[[input$pdata_cell_types]]) 
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
