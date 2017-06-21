
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

refs <- list.files("refs", pattern = ".txt")

## define server global variables
values <- reactiveValues()

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_sankey <- function() {
        scmap_map <- readRDS(input$to_project$datapath)
        
        scmap_ref <- readRDS(values$reference_file)
        scmap_ref <- getFeatures(scmap_ref, n_features = as.numeric(input$n_features))
        
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
    
    get_sankey_ref <- function(ref) {
        # reset the unassigned rate message
        values[[paste0(ref, "-uns")]] <- NULL
        
        print(ref)
        
        ref_data <- read.table(paste0("refs/", ref), check.names = FALSE, sep = "\t")
        scmap_map <- readRDS(input$to_project$datapath)
        
        # main scmap function
        scmap_map <- projectData(
            scmap_map,
            class_ref = ref_data
        )
        
        # quantify the mapping
        
        validate(need(
            pData(scmap_map)$scmap_labs,
            "There are less than ten features in common between the Reference and Projection datasets. Most probably they come from different organisms!"
        ))
        
        labs_new <- pData(scmap_map)$scmap_labs
        
        uns_rate <- length(labs_new[labs_new == "unassigned"])/length(labs_new) * 100
        
        values[[ref]] <- data.frame(cell_ids = rownames(pData(scmap_map)), scmap_assignments = labs_new)
        if (uns_rate > 50) {
            values[[paste0(ref, "-uns")]] <- paste0("<p class='text-danger'><b>", round(uns_rate, 1), "% of the cells were unassigned.</b></p>")
        } else {
            values[[paste0(ref, "-uns")]] <- paste0("<p class='text-success'><b>", round(uns_rate, 1), "% of the cells were unassigned.</b></p>")
        }
        # Sankey diagram
        if (!is.null(pData(scmap_map)$cell_type1)) {
            values[[paste0(ref, "-sank")]] <- NULL
            labs_orig <- as.character(pData(scmap_map)$cell_type1)
            sankey <- getSankey(labs_orig, labs_new)
            return(sankey)
        } else {
            values[[paste0(ref, "-sank")]] <- "<p class='text-danger'>There is no <b><em>cell_type1</em></b> column in the <b><em>phenoData</em></b> slot of your dataset, therefore Sankey diagram cannot be plotted.</p>"
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
                       HTML("<p class='lead'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/muraro.rds'>example</a>)</p>"),
                        fileInput('reference', NULL, accept=c('.rds')),
                       solidHeader = TRUE,
                       status = "primary"
                   )
               ),
               "existing" = list(
                   box(width = 12,
                       HTML("<p class='lead text-warning'>Your data will be projected to all datasets in our Reference.
                             For more details of the Reference please visit
                             our <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/'>collection of scRNA-seq datasets</a>.</p>"),
                       solidHeader = TRUE,
                       status = "primary"
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
                       conditionalPanel("output.projection_dataset",
                       fluidRow(
                           box(width = 6,
                               title = "Sankey diagram",
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
                       conditionalPanel("!output.projection_dataset",
                            fluidRow(
                                column(width = 10,
                                       HTML("
<br><br>
<div class='alert alert-dismissible alert-warning'>
<p class = 'lead'>Looks like you forgot to upload a dataset that you want to 
project to the Reference. Please go back to the <em>Datasets</em> tab and upload 
your <b>Projection</b> dataset.</p>
</div>"),
                                       offset = 1
                                       )
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
                   conditionalPanel("output.projection_dataset",
                   conditionalPanel("input.organism == 'human'",
                   fluidRow(
                       box(width = 12,
                           title = "Human Pancreas",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/' target='_blank'>here</a>.</p>"),
                           # uiOutput("plots"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Baron",
                           uiOutput('baron-human.txt-uns'),
                           uiOutput('baron-human.txt-sank'),
                           uiOutput("human_pancreas_baron"),
                           HTML("<br>"),
                           downloadButton('baron-human.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Muraro",
                           uiOutput('muraro.txt-uns'),
                           uiOutput('muraro.txt-sank'),
                           uiOutput("human_pancreas_muraro"),
                           HTML("<br>"),
                           downloadButton('muraro.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Segerstolpe",
                           uiOutput('segerstolpe.txt-uns'),
                           uiOutput('segerstolpe.txt-sank'),
                           uiOutput("human_pancreas_segerstolpe"),
                           HTML("<br>"),
                           downloadButton('segerstolpe.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Xin",
                           uiOutput('xin.txt-uns'),
                           uiOutput('xin.txt-sank'),
                           uiOutput("human_pancreas_xin"),
                           HTML("<br>"),
                           downloadButton('xin.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Human Tissues",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/human/tissues/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Li",
                           uiOutput('li.txt-uns'),
                           uiOutput('li.txt-sank'),
                           uiOutput("human_tissues_li"),
                           HTML("<br>"),
                           downloadButton('li.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Pollen",
                           uiOutput('pollen.txt-uns'),
                           uiOutput('pollen.txt-sank'),
                           uiOutput("human_tissues_pollen"),
                           HTML("<br>"),
                           downloadButton('pollen.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   )
                   ),
                   conditionalPanel("input.organism == 'mouse'",
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Brain",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/brain/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Tasic (reads)",
                           uiOutput('tasic-reads.txt-uns'),
                           uiOutput('tasic-reads.txt-sank'),
                           uiOutput("mouse_brain_tasic_reads"),
                           HTML("<br>"),
                           downloadButton('tasic-reads.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Tasic (rpkms)",
                           uiOutput('tasic-rpkms.txt-uns'),
                           uiOutput('tasic-rpkms.txt-sank'),
                           uiOutput("mouse_brain_tasic_rpkms"),
                           HTML("<br>"),
                           downloadButton('tasic-rpkms.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Usoskin",
                           uiOutput('usoskin.txt-uns'),
                           uiOutput('usoskin.txt-sank'),
                           uiOutput("mouse_brain_usoskin"),
                           HTML("<br>"),
                           downloadButton('usoskin.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Zeisel",
                           uiOutput('zeisel.txt-uns'),
                           uiOutput('zeisel.txt-sank'),
                           uiOutput("mouse_brain_zeisel"),
                           HTML("<br>"),
                           downloadButton('zeisel.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Retina",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/retina/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                       )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Macosko",
                           uiOutput('macosko.txt-uns'),
                           uiOutput('macosko.txt-sank'),
                           uiOutput("mouse_retina_macosko"),
                           HTML("<br>"),
                           downloadButton('macosko.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Shekhar",
                           uiOutput('shekhar.txt-uns'),
                           uiOutput('shekhar.txt-sank'),
                           uiOutput("mouse_retina_shekhar"),
                           HTML("<br>"),
                           downloadButton('shekhar.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Pancreas",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/pancreas/' target='_blank'>here</a>.</p>"),
                           # uiOutput("plots"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Baron",
                           uiOutput('baron-mouse.txt-uns'),
                           uiOutput('baron-mouse.txt-sank'),
                           uiOutput("mouse_pancreas_baron"),
                           HTML("<br>"),
                           downloadButton('baron-mouse.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Retina",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/esc/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Klein",
                           uiOutput('klein.txt-uns'),
                           uiOutput('klein.txt-sank'),
                           uiOutput("mouse_embryo_stem_cell_klein"),
                           HTML("<br>"),
                           downloadButton('klein.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Kolodziejczyk",
                           uiOutput('kolodziejczyk.txt-uns'),
                           uiOutput('kolodziejczyk.txt-sank'),
                           uiOutput("mouse_embryo_stem_cell_kolodziejczyk"),
                           HTML("<br>"),
                           downloadButton('kolodziejczyk.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Embryo Development",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/edev/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Deng (reads)",
                           uiOutput('deng-reads.txt-uns'),
                           uiOutput('deng-reads.txt-sank'),
                           uiOutput("mouse_embryo_devel_deng_reads"),
                           HTML("<br>"),
                           downloadButton('deng-reads.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       ),
                       box(width = 6,
                           title = "Deng (rpkms)",
                           uiOutput('deng-rpkms.txt-uns'),
                           uiOutput('deng-rpkms.txt-sank'),
                           uiOutput("mouse_embryo_devel_deng_rpkms"),
                           HTML("<br>"),
                           downloadButton('deng-rpkms.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   ),
                   fluidRow(
                       box(width = 12,
                           title = "Mouse Hematopoietic Stem Cells",
                           HTML("
                                <p>More information about the datasets is available <a href='https://hemberg-lab.github.io/scRNA.seq.datasets/mouse/hsc/' target='_blank'>here</a>.</p>"),
                           solidHeader = TRUE,
                           status = "success"
                           )
                   ),
                   fluidRow(
                       box(width = 6,
                           title = "Nestorowa",
                           uiOutput('nestorowa.txt-uns'),
                           uiOutput('nestorowa.txt-sank'),
                           uiOutput("mouse_hematopoietic_stem_cells_nestorowa"),
                           HTML("<br>"),
                           downloadButton('nestorowa.txt', 'Download Results'),
                           solidHeader = TRUE,
                           status = "primary"
                       )
                   )
                   )
                   ),
                   conditionalPanel("!output.projection_dataset",
                                    fluidRow(
                                        column(width = 10,
                                            HTML("
<br><br>
<div class='alert alert-dismissible alert-warning'>
<p class = 'lead'>Looks like you forgot to upload a dataset that you want to 
project to the Reference. Please go back to the <em>Datasets</em> tab and upload 
your <b>Projection</b> dataset.</p>
</div>"),
                                            offset = 1
                                        )
                                    )
                   )
               )
          )
    })
    
    output$human_pancreas_baron <- renderGvis({
        withProgress(message = 'Projecting to Baron (human)', {
            incProgress()
            get_sankey_ref("baron-human.txt")
        })
    })
    
    output$mouse_pancreas_baron <- renderGvis({
        withProgress(message = 'Projecting to Baron (mouse)', {
            incProgress()
            get_sankey_ref("baron-mouse.txt")
        })
    })
    
    output$mouse_embryo_devel_deng_reads <- renderGvis({
        withProgress(message = 'Projecting to Deng (reads)', {
            incProgress()
            get_sankey_ref("deng-reads.txt")
        })
    })
    
    output$mouse_embryo_devel_deng_rpkms <- renderGvis({
        withProgress(message = 'Projecting to Deng (rpkms)', {
            incProgress()
            get_sankey_ref("deng-rpkms.txt")
        })
    })
    
    output$mouse_embryo_stem_cell_klein <- renderGvis({
        withProgress(message = 'Projecting to Klein', {
            incProgress()
            get_sankey_ref("klein.txt")
        })
    })
    
    output$mouse_embryo_stem_cell_kolodziejczyk <- renderGvis({
        withProgress(message = 'Projecting to Kolodziejczyk', {
            incProgress()
            get_sankey_ref("kolodziejczyk.txt")
        })
    })
    
    output$human_tissues_li <- renderGvis({
        withProgress(message = 'Projecting to Li', {
            incProgress()
            get_sankey_ref("li.txt")
        })
    })
    
    output$mouse_retina_macosko <- renderGvis({
        withProgress(message = 'Projecting to Macosko', {
            incProgress()
            get_sankey_ref("macosko.txt")
        })
    })
    
    output$human_pancreas_muraro <- renderGvis({
        withProgress(message = 'Projecting to Muraro', {
            incProgress()
            get_sankey_ref("muraro.txt")
        })
    })
    
    output$mouse_hematopoietic_stem_cells_nestorowa <- renderGvis({
        withProgress(message = 'Projecting to Nestorowa', {
            incProgress()
            get_sankey_ref("nestorowa.txt")
        })
    })
    
    output$human_tissues_pollen <- renderGvis({
        withProgress(message = 'Projecting to Pollen', {
            incProgress()
            get_sankey_ref("pollen.txt")
        })
    })
    
    output$human_pancreas_segerstolpe <- renderGvis({
        withProgress(message = 'Projecting to Segerstolpe', {
            incProgress()
            get_sankey_ref("segerstolpe.txt")
        })
    })
    
    output$mouse_retina_shekhar <- renderGvis({
        withProgress(message = 'Projecting to Shekhar', {
            incProgress()
            get_sankey_ref("shekhar.txt")
        })
    })
    
    output$mouse_brain_tasic_reads <- renderGvis({
        withProgress(message = 'Projecting to Tasic (reads)', {
            incProgress()
            get_sankey_ref("tasic-reads.txt")
        })
    })
    
    output$mouse_brain_tasic_rpkms <- renderGvis({
        withProgress(message = 'Projecting to Tasic (rpkms)', {
            incProgress()
            get_sankey_ref("tasic-rpkms.txt")
        })
    })
    
    output$mouse_brain_usoskin <- renderGvis({
        withProgress(message = 'Projecting to Usoskin', {
            incProgress()
            get_sankey_ref("usoskin.txt")
        })
    })
    
    output$human_pancreas_xin <- renderGvis({
        withProgress(message = 'Projecting to Xin', {
            incProgress()
            get_sankey_ref("xin.txt")
        })
    })
    
    output$mouse_brain_zeisel <- renderGvis({
        withProgress(message = 'Projecting to Zeisel', {
            incProgress()
            get_sankey_ref("zeisel.txt")
        })
    })
    
    observe({
        values$reference_file <- input$reference$datapath
        print(values$projection_file)
        # download buttons
        lapply(refs, function(i) {
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

    outputOptions(output, "projection_dataset", suspendWhenHidden = FALSE)
    outputOptions(output, "reference_dataset", suspendWhenHidden = FALSE)
}
