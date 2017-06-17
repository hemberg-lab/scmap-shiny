
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)
library(plotly)

ui <- tagList( 
dashboardPage(
    skin = "black",
    dashboardHeader(title = "scmap"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("bank")),
            menuItem("Datasets", tabName = "refs", icon = icon("cloud-upload")),
            menuItem("Features", tabName = "ref_overview", icon = icon("gears")),
            menuItem("Results", tabName = "results", icon = icon("area-chart"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
            tags$style(
            HTML(".shiny-output-error-validation {
                    color: red;
                 }
                 ")
            )
        ),
        tags$head(includeScript("google-analytics.js")),
        tabItems(
            
            tabItem(tabName = "about",
                    fluidRow(
                        
                        box(width = 12,
                            title = "About",
                            # background = "navy",
                            HTML("<p class='lead'>Single-cell RNA-seq (scRNA-seq) is 
                                 widely used to investigate the composition of complex tissues. However, it is often challenging to directly compare the cells 
                                 identified in two different experiments. <b>scmap</b> allows to 
                                 project cells from a scRNA-seq experiment (<em>projection</em>) on to the cell-types 
                                 identified in a different experiment (<em>reference</em>).</p>"),
                            solidHeader = TRUE,
                            status = "primary"
                            ),
                        box(width = 12,
                            title = "Links",
                            HTML("
                                 <p class = 'lead'>A copy of the <b>scmap</b> manuscript is available on <a href = 'https://doi.org/10.1101/150292'>bioRxiv</a>.<br>
                                 An R package containing <b>scmap</b> source code is available on <a href = 'https://github.com/hemberg-lab/scmap'>GitHub</a>.<br>
                                 More information about the existing Reference can be found <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/'>here</a>.<br>
                                 Please send your feedback/comments/suggestions to <a href='mailto:vladimir.yu.kiselev@gmail.com'>Vladimir Kiselev</a>.</p>
                                 "),
                            solidHeader = TRUE,
                            status = "primary"
                            ),
                        box(width = 12,
                            title = "Notes",
                            # background = "navy",
                            HTML("
                                 <p class = 'lead'><b>scmap</b> is based on <a href = 'https://doi.org/10.1093/bioinformatics/btw777'>scater</a>
                                 format. Please make yourself familiar with it before running <b>scmap</b>.</p>
                                 <p class = 'lead'><b><em>phenoData</em></b> slot of 
                                 the Reference dataset must have the <b><em>cell_type1</em></b> 
                                 column which contains cell type labels.
                                 
                                 <p class = 'lead'><b><em>featureData</em></b> 
                                 slot of the Projection dataset must have the 
                                 <b><em>feature_symbol</em></b> column which contains 
                                 Feature (gene/transcript) names.</font></p>
                                 "),
                            solidHeader = TRUE,
                            status = "warning"
                            )
                    )
            ),
            
            tabItem(tabName = "refs",
                fluidRow(
                    box(width = 6,
                        title = "Reference",
                        HTML("<div class='panel panel-primary'>
                                <div class='panel-heading'>What would like to do?</div>
                                <div class='panel-body'>"),
                        radioButtons("data_type", NULL,
                                    c("Upload your own dataset" = "own",
                                      "Use existing Reference" = "existing"),
                                    selected = "own"),
                        HTML("</div></div>"),
                        uiOutput("datasets"),
                        solidHeader = TRUE
                        # status = "primary"
                    ),
                    box(width = 6,
                        title = "Projection",
                        HTML("<div class='panel panel-primary'>
                                <div class='panel-heading'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/segerstolpe.rds'>example</a>)</div>
                             
                             <div class='panel-body'>"),
                        fileInput('to_project', NULL, accept=c('.rds')),
                        HTML("</div></div>"),
                        solidHeader = TRUE
                        # status = "primary"
                    )
                )
            ),
            tabItem(tabName = "ref_overview",
                uiOutput("features")
            ),
            tabItem(tabName = "results",
                uiOutput("results")
            )
        )
    )
)
)
