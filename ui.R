
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)

ui <- tagList( 
dashboardPage(
    skin = "black",
    dashboardHeader(title = "scmap"),
    dashboardSidebar(
        conditionalPanel("input.data_type == 'own'", 
            sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("bank")),
                menuItem("Datasets", tabName = "refs", icon = icon("cloud-upload")),
                menuItem("Features", tabName = "ref_overview", icon = icon("gears")),
                menuItem("Results", tabName = "results", icon = icon("area-chart"))
            )
        ),
        conditionalPanel("input.data_type == 'existing'", 
             sidebarMenu(
                 menuItem("About", tabName = "about", icon = icon("bank")),
                 menuItem("Datasets", tabName = "refs", icon = icon("cloud-upload")),
                menuItem("Results", tabName = "results", icon = icon("area-chart"))
            )
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
                                 <p class = 'lead'>A copy of the <b>scmap</b> manuscript is available on <a href = 'https://doi.org/10.1101/150292' target='_blank'>bioRxiv</a>.<br>
                                 An R package containing <b>scmap</b> source code is available on <a href = 'https://github.com/hemberg-lab/scmap' target='_blank'>GitHub</a>.<br>
                                 More information about the existing Reference can be found <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/' target='_blank'>here</a>.<br>
                                 Please send your feedback/comments/suggestions to <a href='mailto:vladimir.yu.kiselev@gmail.com' target='_blank'>Vladimir Kiselev</a>.</p>
                                 "),
                            solidHeader = TRUE,
                            status = "primary"
                            ),
                        box(width = 12,
                            title = "Notes",
                            # background = "navy",
                            HTML("
                                 <p class = 'lead'><b>scmap</b> is based on <a href = 'https://doi.org/10.1093/bioinformatics/btw777' target='_blank'>scater</a>
                                 format. Please make yourself familiar with it before running <b>scmap</b>.</p>
                                 <p class = 'lead'><b><em>phenoData</em></b> slot of 
                                 the Reference dataset must have the <b><em>cell_type1</em></b> 
                                 column which contains cell type labels.
                                 
                                 <p class = 'lead'><b><em>featureData</em></b> 
                                 slots of both the Reference and Projection dataset must have the 
                                 <b><em>feature_symbol</em></b> column which contains 
                                 Feature (gene/transcript) names from the same organism.</font></p>
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
                        box(width = 12,
                            HTML("<p class='lead'>What would like to do?</p>"),
                            radioButtons("data_type", NULL,
                                        c("Upload your own dataset" = "own",
                                          "Use existing Reference" = "existing"),
                                        selected = "own"),
                            solidHeader = TRUE,
                            status = "primary"
                        ),
                        uiOutput("datasets"),
                        solidHeader = TRUE,
                        status = "primary"
                    ),
                    box(width = 6,
                        title = "Projection",
                        box(width = 12,
                            HTML("<p class='lead'>Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater' target='_blank'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/segerstolpe.rds'>example</a>)</p>"),
                            fileInput('to_project', NULL, accept=c('.rds')),
                            solidHeader = TRUE,
                            status = "primary"
                        ),
                        conditionalPanel("input.data_type == 'existing'",
                        box(width = 12,
                            HTML("<p class='lead'>Which organism is your data from?</p>"),
                            radioButtons("organism", NULL,
                                         c("Human" = "human",
                                           "Mouse" = "mouse"),
                                         selected = "human"),
                            solidHeader = TRUE,
                            status = "primary"
                        )
                        ),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                )
            ),
            tabItem(tabName = "ref_overview",
                    conditionalPanel("output.reference_dataset",
                    fluidRow(
                        box(width = 12,
                            title = "Feature selection",
                            HTML("<p class = 'lead'><b>scmap</b> selects 500 most informative features (genes/transcripts) by
                                 fitting a linear model to the <em>log(expression)</em> 
                                 vs <em>log(dropout)</em> distribution of features.
                                 The most informative genes are shown in red on the plot below. Conceptually,
                                 these genes have a higher dropout rate for a given mean expression.</p>"),
                            solidHeader = TRUE,
                            status = "primary"
                            ),
                        box(width = 12,
                            plotOutput("ref_features"),
                            solidHeader = TRUE
                            # status = "primary"
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
            tabItem(tabName = "results",
                uiOutput("results")
            )
        )
    )
)
)
