
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)

ui <- dashboardPage(
    
    # skin = "purple",
    dashboardHeader(title = "scmap"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Datasets", tabName = "refs", icon = icon("cloud-upload")),
            menuItem("Features", tabName = "ref_overview", icon = icon("gears")),
            menuItem("Results", tabName = "results", icon = icon("area-chart"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(
            HTML(".shiny-output-error-validation {
                    color: red;
                 }")
            )
        ),
        tags$head(includeScript("google-analytics.js")),
        tabItems(
            tabItem(tabName = "refs",
                fluidRow(
                    box(width = 12,
                        title = "Reference dataset",
                        HTML("1. What would like to do?"),
                        radioButtons("data_type", NULL,
                                    c("Upload your own dataset" = "own",
                                      "Use existing Reference" = "existing"),
                                    selected = "own"),
                        HTML("<hr>"),
                        uiOutput("ui"),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                ),
                fluidRow(
                    box(width = 12,
                        title = "Projection dataset",
                        HTML("Select an <b>.rds</b> file containing data in <a href = 'http://bioconductor.org/packages/scater'>scater</a> format (<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/segerstolpe.rds'>example</a>)<br><br>"),
                        HTML("<font color='#f0ad4e'><b><em>featureData</em></b> 
                             slot of the Projection dataset must have the 
                             <b><em>feature_symbol</em></b> column. This column contains 
                             Feature (gene/transcript) names that will be used in 
                             projecting. See example above.</font><br><br>"),
                        fileInput('to_project', NULL, accept=c('.rds')),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                )
            ),
            tabItem(tabName = "ref_overview",
                fluidRow(
                   box(width = 12,
                       title = "Projection features (genes/transcripts)",
                       HTML("Choose the number of features to be used for projection"),
                       radioButtons("n_features",
                                    NULL,
                                    choices = c("100", "1000"),
                                    selected = "100",
                                    inline = TRUE),
                       
                       plotOutput("ref_features"),
                       solidHeader = TRUE,
                       status = "primary"
                   )
                )
            ),
            tabItem(tabName = "results",
                fluidRow(
                    box(width = 12,
                        title = "Sankey diagram",
                        HTML("<br>"),
                        htmlOutput("sankey"),
                        HTML("<br>"),
                        downloadLink('download_mapping', 'Download Results'),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                )
            )
        )
    )
)
