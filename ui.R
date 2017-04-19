
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(shiny)

pancreas_datasets <- c(
    "baron-human.rds",
    "baron-mouse.rds",
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

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "scmap"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Reference", tabName = "refs", icon = icon("cloud")),
            menuItem("Feature Selection", tabName = "ref_overview", icon = icon("gears")),
            menuItem("Upload", tabName = "upload", icon = icon("cloud-upload")),
            menuItem("Results", tabName = "results", icon = icon("area-chart"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "refs",
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                        HTML("<h2>Please select a Reference dataset</h2><br>"),
                        box(
                            radioButtons("data_type", "1. Choose Data Type:",
                                               c("Human Pancreas" = "pancreas",
                                                 "Mouse Embryo Development" = "embryo"))
                        ),
                        box(
                            conditionalPanel(
                                condition = "input.data_type == 'pancreas'",
                                selectInput(inputId = "refs_pancreas",
                                            label = "2. Choose Dataset:",
                                            pancreas_datasets)
                            ),
                            conditionalPanel(
                                condition = "input.data_type == 'embryo'",
                                selectInput(inputId = "refs_embryo",
                                            label = "2. Choose Dataset:",
                                            embryo_datasets)
                            )
                        )
                    ),
                    column(width = 1)
                )
            ),
            tabItem(tabName = "ref_overview",
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                           HTML("<h2>Feature selection:</h2><br>"),
                           box(width = 12,
                               radioButtons("n_features",
                                            "Choose the number of selected features:",
                                            choices = c("10", "100", "1000"),
                                            selected = "100",
                                            inline = TRUE)
                           ),
                            box(width = 12,
                                plotOutput("ref_features")
                            )
                    ),
                    column(width = 1)
                )
            ),
            tabItem(tabName = "upload",
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                        HTML("<h2>Please upload your own dataset</h2><br>"),
                        box(width = 12,
                            fileInput('file1', '3. Choose a `scater` object in RDS format:', accept=c('.rds'))
                        )
                    ),
                    column(width = 1)
                )
            ),
            tabItem(tabName = "results",
                    htmlOutput("sankey")
            )
        )
    )
)
