library(shinydashboard)
library(shiny)

ui <- tagList( 
dashboardPage(
    skin = "black",
    dashboardHeader(title = "scmap"),
    dashboardSidebar(
        conditionalPanel("input.data_type == 'own'", 
            conditionalPanel("!(output.reference_file & output.reference_data & output.reference_feature_symbol & output.pdata_cell_types & output.projection_file & output.projection_data & output.projection_feature_symbol)",
                 sidebarMenu(
                     menuItem("About", tabName = "about", icon = icon("bank")),
                     menuItem("Datasets", tabName = "datasets", icon = icon("cloud-upload"))
                 )
            ),
            conditionalPanel("output.reference_file & output.reference_data & output.reference_feature_symbol & output.pdata_cell_types & output.projection_file & output.projection_data & output.projection_feature_symbol",
                  sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("bank")),
                      menuItem("Datasets", tabName = "datasets", icon = icon("cloud-upload")),
                      menuItem("Features", tabName = "features", icon = icon("gears")),
                      menuItem("Results", tabName = "results", icon = icon("area-chart"))
                  )
            )
        ),
        conditionalPanel("input.data_type == 'existing'",
             conditionalPanel("!(output.projection_file & output.projection_data & output.projection_feature_symbol)",
                  sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("bank")),
                      menuItem("Datasets", tabName = "datasets", icon = icon("cloud-upload"))
                  )
             ),
             conditionalPanel("output.projection_file & output.projection_data & output.projection_feature_symbol",
                  sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("bank")),
                      menuItem("Datasets", tabName = "datasets", icon = icon("cloud-upload")),
                      menuItem("Results", tabName = "results", icon = icon("area-chart"))
                  )
             )
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
            tags$style(
            HTML(".shiny-output-error-validation {
                    color: red;
                }")
            )
        ),
        tags$head(includeScript("google-analytics.js")),
        tabItems(
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12,
                            title = "About",
                            HTML("<p>Single-cell RNA-seq (scRNA-seq) is 
                                 widely used to investigate the composition of complex 
                                 tissues. However, it is often challenging to directly 
                                 compare the cells identified in two different experiments. 
                                 <b>scmap</b> allows you to project cells from an scRNA-seq 
                                 experiment (the <em>Projection</em>) on to the cell-types 
                                 identified in a different experiment (the <em>Reference</em>).</p>"),
                            solidHeader = TRUE,
                            status = "primary"
                        ),
                        box(width = 12,
                            title = "Links",
                            HTML("<p>A copy of the <b>scmap</b> 
                                 manuscript is available on 
                                 <a href = 'https://doi.org/10.1101/150292' target='_blank'>bioRxiv</a>.
                                 <br>An R package containing <b>scmap</b> source code 
                                 is available on 
                                 <a href = 'https://github.com/hemberg-lab/scmap' target='_blank'>GitHub</a>.
                                 <br>More information about the existing Reference 
                                 can be found 
                                 <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/' target='_blank'>here</a>.
                                 <br>Please send your feedback/comments/suggestions to 
                                 <a href='mailto:vladimir.yu.kiselev@gmail.com' target='_blank'>Vladimir Kiselev</a>.</p>"),
                            solidHeader = TRUE,
                            status = "primary"
                        ),
                        box(width = 12,
                            title = "Notes",
                            HTML("<p><b>scmap</b> is based on 
                                 <a href = 'https://doi.org/10.1093/bioinformatics/btw777' target='_blank'>scater</a>
                                 format. Please make yourself familiar with it 
                                 before running <b>scmap</b>.</p>
                                 <p><b><em>featureData</em></b> 
                                 slots of both the Reference and Projection dataset must have the 
                                 <b><em>feature_symbol</em></b> column which contains 
                                 Feature (gene/transcript) names from the same organism.</font></p>"),
                            solidHeader = TRUE,
                            status = "warning"
                        )
                   )
            ),
            tabItem(tabName = "datasets",
                fluidRow(
                    box(width = 6,
                        title = "Reference",
                        box(width = 12,
                            HTML("<p class='lead'>What would like to do?</p>"),
                            radioButtons(
                                "data_type", 
                                NULL,
                                c("Upload your own dataset" = "own",
                                  "Use existing Reference" = "existing"),
                                selected = "own"),
                            solidHeader = TRUE
                        ),
                        uiOutput("datasets"),
                        solidHeader = TRUE,
                        status = "primary"
                    ),
                    box(width = 6,
                        title = "Projection",
                        box(width = 12,
                            HTML("<p class='lead'>Select an <b>.rds</b> file 
                                 containing data in 
                                 <a href = 'http://bioconductor.org/packages/scater' target='_blank'>scater</a> 
                                 format</p><p>(<a href='https://scrnaseq-public-datasets.s3.amazonaws.com/scater-objects/segerstolpe.rds'>example-segerstolpe</a> 
                                 - a human pancreatic <a href = 'https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/' target='_blank'>dataset</a> 
                                 with 3514 cells)</p><br>"),
                            fileInput('to_project', NULL, accept=c('.rds')),
                            solidHeader = TRUE
                        ),
                        conditionalPanel("input.data_type == 'existing'",
                            box(width = 12,
                                HTML("<p class='lead'>What organism is your data from?</p>"),
                                radioButtons("organism", NULL,
                                             c("Human" = "human",
                                               "Mouse" = "mouse"),
                                             selected = "human"),
                                solidHeader = TRUE
                            )
                        ),
                        conditionalPanel("output.projection_file & !output.projection_data",
                             box(width = 12,
                                 HTML("
                                 <div class='alert alert-danger'>
                                 <p class = 'lead'>Your data is not in <b>scater</b> format!
                                 Please upload a data in the correct format.</p>
                                 </div>"),
                                 solidHeader = TRUE
                             )
                        ),
                        conditionalPanel("output.projection_file & output.projection_data & !output.projection_feature_symbol",
                             box(width = 12,
                                 HTML("
                                      <div class='alert alert-danger'>
                                      <p class = 'lead'><em>featureData</em> slot of your <b>scater</b> object does not contain a 
                                      <em>feature_symbol</em> column. Please add it to your object and re-upload it.</p>
                                      </div>"),
                                 solidHeader = TRUE
                             )
                        ),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                )
            ),
            tabItem(tabName = "features",
                fluidRow(
                    box(width = 12,
                        title = "Feature Selection",
                        HTML("<p><b>scmap</b> selects 500 most 
                             informative features (genes/transcripts) of the 
                             <b>Reference</b> dataset by
                             fitting a linear model to the <em>log(expression)</em> 
                             vs <em>log(dropout)</em> distribution of features.
                             The most informative features are shown in red on the 
                             plot below. Conceptually, these features have a higher 
                             dropout rate for a given mean expression. 
                             More information about the selected features is 
                             provided in the table below the plot. <em>scmap scores</em> 
                             are defined as the residuals of the linear model.</p>"),
                        solidHeader = TRUE,
                        status = "primary"
                    )
                ),
                fluidRow(
                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                         box(width = 12,
                             plotOutput("ref_features"),
                             solidHeader = TRUE,
                             status = "success"
                         ),
                         box(width = 12,
                             title = "Selected Features",
                             DT::dataTableOutput('feature_table'),
                             solidHeader = TRUE,
                             status = "success"
                         )
                    ),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         column(width = 8,
                         tags$div(
                            HTML("<br><br><br><br>
                                 <div class='progress progress-striped active'>
                                    <div class='progress-bar' style='width: 100%'>
                                    Selecting features...</div>
                                 </div>")),
                         offset = 2
                        )
                    )
                )
            ),
            tabItem(tabName = "results",
                fluidRow(
                    conditionalPanel("input.data_type == 'own'",
                         box(width = 12,
                             title = "Cell Projection",
                             HTML("<p><b>scmap</b> projects all cells
                                  of the Projection dataset to the
                                  reference calculated from the Reference dataset. 
                                  The Reference is computed by calculating
                                  the median expression in each of 500 
                                  selected features across all
                                  cells in each cell type."),
                             solidHeader = TRUE,
                             status = "primary"
                         )
                    ),
                    conditionalPanel("input.data_type == 'existing'",
                        box(width = 12,
                            title = "Cell Projection",
                            HTML("<p><b>scmap</b> projects all cells
                                 of the Projection dataset to the precomputed
                                 References. The references were computed by
                                 selecting 500 most informative features of each
                                 Reference dataset and calculating
                                 the median expression in each of 500 features across all
                                 cells in each cell type."),
                            solidHeader = TRUE,
                            status = "primary"
                        )
                    )
                ),
                conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                    uiOutput("results")
                ),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                    fluidRow(
                        column(width = 8,
                            tags$div(
                                HTML("<br><br><br><br>
                                     <div class='progress progress-striped active'>
                                        <div class='progress-bar' style='width: 100%'>
                                        Projecting cells...</div>
                                     </div>")), 
                            offset = 2)
                    )
                )
            )
        )
    )
))
