library(shiny)
library(shinydashboard)
library(leaflet)
library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

ui <- dashboardPage(
  dashboardHeader(title = "Data processing"),
  dashboardSidebar(
    menuItem("Raw data", tabName = "raw", icon = icon("th")),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart"),
             menuSubItem("Score vs vowels", tabName = "chart1"),
             menuSubItem("Interactive plot", tabName = "chart2"),
             menuSubItem("Interactive barplot", tabName = "chart3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "raw",
        fluidRow(
          box(
            title = "Merged dataset Kaggle & movielens", status = "success", solidHeader = TRUE, width = 12,
            div(style = "overflow-x: scroll", dataTableOutput("table1"))
          )
        )
      ),
      tabItem(tabName = "chart1",
              fluidRow(
                box(
                  title = "Controls", width = 5, solidHeader = TRUE, status = "primary",
                  selectInput(inputId = "plot1input",
                              label = "Select a chart",
                              choices = c("Plot Letters" = "0",
                                          "Plot Consonants" = "1",
                                          "Plot Vowels" = "2"
                              ))
                )
              ),
              tabItem(tabName = "chart1",
                      fluidRow(
                        box(plotOutput("plot1", height = 750), width = 12, height = 800)
                      )
              )
      ),
      tabItem(tabName = "chart2",
              titlePanel("Settings"),
              fluidRow(
                column(3,
                       wellPanel(
                         h4("Filter"),
                         sliderInput("year", "Year released", 1916, 2017, value = c(1916, 2017)),
                         numericInput("minGross", "Minimum gross", value = 0, min = 0, step = 100),
                         numericInput("maxGross", "Maximum gross", value = 9999999999, max = 9999999999, step = 100),
                         textInput("titleContains", "Title name contains (e.g., Pirate)")
                       ),
                       wellPanel(
                         selectInput("xvar", "X-axis variable", axis_vars, selected = "imdb_score"),
                         selectInput("yvar", "Y-axis variable", axis_vars, selected = "no_of_vowels")
                       )
                ),
                column(9,
                       ggvisOutput("plot2"),
                       wellPanel(
                         span("Number of movies in selection:",
                              textOutput("n_movies")
                         ), 
                         span("Total movies in dataset:",
                              textOutput("total_movies")
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "chart3",
              fluidRow(
              headerPanel("Data plot"),
              sidebarPanel(
                selectInput(inputId = "layer",
                            label = "Select a layer",
                            choices = c("Plot Letters" = "3",
                                        "Plot Consonants" = "4",
                                        "Plot Vowels" = "5"
                            )),
                # sliderInput("year2", "Year released", 1916, 2017, value = c(1916, 2017)),
                wellPanel(
                  span("Test stuff:",
                       textOutput("test")
                  ),
                  span("Test stuff2:",
                       dataTableOutput("test2")
                  )
                )
                # sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds),
                #             value = 1000, step = 500, round = 0),
                # selectInput('x', 'X', choices = nms, selected = "carat"),
                # selectInput('y', 'Y', choices = nms, selected = "price"),
                # selectInput('color', 'Color', choices = nms, selected = "clarity"),
                # 
                # selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
                # selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
                # sliderInput('plotHeight', 'Height of plot (in pixels)', 
                #             min = 100, max = 2000, value = 1000)
              ),
              mainPanel(
                plotly::plotlyOutput('plot3', height = "900px")
              ))
      )
    )
  )
)
