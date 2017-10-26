library(shiny)
library(dplyr)
library(ggvis)

source("Logic/Plot.R")
df <- getData()

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  output$table1 <- renderDataTable({
    return(df)
  })
  
  output$plot1 <- renderPlot({
    if (input$plot1input == "0") {
      barPlotLetters()
    } else if (input$plot1input == "1") {
      barPlotConsonants()
    } else {
      barPlotVowels()
    }
  })
  
  movies <- reactive({
    minYear <- input$year[1]
    maxYear <- input$year[2]
    
    moviesDf <- df %>%
      filter(title_year >= minYear,
             title_year <= maxYear)
    
    if (!is.null(input$titleContains) && input$titleContains != "") {
      containsTitle <- input$titleContains
      moviesDf <- moviesDf %>% filter(containsTitle, title, fixed = TRUE)
    }
    
    if (!is.null(input$minGross)) {
      minGross <- isolate({input$minGross})
      maxGross <- 1000000000
      
      if (!is.null(input$maxGross)) {
        maxGross <- isolate({input$maxGross})
      }
      
      moviesDf <- moviesDf %>%
        filter(!is.na(gross)) %>%
        filter(gross >= minGross,
               gross <= maxGross)
    }
  })
  
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$title)) return(NULL)
    
    # Pick out the movie with this title
    df <- isolate(movies())
    movie <- df[df$title == x$title, ]
    
    paste0("<b>", movie$title, "</b><br>",
           "<b>Year: \t</b>",movie$title_year, "<br>",
           "<b>No of letters: \t</b>",movie$no_of_letters, "<br>",
           "<b>No of vowels: \t</b>",movie$no_of_vowels, "<br>",
           "<b>No of consonants: \t</b>",movie$no_of_consonants, "<br>",
           "<b>Gross:</b> $", format(movie$gross, big.mark = ",", scientific = FALSE)
    )
  }
  
  interactivePlot <- reactive({
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    movies %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~content_rating, key := ~title) %>%
      add_tooltip(movie_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 800, height = 500)
  })
  
  interactivePlot %>% bind_shiny("plot2")
  
  output$n_movies <- renderText({
    nrow(movies)
  })
  
  output$total_movies <- renderText({
    nrow(df)
  })
}
