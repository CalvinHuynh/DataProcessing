library(shiny)
library(dplyr)
library(ggvis)
library(plotly)

source("Logic/Plot.R")
df <- getData()
df2 <- getData()

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
  

# Plot 2 ------------------------------------------------------------------
  
  movies <- reactive({
    minYear <- input$year[1]
    maxYear <- input$year[2]
    
    moviesDf <- df %>%
      filter(title_year >= minYear,
             title_year <= maxYear)
    
    if (!is.null(input$titleContains) && input$titleContains != "") {
      containsTitle <- input$titleContains
      moviesDf <- moviesDf %>%
        filter(grepl(containsTitle, title))
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
    nrow(movies())
  })
  
  output$total_movies <- renderText({
    nrow(df)
  })

# Plot 3 ------------------------------------------------------------------

  xAxisValue <- reactive({
    if (input$layer == "3") {
      xAxis <- noquote("no_of_letters")
    } else if (input$layer == "4") {
      xAxis <- noquote("no_of_consonants")
    } else {
      xAxis <- noquote("no_of_vowels")
    }
  })
  
  xAxisValue2 <- reactive({
    if (input$layer == "3") {
      xAxis2 <- df2$no_of_letters
    } else if (input$layer == "4") {
      xAxis2 <- df2$no_of_consonants
    } else {
      xAxis2 <- df2$no_of_vowels
    }
  })
  
  df_layer <- reactive({
    if (input$layer == "3") {
      data <- getTotalLetters()
    } else if (input$layer == "4") {
      data <- getOccurancesConsonants()
    } else {
      data <- getOccurancesVowels()
    }
  })
  
  number_text <- reactive({
    if (input$layer == "3") {
      hoverText <- paste('<b> Number of letters: </b>')
    } else if (input$layer == "4") {
      hoverText <- paste('<b> Number of consonants: </b>')
    } else {
      hoverText <- paste('<b> Number of vowels: </b>')
    }
  })
  
  # minYear <- reactive({
  #   minYear <- input$year2[1]
  # })
  # 
  # maxYear <- reactive({
  #   maxYear <- input$year2[2]
  # })
  
  # dataset <- reactive({
  #   df2 %>%
  #     filter(df2$title_year >= minYear,
  #            df2$title_year <= maxYear)
  # })
  
  output$plot3 <- renderPlotly({
    
    plot_ly() %>%
      add_trace(data = df2, x = xAxisValue2(), y = ~title_year, type = 'scatter', name = 'release year', yaxis = 'y2',
                marker = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste0("<b>", title, "</b><br>",
                                "<b>Year: </b>", title_year, "<br>",
                                "<b>Score: </b>", imdb_score, "<br>",
                                "<b>No of letters: </b>", no_of_letters, "<br>",
                                "<b>No of vowels: </b>", no_of_vowels, "<br>",
                                "<b>No of consonants: </b>", no_of_consonants, "<br>",
                                "<b>Gross:</b> $", format( gross, big.mark = ",", scientific = FALSE))) %>%
      add_trace(data = df_layer() ,x = df_layer()$xAxisValue, y = df_layer()$average_score, type = 'bar', name = 'average score',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(number_text(), df_layer()[,1],
                              '<b></br> Average score: </b>', average_score,
                              '<b></br> Number of titles: </b>',  df_layer()[,3])) %>%
      layout(title = 'Vowels related to score to year',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'score', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'year', showgrid = FALSE, zeroline = FALSE))
  })
  
  
  
  output$test <- renderText({
    xAxisValue()
  })
  
  output$test2 <- renderDataTable({
    head(df_layer(), n = 2)
  })
}
