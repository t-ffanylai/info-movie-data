# Setup
library("dplyr")
library("shiny")
library("ggplot2")
library("maps")
library("maptools")


# Reads movie data from tmdb
setwd('~/info-movie-data')
movie.data <- read.csv('./data/tmdb_5000_movies.csv', stringsAsFactors = FALSE)

# Defines server for the movie revenue data app
server <- function(input, output) {
  options(scipen=8)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Creates a scatterplot of the movie budget by its revenue and draws a best line of fit
  output$budget.plot <- renderPlot({
    ggplot(movie.data, aes(x = budget, y = revenue)) + 
      geom_jitter() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_smooth(mapping = aes(x = budget, y = revenue), color = "RED") +
      labs(title = "Movie Budget by Revenue", 
           x = "Budget (in US Dollars)",  
           y = "Revenue (in US Dollars)") + 
      theme(title = element_text(size=16))
    
  })
  
  # Allows brushing over and double clicking to zoom into budget vs. revenue scatterplot
  # If nothing is brushed over, but is double clicked, plot zooms out
  observeEvent(input$budget_dblclick, {
    brush <- input$budget_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Returns a summary of the movie data, including the mean, average, 
  # and linear correlation coefficient of the budget and revenue
  output$budget_summary <- renderText({
    summary <- summarize(movie.data,
                         cor = round(cor(budget, revenue), 3),
                         mean.budget = round(mean(budget), 2),
                         mean.revenue = round(mean(revenue), 2),
                         median.budget = round(median(budget), 2),
                         median.revenue = round(median(revenue), 2)
                         )
    return(paste0("The average budget is $", summary$mean.budget, ".", sep = "\n",
                  "The average revenue is $", summary$mean.revenue, ".", sep = "\n",
                  "The median budget is $", summary$median.budget, ".", sep = "\n",
                  "The median revenue is $", summary$median.revenue, ".", sep = "\n",
                  "The linear correlation between movie budget and revenue is ", summary$cor, "."))
  })
  
  # Prints information about the movie title, budget, and revenue of the hovered point 
  # on the budget vs. revenue plot
  output$budget_info <- renderPrint({
    filtered.data <- select(movie.data, title, budget, revenue)
    budget.filtered.data <- nearPoints(filtered.data, input$budget_hover, 
                                       maxpoints = 1,
                                       xvar = "budget", yvar = "revenue")
    if(dim(budget.filtered.data[0]) != 0) {
      print(budget.filtered.data, row.names = FALSE)
    }
  })
  
  # filters the data by original language to find the mean and median revenue of each language
  language.stats <- reactive ({
    language.summary <- group_by(movie.data, original_language) %>%
      summarize(
        mean = mean(revenue),
        median = median(revenue),
        n = n()
      ) %>%
      filter(original_language == input$language.choice)
    return(paste0("There were ", language.summary[1,4], " movies filmed in this language.",
                  " The mean for the selected language was ", language.summary[1,2], 
                  ". The median revenue for the selected language was ", language.summary[1,3],
                  ". The mean and median are measured in U.S. dollar."))
  })
  
  # renders the revenue summary of the selected language
  output$language.summary <- renderText({
    return(language.stats())
  })
  
  # filters the data by the input original language to plot on a graph
  by.language <- reactive ({
    chosen.language <- movie.data %>%
      filter(original_language == "zh") %>%
      select(original_language, title, revenue)
    return(chosen.language)
  })
  
  output$language.plot <- renderPlot({
    
  })
}