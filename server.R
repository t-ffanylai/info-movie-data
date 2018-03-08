# Setup
library("dplyr")
library("shiny")
library("ggplot2")
library("maptools")
library("jsonlite")
library("ISOcodes")
library("purrr")
library("tidyr")

# Reads movie data from tmdb
movie.data <- read.csv('./data/tmdb_5000_movies.csv', stringsAsFactors = FALSE)
movie.countries <- movie.data[c("original_title", "production_countries", "revenue")]

# Parses JSON format of the production countries into a dataframe
movie.countries <- movie.countries %>% 
  mutate(production_countries = map(production_countries, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest() 
map.world = ggplot2::map_data("world")

# Defines server for the movie revenue data app
server <- function(input, output) {
  options(scipen=8)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Creates a scatterplot of the movie budget by its revenue and draws a best line of fit
  output$budget.plot <- renderPlot({
    budget.movie.filter <- movie.data %>%
      filter(budget > input$budget[1] & budget < input$budget[2]) %>%
      filter(revenue > input$revenue[1] & revenue < input$revenue[2])
    ggplot(budget.movie.filter, aes(x = budget, y = revenue)) + 
      geom_jitter() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_smooth(mapping = aes(x = budget, y = revenue), color = "#21D17A") +
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
    budget.movie.filter <- movie.data %>%
      filter(budget > input$budget[1] & budget < input$budget[2]) %>%
      filter(revenue > input$revenue[1] & revenue < input$revenue[2])
    summary <- summarize(budget.movie.filter,
                         cor = round(cor(budget, revenue), 3),
                         mean.budget = round(mean(budget), 2),
                         mean.revenue = round(mean(revenue), 2),
                         median.budget = round(median(budget), 2),
                         median.revenue = round(median(revenue), 2)
    )
    
    range.budget <- range(budget.movie.filter$budget)
    range.revenue <- range(budget.movie.filter$revenue)
    
    return(paste0("Movie budgets range from $", range.budget[1], " to $", range.budget[2], sep = "\n",
                  "Movie revenue range from $", range.revenue[1], " to $", range.revenue[2], sep = "\n",
                  "The average budget is $", summary$mean.budget, sep = "\n",
                  "The average revenue is $", summary$mean.revenue, sep = "\n",
                  "The median budget is $", summary$median.budget, sep = "\n",
                  "The median revenue is $", summary$median.revenue, sep = "\n",
                  "The linear correlation between movie budget and revenue is ", summary$cor))
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
  
  # filters the overall data by original language to find the mean and median revenue of each language
  # as well as number of movies filmed in that language
  language.stats <- reactive ({
    language.summary <- by.language() %>%
      filter(original_language == input$language.choice)
    return(paste0("There was ", language.summary[1,4], " movie(s) filmed in this language", sep = "\n",
                  "The mean revenue for the selected language was $", language.summary[1,2], sep = "\n",
                  "The median revenue for the selected language was $", language.summary[1,3], sep ="\n",
                  "The mean and median are measured in U.S. dollar."))
  })
  
  # renders the revenue summary of the selected language
  output$language.summary <- renderText({
    return(language.stats())
  })
  
  # filters the data by the input original language for its mean and median of revenue as well as count 
  # and orders it by number of movies created in that language
  by.language <- reactive ({
    # import language iso codes
    lang.iso <- ISOcodes::ISO_639_2 %>% 
      select(Alpha_2, Name)
    filter.data <- group_by(movie.data, original_language) %>%
      summarize(
        mean = mean(revenue),
        median = median(revenue),
        n = n()
      ) %>%
      arrange(-n) %>%
      left_join(lang.iso, by = c("original_language" = "Alpha_2"))
    # change cn to reflect cantonese and xx to reflect artistic language
    filter.data[9,5] <- "Cantonese"
    filter.data[37, 5] <- "Artistic Language"
    return(filter.data)
  })
  
  # renders a plot to show revenue by original language
  output$language.plot <- renderPlot({
    plot.lang <- by.language() %>%
      head(input$language.num) %>%
      select(Name, input$language.type)
    names(plot.lang)[2] <- "lang.type"
    plot.bar <- ggplot(data = plot.lang, aes(x=Name, y=lang.type)) +
      labs(title = "Movie Language by Revenue",
           x = "original language",
           y = input$language.type) +
      geom_bar(stat="identity", fill = "#21D17A") + 
      geom_text(aes(label=lang.type), vjust=2, color="black", size=3, hjust=0.5) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_line(color=NA),
            axis.line.x = element_line(color="grey80"))
    return(plot.bar)
  })
  
  # find the p value for original language
  p.lang <- reactive ({
    lang.data <- movie.data %>%
      select(original_language, revenue)
    names(lang.data)[1] <- "Language"
    names(lang.data)[2] <- "type" 
    p.value <- aov(type ~ Language, data = lang.data)
    return(summary(p.value))
  })
  
  # render the text for p value
  output$p.lang <- renderPrint({
    return(p.lang()) 
  })
  
  # render what the revenue at the point is
  output$language.info <- renderPrint({
    hover.y <- input$language.hover$y
    return(cat("The revenue at the point you are hovering at is $", hover.y, sep = ""))
  })
  
  # Renders scatter plot to show revenue by movie popularity
  output$pop.plot <- renderPlot({
    pop.mov.filter <- movie.data %>%
      filter(popularity > input$pop[1] & popularity < input$pop[2]) %>%
      filter(revenue > input$rev[1] & revenue < input$rev[2])
    ggplot(pop.mov.filter, mapping = aes(x = popularity, y = revenue)) + 
      geom_jitter() +
      geom_smooth(mapping = aes(x = popularity, y = revenue), color = "#21D17A") +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      labs(title = "Movie Popularity by Revenue", 
           x = "Popularity",  
           y = "Revenue (in US Dollars)") + 
      theme(title = element_text(size=16))
    
  })
  
  # Calcuates mean, median, range, and linear correlation of revenue and popularity data.
  # Filters the data based on widgets. Returns text of summarized statistics.
  output$pop.summary <- renderText({
    pop.mov.filter <- movie.data %>%
      filter(popularity > input$pop[1] & popularity < input$pop[2]) %>%
      filter(revenue > input$rev[1] & revenue < input$rev[2])
    
    #Calcualtes popularity and revenue statistics
    mean.pop <- mean(pop.mov.filter$popularity)
    mean.budget <- mean(pop.mov.filter$budget)
    median.pop <- median(pop.mov.filter$popularity)
    median.budget <- median(pop.mov.filter$budget)
    range.pop <- range(pop.mov.filter$popularity)
    range.budget <- range(pop.mov.filter$budget)
    pop.budget.cor <- cor(pop.mov.filter$popularity, pop.mov.filter$revenue)
    
    return(paste0("Movie budgets range from $", range.budget[1], " to $", range.budget[2], sep = "\n",
                  "Movie popularity range from $", range.pop[1], " to $", round(range.pop[2], 0), sep = "\n",
                  "The average budget amongst all movies is $", round(mean.budget, 0), sep = "\n",
                  "The average popularity amongst all movies is $", round(mean.pop, 0), sep = "\n",
                  "The median budget amongst all movies is $", round(median.budget, 0), sep = "\n",
                  "The median popularity amongst all movies is $", round(median.pop, 0), sep = "\n",
                  "The linear correlation between movie popularity and budget is ", round(pop.budget.cor, 3)
    )
    )
  })
  
  
  # Creates brush and double-click interactive feature of map, zooms in on brushed area
  # of visualization
  observeEvent(input$pop_dblclick, {
    brush <- input$pop_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Creates interactive hover feature of map, displays movie title, popularity, and revenue values
  output$pop.info <- renderPrint({
    filtered.pop <- select(movie.data, title, popularity, revenue)
    pop.filtered.data <- nearPoints(filtered.pop, input$pop_hover, 
                                    maxpoints = 1,
                                    xvar = "popularity", yvar = "revenue")
    
    if(dim(pop.filtered.data[0]) != 0) {
      print(pop.filtered.data, row.names = FALSE)
    }
  })
  
  
  # renders a map to show the revenue of production countries 
  output$prod.map <- renderPlot({
    ggplot(data = map.world) + 
      geom_polygon(aes(x = long, y = lat, group = group), fill = "#21d17a", color = "Green") +
      coord_fixed(1.3) +
      labs(title = "Map of Production Countries", 
           x = "Longtitude",  
           y = "Latitude")
  })
  
  # gets the information about the chosen point on the map
  output$map.info <- renderPrint({
    filter.country.data <- select(map.world, region, long, lat)
    chosen.country.info <- nearPoints(filter.country.data, input$map_hover,
                                      maxpoints = 1, 
                                      xvar = "long", yvar = "lat")
    if(dim(chosen.country.info[0]) != 0) {
      print(chosen.country.info, row.names = FALSE)
    }
  })
  
  # gets the detailed information about the chosen point on the map
  output$country.info <- renderText({
    filter.country.data <- select(map.world, region, long, lat)
    chosen.country.info <- nearPoints(filter.country.data, input$map_hover,
                                      maxpoints = 1, 
                                      xvar = "long", yvar = "lat")
    country.name <- chosen.country.info$region
    final.info <- filter(movie.countries, name == country.name)
    if(length(final.info) == 0){
      num.movie = 0  
      avg.reve = "NA"
    } else {
      num.movie <- nrow(final.info)
      avg.rev <- summarise(final.info, avg = mean(final.info$revenue))  
    }
    return(paste0(country.name, " produced total ", num.movie," movies.", sep = "\n",
                  "The average revenue ", country.name, " made is $", avg.rev, sep = "\n"))
  })
  
  
  # Calculates mean and median revenue of the total production countries
  output$totalprod.summary <- renderText({
    total.mean <- summarise(movie.countries, avg = mean(movie.countries$revenue))
    total.median <- summarise(movie.countries, median = median(movie.countries$revenue))
    highest <- summarise(movie.countries, max = max(movie.countries$revenue))
    lowest <- summarise(movie.countries, min = min(movie.countries$revenue))
    
    return(paste0("There are total 88 countries that have produced movies.", sep = "\n",
                  "The mean revenue of the movies is $", total.mean, sep = "\n",
                  "The median revenue of the movies is $", total.median, sep = "\n",
                  "Highest revenue among the movies is $", highest, sep = "\n",
                  "Lowest revenue among the movies is $", lowest)
    )
  })
  
  # Filters statistical information about selected country
  output$country.summary <- renderText({
    country.chosen <- filter(movie.countries, name == input$choice.country)
    num.movie <- nrow(country.chosen)
    mean.country <- summarise(country.chosen, avg = mean(country.chosen$revenue)) 
    median.country <- summarise(country.chosen, avg = median(country.chosen$revenue))
    paste0(input$choice.country, " produced total ", num.movie, " movies", sep = "\n",
           "The country's mean revenue is $", mean.country, sep = "\n",
           "The country's median revenue is $", median.country)
    
  })
  
  # Calculates p value of production country and revenue
  p.produce <- reactive ({
    produce.data <- movie.countries %>%
      select(name, revenue)
    names(produce.data)[1] <- "Name"
    names(produce.data)[2] <- "type" 
    p.value <- aov(type ~ Name, data = produce.data)
    return(summary(p.value))
  })
  
  output$p.produce <- renderPrint({
    return(p.produce()) 
  })
  
}
