# Setup
library(shiny)

#
ui <- fluidPage(
  
  titlePanel("Movie"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  # Widgets to manipulate original language
                  tabPanel("Original Language",
                           h4(strong("Bar Chart Options")),
                           # select whether you want to view the mean or median by language
                           radioButtons('language.type', 'Choose a Type of Revenue to View:',
                                        c("Mean" = "mean",
                                          "Median" = "median")
                                        ),
                           # select how many languages you want to view
                           numericInput('language.num', 'Select a Number of Languages to View:',
                                        5, min = 1, max = 37),
                           h4(strong("Individual Summary Options")),
                           # select the language you want to view
                           selectInput('language.choice', "Choose a Language:",
                                       c("Afrikans" = "af",
                                         "Arabic" = "ar",
                                         "Czech" = "cs",
                                         "Danish" = "da",
                                         "German" = "de",
                                         "Greek" = "el",
                                         "English" = "en",
                                         "Spanish" = "es",
                                         "Persian (Farsi)" = "fa",
                                         "French" = "fr",
                                         "Hebrew" = "he",
                                         "Hindi" = "hi",
                                         "Hungarian" = "hu",
                                         "Indonesian" = "id",
                                         "Icelandic" = "is",
                                         "Italian" = "it",
                                         "Japanese" = "jp",
                                         "Korean" = "ko",
                                         "Kyrgyz" = "ky",
                                         "Dutch" = "nl",
                                         "Norwegian" = "no",
                                         "Polish" = "pl",
                                         "Pashto, Pushto" = "ps",
                                         "Portuguese" = "pt",
                                         "Romanian" = "ro",
                                         "Russian" = "ru",
                                         "Slovenian" = "sl",
                                         "Swedish" = "sv",
                                         "Tamil" = "ta",
                                         "Telugu" = "te",
                                         "Thai" = "th",
                                         "Turkish" = "tr",
                                         "Vietnamese" = "vi",
                                         "Mandarin" = "zh",
                                         "Cantonese" = "cn",
                                         "Artistic Language" = "xx"
                                       )
                            )
                          ),
                  # Creates widgets to manipulate popularity vs revenue visualization
                  conditionalPanel(condition = "input.conditionedPanels==1",
                           "Popularity",
                           h4("Filter by:"),
                           # Filter by popularity value
                           sliderInput("pop", "Popularity", min = 0, max = 1000, value = c(min, max)),
                           
                           # Filter by revenue value
                           sliderInput("rev", "Revenue", min = 0, max = 4000000000, value = c(min, max))
                  ),
                  
                  conditionalPanel(condition = "input.conditionedPanels==3",
                                   selectInput("choice.country", "Choose Country:", 
                                               movie.countries$name)
                  )
      )
      
    ),
    
    mainPanel(
      
      # Tabset of interactive scatterplots
      tabsetPanel(type = "tabs",
                  # Scatterplot of movie budget vs. revenue
                  tabPanel("Budget", 
                           br(),
                           h4("This graph plots movie budgets by its revenue."),
                           
                           strong("To navigate through the scatterplot:"),
                           br(),
                           em("Hover over the points in the plot to see individual descriptions."),
                           br(),
                           em("Zoom in by highlighting and double-clicking into the plot."),
                           br(),
                           br(),
                           plotOutput("budget.plot",
                                      dblclick = "budget_dblclick",
                                      hover = "budget_hover",
                                      brush = brushOpts(id = "budget_brush",
                                                        resetOnNew = TRUE)
                                      ),
                           br(),
                           verbatimTextOutput("budget_info"),
                           
                           h4("Summary"),
                           verbatimTextOutput("budget_summary"),
                           p("With the given linear correlation coefficient, there is a strong positive correlation 
                             between the movie budget and revenue. Hence, we can assume that the movie revenue is 
                             most likely to increase as the budget increases.")
                          ),
                  
                  # Plot and info for analyzing original language and revenue
                  tabPanel("Original Language",
                           br(),
                           h4("This graph plots the revenue of movies by original language."),
                           plotOutput("language.plot", 
                                      hover = "language.hover"),
                           h4(em("Notes about the bar chart")),
                           em(p("You can view the revenue of movies by original language charted by either the mean or the median."),
                           p("The number of languages shown can be changed by increasing the input number on the left."),
                           p("The bar plot outputs languages in alphabetical order. So, when you increase the number of languages shown, the added language
                             will appear in alphabetical order in relation to the other bars."),
                           p("If the mean/median shows 0, it is not an error.
                             There may be several reasons why the revenue was 0. For example, the movies filmed in the language
                             could have been independent films who did not release in the box office and were uploaded online for free
                             thus resulting in $0 for revenue.")),
                           h4("Hover over the bar graph with your mouse to view the revenue at that point."),
                           em("Values are listed in US dollar"),
                           verbatimTextOutput("language.info"),
                           br(),
                           h4("Analyzing the bar graph"),
                           p("As the bar chart displays, movies filmed in English generate more revenue than movies filmed in any other language. 
                             This trend is present when comparing both the mean and median revenues of movies.
                             This suggests that, in general, movies filmed in English will generate the most revenue when compared to other languages."),
                           br(),
                           h4("Viewing statistics for a specific language."),
                           p("To view individual statistics for a specific language, select the language you want to view from the dropdown box on the left."),
                           verbatimTextOutput("language.summary")
                           ),
                
                  # Interactive scatterplot and summary information of movie popularity and revenue 
                  tabPanel("Popularity", 
                           value = 1, 
                           br(),
                           h4("This graph plots the revenue of movies by overall popularity."),
                           br(),
                           
                           # Describes interactive map features
                           h6("Map Features:"),
                           p(" - The left panel has two slider widgets that allow you to manipulate the range of both the
                              movie revenue and popularity. When you change the range, the summary data (mean, median, range, etc.)
                              will also change, reflecting the statistics of the data in the specified range."),
                           em(" - Hover over individual points on the plot to see the specific revenue and popularity information. 
                               If you wish to take a closer look, brush and double-click over the area you want to zoom-in on."),
                            
                           # Creates scatterplot with interactive features 
                           plotOutput("pop.plot",                                       
                                      dblclick = "pop_dblclick",
                                      hover = "pop_hover",
                                      brush = brushOpts(id = "pop_brush",
                                                        resetOnNew = TRUE)),
                           br(),
                           # Prints summary statistics and over analysis of data and visualization
                            verbatimTextOutput("pop.info"),
                            em("Overall popularity is a rating assigned to each movie based on other statistical factors of the film. When calculating 
                               movie popularity, factors such as average rating for the movie, number of votes for the movie, and the average overall 
                               vote for the movie are taken into account."),
                            
                          h4("Summary"),
                          verbatimTextOutput("pop.summary"),
                          
                          p("Looking at the graph, we see the line of best fit has a slight parabola shape. This indicates that the relationship
                            between movie popularity and revenue peaks at a certain point, and then becomes irrelevant. The linear correlation between 
                            revenue and popularity is around 0.6, meaning that the relationship between the two variables is moderately positive. By looking at the graph, 
                            the outliers indicate that movies with high revenues can be less popular and the same in 
                            reverse, movies with low revenues can have high popularity. However, the overall trend of the graph indicates that revenue and 
                            popularity are positively correlated, so as popularity increases, so should revenue.")
                           
                    ), id = "conditionedPanels",
                  
                  tabPanel("Production Country", 
                           value = 3,
                           br(),
                           h4("This map plots the revenue of movies by production countries."),
                           br(),
                           strong("Features"),
                           br(),
                           em("- By hovering on the map, you can select a specific point (country). On the bottom of the map, you will be 
                              able to see detailed information about the selected point."),
                           p("- Using the panel on the left, you can choose a country for more information. 'Selected Country Information' 
                             part will give additional information about the selected production country."),
                           plotOutput("prod.map", 
                                      hover = "map_hover"),
                           br(),
                           h4("Selected Country Information"),
                           verbatimTextOutput("map.info"),
                           verbatimTextOutput("country.info"),
                           br(),
                           h4("Summary"),
                           verbatimTextOutput("totalprod.summary"),
                           h4("Selected Country Information"),
                           verbatimTextOutput("country.summary"),
                           h4("Correlation"),
                           verbatimTextOutput("p.produce"),
                           "From the original dataset, a One Way Analysis of Variance (ANOVA) test was run between
                           production country and revenue revealing a p-value of 2e-16, which is 0.00000000000000022.
                           Because the p-value is < 0.05 by large difference, 
                           production country and revenue can therefore be considered a statistically highly significant factor."
                           
                  )
                  
    )
  )
)
)
