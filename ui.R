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
                           )
                           
                    )
    )
  )
)
