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
                           h4(em("Bar Chart Options")),
                           # select whether you want to view the mean or median by language
                           radioButtons('language.type', 'Choose a Type of Revenue to View:',
                                        c("Mean" = "mean",
                                          "Median" = "median")
                                        ),
                           # select how many languages you want to view
                           numericInput('language.num', 'Select a Number of Languages to View:',
                                        5, min = 1, max = 37),
                           h4(em("Individual Summary Options")),
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
                                         "Traditional Chinese" = "zh",
                                         "Simplified Chinese" = "cn"
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
                           h4("This graph plots the mean revenue of movies by original language."),
                           p("The number of languages shown can be changed by increasing the input number on the left.
                             The bar plot outputs languages in alphabetical order. If the mean/median shows 0, it is not an error.
                             There may be several reasons why the revenue was 0. For example, the movies filmed in the language
                             could have been independent films who did not release in the box office and were uploaded online for free
                             thus resulting in $0 for revenue."),
                           plotOutput("language.plot",
                                      hover = "language_hover"),
                           br(),
                           h4("This text shows the individual statistics of the selected language."),
                           verbatimTextOutput("language.summary")
                           )
                           
                    )
    )
  )
)
