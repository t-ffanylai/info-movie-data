# Setup
library(shiny)
library(shinydashboard)

mycss <- ".irs-bar,
          .irs-bar-edge,
          .irs-single,
          .irs-grid-pol {
            background: #21D17A;
            border-color: #21D17A;
          }
"

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Movies"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      
      ##################
      ####  Graphs  ####
      ##################
      
      # Creates a box with tabs for the interactive graphs
      tabBox(
        id = "graphName", height = "500px", 

        # Scatterplot of movie budget vs. revenue
        tabPanel("Budget", 
                 "This graph plots movie budgets by its revenue.", 
                 br(),
                 br(),
                 
                 # Creates scatterplot with interactive features
                 plotOutput("budget.plot",
                            dblclick = "budget_dblclick",
                            hover = "budget_hover",
                            brush = brushOpts(id = "budget_brush",
                                              resetOnNew = TRUE)
                           )
                ),
        
        # Scatterplot of popularity vs. revenue
        tabPanel("Popularity", 
                 "This graph plots the revenue of movies by overall popularity.",
                 br(),
                 br(),
                 
                 # Creates scatterplot with interactive features 
                 plotOutput("pop.plot",                                       
                            dblclick = "pop_dblclick",
                            hover = "pop_hover",
                            brush = brushOpts(id = "pop_brush",
                                              resetOnNew = TRUE)
                           )
                ),
        
        # Bar graph of original language vs. revenue
        tabPanel("Original Language", 
                 "This graph plots the revenue of movies by original language.",
                 br(),
                 br(),
                 
                 # Creates bar graph with interactive features 
                 plotOutput("language.plot", 
                            hover = "language.hover")
                )
      ),
      
      ##################
      ####  Guides  ####
      ##################
      
      # Creates a box of instructions to interact with the budget vs. revenue visualization 
      conditionalPanel(
        condition = "input.graphName == 'Budget'",
        box(status = "success",
            strong("Instructions:"),
            br(),
            em("The box below has two slider widgets that allow you to manipulate the range of both the
                movie revenue and budget. When you change the range, the summary data (mean, median, range, etc.)
                will also change, reflecting the statistics of the data in the specified range.")
        )
      ),
      
      # Creates a box of instructions to interact with the popularity vs. revenue visualization 
      conditionalPanel(
        condition = "input.graphName == 'Popularity'",
        box(status = "success",
            strong("Instructions:"),
            br(),
            em("The box below has two slider widgets that allow you to manipulate the range of both the
               movie revenue and popularity. When you change the range, the summary data (mean, median, range, etc.)
               will also change, reflecting the statistics of the data in the specified range.")
        )
      ),
      
      # Creates a box of instructions to interact with the popularity vs. revenue visualization 
      conditionalPanel(
        condition = "input.graphName == 'Original Language'",
        box(status = "success",
            strong("Instructions:"),
            br(),
            em("By default, the first 5 languages who have the highest mean revenue are plotted.",
               br(),
               "You can view the revenue of movies by original language charted by either the mean or the median.",
               br(),
               "The number of languages shown can be changed by increasing the input number on the left.",
               br(),
               "The bar plot outputs languages in alphabetical order. So, when you increase the number of languages shown, 
               the added language will appear in alphabetical order in relation to the other bars.",
               br(),
               br(),
               "If the mean/median shows 0, it is not an error. There may be several reasons why the revenue was 0.",
               br(),
               "For example, the movies filmed in the language could have been independent films that did not release 
                in the box office and were uploaded online for free, thus resulting in $0 for revenue."
            )
        )
      ),
      
      #####################
      ####  More Info  ####
      #####################
      
      # Creates a box of specific revenue and budget information for a point 
      conditionalPanel(
        condition = "input.graphName == 'Budget'",
        box(status = "success",
            strong("More Information: "),
            br(),
            em("Hover over individual points on the plot to see the specific revenue and budget information.",
               br(),
               "If you wish to take a closer look, brush and double-click over the area you want to zoom-in on."),
            br(),
            br(),
            verbatimTextOutput("budget_info"))
      ),
      
      # Creates a box of specific revenue and popularity information for a point 
      conditionalPanel(
        condition = "input.graphName == 'Popularity'",
        box(status = "success",
            strong("More Information: "),
            br(),
            em("Hover over individual points on the plot to see the specific revenue and popularity information.",
               br(),
               "If you wish to take a closer look, brush and double-click over the area you want to zoom-in on."),
            br(),
            br(),
            verbatimTextOutput("pop.info"))
      ),
      
      # Creates a box of specific revenue information
      # and individual statistic for a specific language for a point 
      conditionalPanel(
        condition = "input.graphName == 'Original Language'",
        box(status = "success",
            strong("More Information: "),
            br(),
            em("Hover over the bar graph with your mouse to view the revenue at that point.",
               br(),
               br(),
               verbatimTextOutput("language.info"),
               br(),
               "To view individual statistics for a specific language, 
               select the language you want to view from the dropdown box on the left.",
               br(),
               br(),
               verbatimTextOutput("language.summary")
            )
        )
      ),

      ###################
      ####  Widgets  ####
      ###################
      
      # Creates widgets to manipulate popularity vs. revenue visualization
      conditionalPanel(
        condition = "input.graphName == 'Budget'",
        box(status = "success",
            "Filter by:",
            tags$style(mycss),
            # Filter by budget value
            sliderInput("budget", "Budget", min = 0, max = 400000000, value = c(min, max)),
            
            # Filter by revenue value
            sliderInput("revenue", "Revenue", min = 0, max = 3000000000, value = c(min, max))
        )
      ),
      
      # Creates widgets to manipulate popularity vs. revenue visualization
      conditionalPanel(
        condition = "input.graphName == 'Popularity'",
        box(status = "success",
            "Filter by:",
            # Filter by popularity value
            sliderInput("pop", "Popularity", min = 0, max = 1000, value = c(min, max)),
            
            # Filter by revenue value
            sliderInput("rev", "Revenue", min = 0, max = 4000000000, value = c(min, max))
        )
      ),
      
      # Creates widgets to manipulate original language vs. revenue visualization
      conditionalPanel(
        condition = "input.graphName == 'Original Language'",  
        box(status = "success",
            "Bar Chart Options",
            # select whether you want to view the mean or median by language
            radioButtons('language.type', 'Choose a Type of Revenue to View:',
                         c("Mean" = "mean", "Median" = "median")
                        ),
                       
            # select how many languages you want to view
            numericInput('language.num', 'Select a Number of Languages to View:',
                         5, min = 1, max = 37),
                       
            "Individual Summary Options",
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
      ),
      
      ###################
      ####  Summary  ####
      ###################
      
      # Creates a box of statistical summary and analysis of budget vs. revenue plot
      conditionalPanel(
        condition = "input.graphName == 'Budget'",
        box(status = "success",
            strong("Summary & Analysis: "),
            verbatimTextOutput("budget_summary"),
            br(),
            "In an overview, with the linear correlation coefficient of 0.705, 
             there is a strong positive correlation between the movie budget and revenue. 
             Hence, we can assume that the movie revenue is most likely to increase as the budget increases."
        )
      ),
      
      # Creates a box of statistical summary and analysis of popularity vs. revenue plot
      conditionalPanel(
        condition = "input.graphName == 'Popularity'",
        box(status = "success",
            strong("Summary & Analysis: "),
            verbatimTextOutput("pop.summary"),
            br(),
            "Looking at the graph, we see the line of best fit has a slight parabola shape. 
             This indicates that the relationship between movie popularity and revenue peaks at a certain point, 
             and then becomes irrelevant. The linear correlation between revenue and popularity is around 0.6, 
             meaning that the relationship between the two variables is moderately positive. By looking at the graph, 
             the outliers indicate that movies with high revenues can be less popular and the same in reverse, 
             movies with low revenues can have high popularity. However, the overall trend of the graph indicates 
             that revenue and popularity are positively correlated, so as popularity increases, so should revenue."
        )
      ),
      
      # Creates a box of statistical summary and analysis of budget vs. revenue plot
      conditionalPanel(
        condition = "input.graphName == 'Original Language'",
        box(status = "success",
            strong("Summary & Analysis: "),
            br(),
            "As the bar chart displays, movies filmed in English generate more revenue than movies filmed 
             in any other language. This trend is present when comparing both the mean and median revenues of movies.
             This suggests that, in general, movies filmed in English will generate the most revenue 
             when compared to other languages.",
            br(),
            br(),
            verbatimTextOutput("p.lang"),
            br(),
            "From the original dataset, a One Way Analysis of Variance (ANOVA) test was run between
             original language and revenue revealing a p-value of 0.041. Because the p-value is < 0.05, 
             original language and revenue can therefore be considered a statistically significant factor."
        )
      )
    )
  )
)

