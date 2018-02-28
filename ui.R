# Setup
library(shiny)

#
ui <- fluidPage(
  
  titlePanel("Movie"),
  
  sidebarLayout(
    
    sidebarPanel(
      
    ),
    
    mainPanel(
      
      # Tabset of interactive scatterplots
      tabsetPanel(type = "tabs",
                  # Scatterplot of movie budget vs. revenue
                  tabPanel("Budget", 
                           br(),
                           h4("This graph plots movie budgets by its revenue."),
                           br(),
                           
                           verbatimTextOutput("budget_summary"),
                           p("With the given linear correlation coefficient, there is a strong positive correlation 
                              between the movie budget and revenue. Hence, we can assume that the movie revenue is 
                              most likely to increase as the budget increases."),
                           br(),
                           
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
                           verbatimTextOutput("budget_info")
                          )
                           
                    )
    )
  )
)
