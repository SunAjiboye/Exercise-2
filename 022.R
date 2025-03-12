install.packages("shiny")
library(shiny)



library(shiny)
library(ggplot2)
library(shinydashboard)

install.packages(c("shiny", "ggplot2", "shinydashboard"))
library(shinydashboard)



# Define UI ---------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Study on Volcanic Eruption"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Mean Waiting Time", tabName = "mean_waiting_time", icon = icon("clock")),
      menuItem("Median Waiting Time", tabName = "median_waiting_time", icon = icon("bar-chart")),
      menuItem("Eruption Count", tabName = "eruption_count", icon = icon("database"))
    )
  ),
  dashboardBody(
    h1("Volcanic Eruption Dashboard!"),
    fluidRow(
      box(
        title = "Settings",
        width = 4,
        sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
        selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
        selectInput("theme", "Choose a theme:", choices = c("Classic", "Minimal", "Dark"))
      ),
      valueBoxOutput("mean_waiting", width = 4),
      valueBoxOutput("median_waiting", width = 4),
      valueBoxOutput("eruption_count", width = 4)
    ),
    fluidRow(
      box(title = "Histogram", collapsible = TRUE, width = 6, plotOutput("distPlot")),
      box(title = "Density Plot", collapsible = TRUE, width = 6, plotOutput("densityPlot"))
    ),
    fluidRow(
      box(title = "Box Plot", collapsible = TRUE, width = 6, plotOutput("boxPlot")),
      box(title = "Scatter Plot", collapsible = TRUE, width = 6, plotOutput("scatterPlot"))
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  theme_choice <- reactive({
    switch(input$theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark(),
           theme_classic()
    )
  })
  
  output$mean_waiting <- renderValueBox({
    valueBox(
      value = paste(round(mean(faithful$waiting), 2), "mins"),
      subtitle = "Mean Waiting Time",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$median_waiting <- renderValueBox({
    valueBox(
      value = paste(round(median(faithful$waiting), 2), "mins"),
      subtitle = "Median Waiting Time",
      icon = icon("bar-chart"),
      color = "green"
    )
  })
  
  output$eruption_count <- renderValueBox({
    valueBox(
      value = nrow(faithful),
      subtitle = "Total Eruptions",
      icon = icon("database"),
      color = "red"
    )
  })
  
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(breaks = bins, fill = input$color, color = "white") +
      labs(x = "Waiting time to next eruption (in mins)", y = "Frequency") +
      theme_choice()
  })
  
  output$densityPlot <- renderPlot({
    x <- faithful$waiting
    ggplot(data.frame(x), aes(x)) +
      geom_density(fill = input$color, alpha = 0.5) +
      labs(title = "Density Plot of Waiting Times", x = "Waiting Time (mins)", y = "Density") +
      theme_choice()
  })
  
  output$boxPlot <- renderPlot({
    x <- faithful$waiting
    ggplot(data.frame(x), aes(y = x)) +
      geom_boxplot(fill = input$color, color = "black") +
      labs(title = "Boxplot of Waiting Times", y = "Waiting Time (mins)") +
      theme_choice()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
      geom_point(color = input$color) +
      labs(title = "Scatter Plot of Eruptions vs Waiting Time", x = "Eruption Duration (mins)", y = "Waiting Time (mins)") +
      theme_choice()
  })
}

# Run the App in Showcase Mode
shinyApp(ui, server, options = list(display.mode = "showcase"))

