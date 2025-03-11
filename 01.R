install.packages("bs4Dash")
install.packages("ggplot2")
library(bs4Dash)
library(ggplot2)

# Define UI ---------------------------------------------------------------
ui <- bs4DashPage(
  dark = FALSE,
  header = bs4DashNavbar(title = "Hello Shiny 3!"),
  sidebar = bs4DashSidebar(
    title = "Controls!",
    shiny::sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
    shiny::selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
    shiny::selectInput("theme", "Choose a theme:", choices = c("Classic", "Minimal", "Dark"))
  ),
  body = bs4DashBody(
    fluidRow(
      bs4ValueBoxOutput("mean_waiting", width = 4),
      bs4ValueBoxOutput("median_waiting", width = 4),
      bs4ValueBoxOutput("eruption_count", width = 4)
    ),
    fluidRow(
      bs4Card(
        title = "Histogram",
        collapsible = TRUE, maximizable = TRUE,
        width = 6,
        plotOutput("distPlot")
      ),
      bs4Card(
        title = "Density Plot",
        collapsible = TRUE, maximizable = TRUE,
        width = 6,
        plotOutput("densityPlot")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Box Plot",
        collapsible = TRUE, maximizable = TRUE,
        width = 6,
        plotOutput("boxPlot")
      ),
      bs4Card(
        title = "Scatter Plot",
        collapsible = TRUE, maximizable = TRUE,
        width = 6,
        plotOutput("scatterPlot")
      )
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  theme_choice <- reactive({
    switch(input$theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  output$mean_waiting <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste(round(mean(faithful$waiting), 2), "mins"),
      subtitle = "Mean Waiting Time",
      icon = icon("clock")  
    )
  })
  
  output$median_waiting <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste(round(median(faithful$waiting), 2), "mins"),
      subtitle = "Median Waiting Time",
      icon = icon("chart-bar")
    )
  })
  
  output$eruption_count <- renderbs4ValueBox({
    bs4ValueBox(
      value = nrow(faithful),
      subtitle = "Total Eruptions",
      icon = icon("database") 
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

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
