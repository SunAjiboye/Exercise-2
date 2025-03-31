library(shiny)
library(ggplot2)
library(bslib)

# Define UI ---------------------------------------------------------------
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "superhero"),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
      shiny::selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
      shiny::selectInput("theme", "Choose a theme:", choices = c("Classic", "Minimal", "Dark"))
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        "Mean Waiting Time", 
        shiny::textOutput("mean_waiting"), 
        showcase = shiny::icon("clock"),
        theme = "bg-yellow"  # Yellow background
      ),
      bslib::value_box(
        "Median Waiting Time", 
        shiny::textOutput("median_waiting"), 
        showcase = shiny::icon("chart-bar"),
        theme = "bg-purple"  # Purple background (replacing Rose)
      ),
      bslib::value_box(
        "Total Eruptions", 
        shiny::textOutput("eruption_count"), 
        showcase = shiny::icon("database"),
        theme = "bg-red"     # Red background
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_header("Histogram"),
        full_screen = TRUE,
        bslib::card_body(shiny::plotOutput("distPlot"))
      ),
      bslib::card(
        bslib::card_header("Density Plot"),
        full_screen = TRUE,
        bslib::card_body(shiny::plotOutput("densityPlot"))
      ),
      bslib::card(
        bslib::card_header("Box Plot"),
        full_screen = TRUE,
        bslib::card_body(shiny::plotOutput("boxPlot"))
      ),
      bslib::card(
        bslib::card_header("Scatter Plot"),
        full_screen = TRUE,
        bslib::card_body(shiny::plotOutput("scatterPlot"))
      )
    )
  )
)

# Define server logic -----------------------------------------------------
server <- function(input, output) {
  # Reactive theme choice
  theme_choice <- shiny::reactive({
    switch(input$theme,
           "Classic" = ggplot2::theme_classic(),
           "Minimal" = ggplot2::theme_minimal(),
           "Dark" = ggplot2::theme_dark())
  })
  
  # Render text outputs
  output$mean_waiting <- shiny::renderText({
    paste(round(mean(faithful$waiting), 2), "mins")
  })
  
  output$median_waiting <- shiny::renderText({
    paste(round(median(faithful$waiting), 2), "mins")
  })
  
  output$eruption_count <- shiny::renderText({
    paste(nrow(faithful))
  })
  
  # Render plots
  output$distPlot <- shiny::renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot2::ggplot(data.frame(x), ggplot2::aes(x)) +
      ggplot2::geom_histogram(breaks = bins, fill = input$color, color = "white") +
      ggplot2::labs(x = "Waiting time to next eruption (in mins)", y = "Frequency") +
      theme_choice()
  })
  
  output$densityPlot <- shiny::renderPlot({
    x <- faithful$waiting
    
    ggplot2::ggplot(data.frame(x), ggplot2::aes(x)) +
      ggplot2::geom_density(fill = input$color, alpha = 0.5) +
      ggplot2::labs(title = "Density Plot of Waiting Times", x = "Waiting Time (mins)", y = "Density") +
      theme_choice()
  })
  
  output$boxPlot <- shiny::renderPlot({
    x <- faithful$waiting
    
    ggplot2::ggplot(data.frame(x), ggplot2::aes(y = x)) +
      ggplot2::geom_boxplot(fill = input$color, color = "black") +
      ggplot2::labs(title = "Boxplot of Waiting Times", y = "Waiting Time (mins)") +
      theme_choice()
  })
  
  output$scatterPlot <- shiny::renderPlot({
    ggplot2::ggplot(faithful, ggplot2::aes(x = eruptions, y = waiting)) +
      ggplot2::geom_point(color = input$color) +
      ggplot2::labs(title = "Scatter Plot of Eruptions vs Waiting Time", 
                    x = "Eruption Duration (mins)", y = "Waiting Time (mins)") +
      theme_choice()
  })
}

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)