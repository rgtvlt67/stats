library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Aids Progression Dash Board"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
       sliderInput("slider", "Time Period:", 1, 10, 1)
      )
    )
  )
)

server <- function(input, output) {
data_df <- read.csv("allvar.csv",stringsAsFactors=FALSE)
data_df <- na.omit(data_df)
data_df$Time <- data_df$visage - data_df$baseage
data_df$CD4PCT_SQRT <- data_df$CD4PCT ^0.5

 output$plot1 <- renderPlot({
  p <-ggplot(data_df, aes(x =input$slider, y = CD4PCT_SQRT)) + geom_point()
  print(p)
  })
}

shinyApp(ui, server)