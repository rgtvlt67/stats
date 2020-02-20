library(shinydashboard)

ui <- dashboardPage(

 dashboardHeader(title = "Hello",titleWidth = 350),
  dashboardHeader(title = "Aids Progression"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Select Person",
       sliderInput("slider", "Person ID:", 1, 10, 1)
      )
    )
  )
)

server <- function(input, output) {
data_df <- read.csv("allvar.csv",stringsAsFactors=FALSE)
data_df <- na.omit(data_df)
data_df$Time <- data_df$visage - data_df$baseage
data_df$CD4PCT_SQRT <- data_df$CD4PCT ^0.5
child <- lmer(CD4PCT_SQRT ~ Time + ( 0 + Time | newpid) , data = data_df)


model_coefs <- coef(child)$newpid %>% 
  rename(Intercept = `(Intercept)`, Slope = Time) %>% 
  rownames_to_column("newpid")
model_coefs$newpid <- as.numeric(model_coefs$newpid)
data_df$newpid <- as.numeric(data_df$newpid)
data<- left_join(data_df, model_coefs, by = "newpid")

 output$plot1 <- renderPlot({
 p <-  ggplot(data = subset(data,newpid == input$slider), 
       mapping = aes(x = Time, 
                     y = CD4PCT_SQRT, 
                     colour = newpid)
       ) +
  geom_point(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = newpid
                  ),
              size = 1.5
              ) 
  print(p)
  })
}

shinyApp(ui, server)