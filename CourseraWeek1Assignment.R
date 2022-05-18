library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
data <- data %>% select(c("pid7","ideo5"))
data <- drop_na(data)

ui <- fluidPage(
  titlePanel("Publishing Visualizations in R with Shiny and flexdashboard - Week1 - Assignment"),
  sliderInput(inputId = "ideology", 
              label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
              min = 1,
              max = 5,
              value = 3),
  plotOutput(outputId = "barp")
)

server <- function(input, output) {
  
  output$barp <- renderPlot({
    ggplot(data = filter(data, ideo5 == input$ideology),
           aes(x = pid7)) + 
      geom_bar(fill = input$ideology) + 
      xlab("7 Point Party ID, 1=Very D, 7=Very R") + 
      ylab("Count") +
      ylim(0,120)
  })
  
}

shinyApp(ui = ui, server = server)


