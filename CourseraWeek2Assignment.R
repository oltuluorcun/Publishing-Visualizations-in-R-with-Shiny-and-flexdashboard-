library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)


data <- read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
data <- data %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
data <- drop_na(data)

ui <- navbarPage(
  title = "Assignment - Week2",
  tabPanel("Page 1",
           column(4,
                  inputPanel(
                    sliderInput(inputId = "ideology", 
                                label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                                min = 1,
                                max = 5,
                                value = 3,
                                )
                  )),
           column(8,
                  tabsetPanel(
                    tabPanel("Tab1", plotOutput(outputId = "barplot1")),
                    tabPanel("Tab2", plotOutput(outputId = "barplot2"))
                  )
                 )
           ),
  tabPanel("Page 2",
           column(4,
                  inputPanel(
                    checkboxGroupInput(inputId = "Gender", label =  "Select Gender", 
                                  choices = c("1","2"))
                  )
           ),
           column(8,
                  plotlyOutput(outputId = "Myplot")
                  )
           ),
  tabPanel("Page 3",
           column(4,
                  inputPanel(
                    selectInput(inputId = "Region", label =  "Select Region",
                                   multiple = TRUE,
                                   choices=c("1","2","3","4"))
                  )
                  ),
           column(8,
                  dataTableOutput(outputId = "tab",
                                  height=500))
           )
)

server <- function(input, output) {
  
  #### Page 1 ####
  
  output$barplot1 <- renderPlot({
    ggplot(data = data %>% select(c("pid7","ideo5")) %>% filter(ideo5 == input$ideology),
           aes(x = pid7)) + 
      geom_bar() + 
      xlab("7 Point Party ID, 1=Very D, 7=Very R") + 
      ylab("Count") 
  })
  
  output$barplot2 <- renderPlot({
    ggplot(data = data %>% select(c("CC18_308a","ideo5")) %>% filter(ideo5 == input$ideology),
           aes(x = CC18_308a)) + 
      geom_bar() + 
      xlab("Trump Support") + 
      ylab("Count") 
  })
  
  #### Page 2 ####
  
  output$Myplot <- renderPlotly({
      ggplotly(
        ggplot(data = filter(data, gender %in% input$Gender) %>% select(educ,pid7),
           aes(x = educ, y = pid7)) +
      geom_point() + geom_jitter() + 
      geom_smooth(method = "lm")
      )
  })
  
  #### Page 3 ####
  
  output$tab <- renderDataTable(data %>% filter(region %in% input$Region))
  
}

shinyApp(ui = ui, server = server)


