
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Аналіз емоцій"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1",
                "Оберіть фото для аналізу",
                multiple = TRUE,
                accept=c('.png','jpg')),
      tags$hr(),
      checkboxGroupInput("emo","Оберіть емоції, які бажаєте розмістити на візуалізації",c("Радість"="Радість",
                                                                                          "Злість"="Злість",
                                                                                          "Здивування"="Здивування",
                                                                                          "Сум"="Сум",
                                                                                          "Відраза"="Відраза",
                                                                                          "Нейтральність"="Нейтральність",
                                                                                          "Зневага"="Зневага",
                                                                                          "Страх"="Страх")),
      downloadButton('downloadPlot',"Завантажити візуалізацію в pdf!"),
      downloadButton('download',"Завантажити візуалізацію в png!")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Візуалізація емоцій",plotOutput('plot')),
      tabPanel("Таблиця даних",tableOutput('contents'))
    )
      )
  )
))
