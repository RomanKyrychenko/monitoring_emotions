
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
                                                                                          "Страх"="Страх"),selected	= c("Радість"="Радість",
                                                                                                                        "Злість"="Злість",
                                                                                                                        "Здивування"="Здивування",
                                                                                                                        "Сум"="Сум",
                                                                                                                        "Відраза"="Відраза",
                                                                                                                        "Зневага"="Зневага",
                                                                                                                        "Страх"="Страх"))
     
    ),
    mainPanel(tabsetPanel(
      tabPanel("Візуалізація емоцій",plotOutput('plot'), downloadButton('downloadPlot',"Завантажити візуалізацію в pdf!"),
               downloadButton('download',"Завантажити візуалізацію в png!")),
      tabPanel("Сумарно по емоціям",plotOutput('sumEmo'), downloadButton('downloadPlot2',"Завантажити візуалізацію в pdf!"),
               downloadButton('download2',"Завантажити візуалізацію в png!")),
      tabPanel("Таблиця даних",tableOutput('contents'), downloadButton('downloadPlot3',"Завантажити таблицю в csv!"),
               downloadButton('download3',"Завантажити таблицю в xlcx!")),
      tabPanel("Таблиця даних (сума)",tableOutput('contents2'), downloadButton('downloadPlot4',"Завантажити таблицю в csv!"),
               downloadButton('download4',"Завантажити таблицю в xlcx!"))
    )
  )
  )
))
