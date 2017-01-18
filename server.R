
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyServer(function(input, output){
  test <- reactive({
    validate(
      need(input$file1 != "", "Будь ласка, оберіть фото для аналізу")
    )
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      body <- lapply(inFile$datapath, upload_file)
    }
    
    face_api_url = "https://api.projectoxford.ai/emotion/v1.0/recognize"
    
    test <- lapply(body,function (x) {
      a <- as.data.frame(
        content(
          POST(
            face_api_url, body = x,
            add_headers(.headers = c("Content-Type"="application/octet-stream",
                                     "Ocp-Apim-Subscription-Key"="4423ea1091254e7497d0e35bcc51c462")))))
      names(a)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
      a
    })
  }
  )
  
  df <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    a <- test[,c(input$emo,"time")]
    b <- reshape2::melt(a, id.vars=c("time"))
    
    ggplot(b,aes(time)) + 
      geom_point(aes(y=value,fill=variable,color=variable), alpha=0.4) +
      scale_y_continuous(labels = percent) +
      theme(axis.line = element_line(size=1, colour = "black"), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.border = element_blank(), panel.background = element_blank()) + 
      theme( 
        axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")
  })
  
  output$plot <- renderPlot({
    tryCatch(df())
  })
  
output$contents <- renderTable({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    tryCatch(test[5:12])
  }
)
 
 output$downloadPlot <-  downloadHandler(
   filename = 'test.pdf',
   content = function(file) {
     pdf(file = file, width=12, height=4)
     print(df())
     dev.off()
   }
  )
  
  output$download<-  downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = df(), device = "png",width = 1100, height = 750)
    })
})