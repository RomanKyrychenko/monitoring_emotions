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
      a
    })
  }
  )
  
  df <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    a <- test[,c(input$emo,"time")]
    b <- reshape2::melt(a, id.vars=c("time"))
    
    ggplot(b,aes(time)) + 
      geom_point(aes(y=value,fill=variable,color=variable), alpha=0.8) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
      scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
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
  
  ems <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    a <- test[,c(input$emo,"time")]
    b <- reshape2::melt(a, id.vars=c("time"))
    c <- b %>% group_by(variable) %>% summarise(value=mean(value))
    ggplot(c,aes(reorder(variable,-value),value,fill=factor(reorder(variable,-value)))) + geom_bar(stat = "identity") +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
      scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
      theme(axis.line = element_line(size=1, colour = "black"), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.border = element_blank(), panel.background = element_blank()) + 
      theme( 
        axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + coord_flip()
  })
  
  output$sumEmo <- renderPlot({
    ems()
  })
  
  tbl <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    tryCatch(test[5:12])
  })
  
output$contents <- renderTable({
    tbl()
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
    filename = 'test.png',
    content = function(file) {
      png(file = file, width=1200, height=400)
      print(df())
      dev.off()
    }
    )
  output$downloadPlot2 <-  downloadHandler(
    filename = 'test.pdf',
    content = function(file) {
      pdf(file = file, width=12, height=4)
      print(ems())
      dev.off()
    }
  )
  
  output$download2<-  downloadHandler(
    filename = 'test.png',
    content = function(file) {
      png(file = file, width=1200, height=400)
      print(ems())
      dev.off()
    }
  )
  output$downloadPlot3 <- downloadHandler(
    filename = function() { paste('emotion',Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(tbl(), file)
    }
  )
})