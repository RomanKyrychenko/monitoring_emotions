library(shiny)
library(shinythemes)

shinyServer(function(input, output){
  tes <- reactive({
    validate(
      need(input$file1 != "", "Будь ласка, оберіть фото для аналізу")
    )
    inFile <- input$file1
    inFile
  })
  
  te <- reactive({
    if (is.null(tes())) {
      return(NULL)
    } else {
      body <- lapply(tes()$datapath, upload_file)
    }
    body
  })



  test <- reactive({
  face_api_url = "https://api.projectoxford.ai/emotion/v1.0/recognize"
  
  test <- lapply(te(),function (x) {
    a <- as.data.frame(
      content(
        POST(
          face_api_url, body = x,
          add_headers(.headers = c("Content-Type"="application/octet-stream",
                                   "Ocp-Apim-Subscription-Key"="4423ea1091254e7497d0e35bcc51c462")))))
    a
    })
  })
  
  df <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    a <- test[,c(input$emo,"time")]
    b <- reshape2::melt(a, id.vars=c("time"))
    b$colors <- ifelse(b$variable=="Злість","#f15d63",
                       ifelse(b$variable=="Зневага","#a4d06f",
                              ifelse(b$variable=="Відраза","#cd956e",
                                     ifelse(b$variable=="Страх","#9277b6",
                                            ifelse(b$variable=="Радість","#ffe155",
                                                   ifelse(b$variable=="Нейтральність","#cdcfd0",
                                                          ifelse(b$variable=="Сум","#62abd2","#f6afce")))))))
    ggplot(b,aes(time,value)) + 
      geom_jitter(fill=b$colors,color=b$colors, alpha=1)+
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
  en <- reactive({
    test <- do.call("smartbind", test())
    test$time <- c(1:nrow(test))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    a <- test[,c(input$emo,"time")]
    b <- reshape2::melt(a, id.vars=c("time"))
    c <- b %>% group_by(variable) %>% summarise(value=mean(value))
    c$colors <- ifelse(c$variable=="Злість","#f15d63",
                       ifelse(c$variable=="Зневага","#a4d06f",
                              ifelse(c$variable=="Відраза","#cd956e",
                                     ifelse(c$variable=="Страх","#9277b6",
                                            ifelse(c$variable=="Радість","#ffe155",
                                                   ifelse(c$variable=="Нейтральність","#cdcfd0",
                                                          ifelse(c$variable=="Сум","#62abd2","#f6afce")))))))
    c
    
  })
  
  output$contents2 <- renderTable({
    en()
  })
  
  ems <- reactive({
    ggplot(en(),aes(reorder(variable,-value),value, fill=variable)) + geom_bar(stat = "identity") +
      scale_fill_manual(values = en()$colors) +
      scale_y_continuous(labels = percent) +
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
    test$name <- tes()$datapath
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    tryCatch(test[c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування","name")])
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
  output$download3 <- downloadHandler(
    filename = function() { paste('emotion',Sys.Date(), '.xlsx', sep='') },
    content = function(file) {
  write.xlsx(tbl(), file, sheetName = "emotions", row.names = FALSE)
    }
  )
  output$downloadPlot4 <- downloadHandler(
    filename = function() { paste('emotion_sum',Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(en(), file)
    }
  )
  output$download4 <- downloadHandler(
    filename = function() { paste('emotion_sum',Sys.Date(), '.xlsx', sep='') },
    content = function(file) {
      write.xlsx(en(), file, sheetName = "emotions_sum", row.names = FALSE)
    }
  )
})