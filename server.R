library(shiny)
library(shinythemes)
options(rsconnect.max.bundle.size=3145728000)
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
                                   "Ocp-Apim-Subscription-Key"="335d7b5b1aeb47eebf6d69b0afd9a654")))))
    a
    })
  })
  
  df <- reactive({
    test <- do.call("smartbind", test())
    test$Час <- parse_number(sapply(strsplit(tes()$name, split="\\IMG_"), tail, 1L))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    a <- test[,c(input$emo,"Час")]
    b <- reshape2::melt(a, id.vars=c("Час"))
    b$colors <- ifelse(b$variable=="Злість","#f15d63",
                       ifelse(b$variable=="Зневага","#a4d06f",
                              ifelse(b$variable=="Відраза","#cd956e",
                                     ifelse(b$variable=="Страх","#9277b6",
                                            ifelse(b$variable=="Радість","#ffe155",
                                                   ifelse(b$variable=="Нейтральність","#cdcfd0",
                                                          ifelse(b$variable=="Сум","#62abd2","#f6afce")))))))
    ggplot(b,aes(as.POSIXct(Час-7200,origin="1975-01-01"),value)) + 
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
    test$Час <- parse_number(sapply(strsplit(tes()$name, split="\\IMG_"), tail, 1L))
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    a <- test[,c(input$emo,"Час")]
    b <- reshape2::melt(a, id.vars=c("Час"))
    c <- b %>% group_by(variable) %>% summarise(value=mean(value,na.rm=T))
    names(c)<- c("Емоція","Середнє значення")
    c$Колір <- ifelse(c$Емоція=="Злість","#f15d63",
                       ifelse(c$Емоція=="Зневага","#a4d06f",
                              ifelse(c$Емоція=="Відраза","#cd956e",
                                     ifelse(c$Емоція=="Страх","#9277b6",
                                            ifelse(c$Емоція=="Радість","#ffe155",
                                                   ifelse(c$Емоція=="Нейтральність","#cdcfd0",
                                                          ifelse(c$Емоція=="Сум","#62abd2","#f6afce")))))))
    tryCatch(c)
    
  })
  
  output$contents2 <- renderTable({
    en()[1:2]
  })
  
  ems <- reactive({
    ggplot(en(),aes(reorder(Емоція,-`Середнє значення`),`Середнє значення`, fill=Емоція)) + geom_bar(stat = "identity") +
      scale_fill_manual(values = en()$Колір) +
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
    test$Час <- parse_number(sapply(strsplit(tes()$name, split="\\IMG_"), tail, 1L))
    test$Назва <- tes()$name
    names(test)[5:12] <- c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування")
    tryCatch(test[c("Злість","Зневага","Відраза","Страх","Радість","Нейтральність","Сум","Здивування","Час","Назва")])
  })
  
  output$contents <- renderTable({
    tbl()
  }
  )
 
 output$downloadPlot <-  downloadHandler(
   filename =  function() { paste('emotion_points',Sys.Date(), '.pdf', sep='') },
   content = function(file) {
     pdf(file = file, width=12, height=4)
     print(df())
     dev.off()
   }
  )
  
  output$download<-  downloadHandler(
    filename =  function() { paste('emotion_points',Sys.Date(), '.png', sep='') },
    content = function(file) {
      png(file = file, width=1200, height=400)
      print(df())
      dev.off()
    }
    )
  output$downloadPlot2 <-  downloadHandler(
    filename =  function() { paste('emotion_sum',Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file = file, width=12, height=4)
      print(ems())
      dev.off()
    }
  )
  
  output$download2<-  downloadHandler(
    filename =  function() { paste('emotion_sum',Sys.Date(), '.png', sep='') },
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