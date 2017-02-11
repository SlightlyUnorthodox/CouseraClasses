shinyServer(function(input, output) {
  
  output$table <- renderDataTable(
    tab <- read.table(url(input$url),header = input$header,sep = input$space,na.strings = c("NA","","na","!#DIV/0"))  
  )
  
  output$table2 <- renderDataTable(
    tab2 <<- read.table(url(input$url),header = input$header,skip = input$skip, sep = input$space,na.strings = c("NA","","na","!#DIV/0")),
    if(input$nas == TRUE) {
      tab2 <- tab2[,which(apply(tab2,2,function(x) {sum(is.na)}) == 0)]
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$fileName, '.csv',sep='')
    },
    content = function(file) {
      write.csv(tab2, file)
    }
  )
})