pacman::p_load(ggplot2, data.table, shiny, shinythemes)
load("uik.data.Rdata")

function(input, output){
  selectedData <- reactive({
    if (input$region != "Российская Федерация"){
      dt <- subset(uik.results, region == input$region, 
                   select =c(input$axisX, input$axisY))
    }
    else{
      dt <- subset(uik.results, select =c(input$axisX, input$axisY))
    }
    setnames(dt, 1:2, c("x","y"))
    setkey(dt, x, y)
    list(
    data = dt[x >= input$axisXrange[1] & x <= input$axisXrange[2] &
               y >= input$axisYrange[1] & y <= input$axisYrange[2]],
    Xlab = paste0(names(which(choices==input$axisX)), ", %\r\n"),
    Ylab = paste0('\r\n',names(which(choices==input$axisY)), ", %")
    )
  })
  
  output$scatterPlot <- renderPlot({
    df <- selectedData()
    if (req(input$region) == "Российская Федерация"){
      opacity <- 0.02
    }
    else{
      opacity <- 0.09
    }
    if (req(input$plotType) == "scatterPlot"){
    ggplot(df$data, aes(x,y)) + geom_point(alpha = opacity) + 
        xlab(df$Xlab) + ylab(df$Ylab) +
        ggtitle(paste0("Выборы-2016 (", input$region, "): Результаты по участковым комиссиям\r\n")) +
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold"),
                         axis.title.x = element_text(face = "bold"),
                         axis.title.y = element_text(face = "bold"))
    }
    else if (input$plotType == "smoothScatter"){
      smoothScatter(df$data, xlab = df$Xlab, ylab = df$Ylab)
      title(paste0("Выборы-2016 (", input$region, "): Плотность результатов по участковым комиссиям"))
    }
  })
  
  output$source <- renderText({
    HTML("Источник:<br> Парсинг сайта ЦИК РФ, агрегированные 
          <a href='https://drive.google.com/drive/folders/0ByFMnUnpIlrib0lETWhTNkdZRVk'>данные</a>
          предоставлены Сергеем Шпилькиным")
  })
}
