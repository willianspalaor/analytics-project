if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(dygraphs)) { install.packages("dygraphs"); require(dygraphs)}
if(!require(devtools)) { install.packages("devtools"); require(devtools)}
if(!require(MASS)) { install.packages("MASS"); require(MASS)}
if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}
if(!require(plotly)) { devtools::install_github("ropensci/plotly"); require(plotly)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(forecast)){install.packages("forecast"); require(forecast)}

shinyServer(function(input, output, session) {
  
  
  #Lê os dado
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = T, sep = input$sep)
  })
  
  
  # Armazena os dados
  data <- reactive({
    df = filedata()
    if (is.null(df)) return(NULL)
  })
  
  
  # Mostra os dados
  output$dados = renderDataTable({
    if(is.null(filedata())){
      validate(need(input$file != "", "Selecione o arquivo a ser utilizado"))
    }else{
      filedata()
    }
  }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))
  
  
  # Template
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Template_Serie_Temporal", "csv", sep = ".")
    },
    content = function(file) {
      write.table(economics, file,row.names = FALSE, sep=",")
    }
  )
  
  # Função para formatar os campos obrigatórios
  requiredInput <- function(titulo){
    paste(titulo, "<font color='red'>*</font>")
  }
  
  # Função para formatar os campos obrigatórios
  requiredText <- function(texto){
    paste("<font color = 'red'>", texto, "</font>")
  }
  
  
  # Box método
  metodoUI <- reactive ({
      column(width=12,
        box(
          title = "Método", status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE, width = NULL,
          selectInput("metodo", HTML(requiredInput("Método")),choices = c("Regressão Linear Simples","Suavização Exponencial"))
        ),
        box(
          title = "Variáveis", status = "primary", solidHeader = FALSE, collapsible = TRUE, width = NULL,
          uiOutput("xcol"),
          uiOutput("ycol")
        ),
        box(title = "Previsão", status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed=TRUE,width = NULL,
            sliderInput("months", "Quantidade de Meses:", 
                        min=12, max=144, value=12, step = 12), 
            sliderInput("interval", "Intevalo de confiança:", 
                        min=0.80, max=0.99, value=0.80, step=0.05), 
            hr(),
            checkboxInput("showgrid", label = "Mostrar grid", value = TRUE),
           # checkboxInput("sazonalidade", label = "Efeito Sazonal", value = TRUE),
            checkboxInput("decompor", label = "Decompor série", value = FALSE)
            
        )
      )
  })
  
  # Box opções regressão
  optionsRegressaoUI <- reactive(
    box(title = "Regressão", status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed=TRUE,width = NULL,
      checkboxInput("log", label = "Aplicar log", value = FALSE)
        
    )
  )
  
  # Renderiza as opções
  output$configuracoes <- renderUI ({
    if(input$metodo == '') return(NULL)
  })
  
  # Renderiza o gráfico da previsão
  previsaoUI <- reactive({
    column(width=12,
      dygraphOutput("previsao"),br(),br(),
      helpText("Clique e arraste para aumentar o zoom (duplo clique para diminuir).")
    )
  })
  
  # Renderiza o gráfico da suavização
  suavizacaoUI <- reactive({
    column(width=12,
      plotlyOutput("suavizacao"),br(),br(),br(),
      dygraphOutput("previsao")
    )
  })
  
  # Renderiza o gráfico da regressão
  regressaoUI <- reactive({
    column(width=12,
      plotlyOutput("regressao"),br(),br(),br(),hr(),
      dygraphOutput("previsao")
    )
  })
  
  
  # Conteúdo do gráfico
  output$graficoContent <- renderUI ({
    validate(need(input$file != "", "Selecione o arquivo a ser utilizado"))
    validate(need(input$metodo != "", "Selecione o método a ser aplicado"))
    if(input$metodo != ''){
      validate( 
        need(input$x != "", "Informe a variável x"),
        need(input$y != "", "Informe a variável y")
      )
      
      if(input$metodo == "Suavização Exponencial"){suavizacaoUI()}
      else if(input$metodo == "Regressão Linear Simples"){regressaoUI()}
    }
  })
  
  
  # Interface do Gráfico
  graficoUI <- reactive ({
    uiOutput("graficoContent")
  })
  
  # Gráficos
  output$graficos = renderUI({
    df <-filedata()
    if(is.null(df)){
      graficoUI()
    }else{
      column(width=12,
        column(width=9,graficoUI()),
        column(width=3,metodoUI(),uiOutput("configuracoes"))
      )
    }
  })
  
  
  # Input X
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items = names(df)
    items <- append(items, "", after=0)
    names(items)=items
    selectInput("x", HTML(requiredInput("Variável x")),items)
  })
  
  
  # Input Y
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items = names(df)
    items <- append(items, "", after=0)
    names(items)=items
    selectInput("y", HTML(requiredInput("Variável y")),items)
  })
  
  
  # Renderiza o gráfico de regressão
  output$regressao <- renderPlotly({
    
    # Recupera o arquivo
    df <- filedata()
    
    # Recupera a variável x e y
    xcol = df[[input$x]]
    ycol = df[[input$y]]
    
    # Realiza o filtro para achar o maior valor
    filtro <- filter(df, ycol == max(ycol))
    xfiltered = filtro[[input$x]]
    yfiltered = filtro[[input$y]]
    
    # Monta o gráfico
    p <- plot_ly(df, x = xcol, y = ycol, mode = "markers", name = input$y)
    regressao <- fitted(lm(as.numeric(ycol) ~ as.numeric(xcol), data = df))
   # serie <- ts(ycol, start=c(1967,6),end=c(2007,3), frequency=12)
   # regressao <- fitted(tslm(serie) ~ trend + season), data = df)

    
    # Configura os títulos x e y do gráfico
    xcolumn <- list(title = input$x)
    ycolumn <- list(title = input$y)
  
    # Configura o gráfico
    p %>%
      
      add_trace(p, y = as.numeric(regressao), mode = "lines", name="Ajustado") %>%
      layout(title = "Regressão Linear",showlegend = TRUE) %>%
      layout(xaxis = xcolumn, yaxis = ycolumn)
      #layout(annotations = list(x = xfiltered, y = yfiltered, text = "Pico", showarrow = T))
  })
  
  
  # Renderiza o gráfico de regressão
  output$previsao2 <- renderPlotly({
    
    # Recupera o arquivo
    df <- filedata()
    
    # Recupera a variável x e y
    xcol = df[[input$x]]
    ycol = df[[input$y]]
    
    # Encontra a maior data
    maiordata <- max(as.Date(xcol))
    xValues <- seq(as.Date(maiordata), by="month", length.out=12)
    previsao <- as.data.frame(predicted())
    
    
    # Monta o gráfico
    p <- plot_ly(df, x = xValues, y = previsao$fit, name = "fit")
    
    
    l <- list(
      color = toRGB("gray90", alpha = 0.3),
      fillcolor = toRGB("gray90", alpha = 0.3)
    )
    
    p %>%
      add_trace(p, data = previsao, y = upr, mode = "lines",fill = "tonexty", line = l, tittle="upr", name = "upr") %>%
      add_trace(p, data = previsao, y = lwr, mode = "lines",fill = "tonexty", line = l, tittle="lwr", name = "lwr")
  })
  
  
  
  # Renderiza o gráfico de suavização
  output$suavizacao <- renderPlotly({
    
    # Recupera o arquivo
    df <- filedata()
    
    # Recupera a variável x e y
    xcol = df[[input$x]]
    ycol = df[[input$y]]
    
    # Monta o gráfico
    p <- plot_ly(df, x = xcol, y = ycol, name = input$y, mode = 'markers')
    
    # Faz o ajuste dos dados
    ajuste <- fitted(loess(ycol ~ as.numeric(xcol), data = df))
    
    # Encontra a maior data
    maiordata <- max(as.Date(xcol))
    maiorAno <- as.numeric(format(maiordata, "%Y"))
    maiorMes <- as.numeric(format(maiordata, "%m"))
    
    # Encontra a menor data
    menorData <- min(as.Date(xcol))
    menorAno <- as.numeric(format(menorData, "%Y"))
    menorMes <- as.numeric(format(menorData, "%m"))
    
    # Realiza o filtro para achar o maior valor
    filtro <- filter(df, ycol == max(ycol))
    xfiltered = filtro[[input$x]]
    yfiltered = filtro[[input$y]]
  
    # Configura os títulos x e y do gráfico
    xcolumn <- list(title = input$x)
    ycolumn <- list(title = input$y)
    
    # Configura o gráfico
    p %>%
      
      add_trace(p, y = ajuste, mode = "lines", name="Ajustado") %>%
      layout(title = "Suavização Exponencial",showlegend = TRUE) %>%
      layout(xaxis = xcolumn, yaxis = ycolumn)
     # layout(annotations = list(x = xfiltered, y = yfiltered, text = "Pico", showarrow = T))
    
  })
  
  # Previsão exponencial
  exponencialPredicted <- reactive({
    
    # Recupera o arquivo
    df <- filedata()
    
    # Recupera a variável x e y 
    ycol = df[[input$y]]
    data = df[["date"]]
    xcol = df[[input$x]]
    
    # Encontra a maior data
    maiordata <- max(as.Date(data))
    maiorAno <- as.numeric(format(maiordata, "%Y"))
    maiorMes <- as.numeric(format(maiordata, "%m"))
    
    # Encontra a menor data
    menorData <- min(as.Date(data))
    menorAno <- as.numeric(format(menorData, "%Y"))
    menorMes <- as.numeric(format(menorData, "%m"))
    
    #Monta a série temporal
    ajuste <- fitted(loess(ycol ~ as.numeric(xcol), data = df))
    serie <- ts(ajuste, start=c(menorAno,menorMes),end=c(maiorAno,maiorMes), frequency=12)
    
    #if(input$sazonalidade){hw <- HoltWinters(serie)}
    #else{hw <- HoltWinters(serie, gamma = FALSE)}
    
    hw <- HoltWinters(serie)
    
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  
  # Previsão exponencial
  linearPredicted <- reactive({
    
    # Recupera o arquivo
    df <- filedata()
    
    # Recupera a variável x e y 
    ycol = df[[input$y]]

    #if(is.null(df$date)){
    xcol = df[[input$x]]
    
    data = df[["date"]]
    
    # Encontra a maior data
    maiorData <- max(as.Date(data))
    maiorAno <- as.numeric(format(maiorData, "%Y"))
    maiorMes <- as.numeric(format(maiorData, "%m"))
    
    # Encontra a menor data
    menorData <- min(as.Date(data))
    menorAno <- as.numeric(format(menorData, "%Y"))
    menorMes <- as.numeric(format(menorData, "%m"))
    
    #Monta a série temporal
    regressao <- fitted(lm(as.numeric(ycol) ~ as.numeric(xcol), data = df))
    serie <- ts(regressao, start=c(menorAno,menorMes),end=c(maiorAno,maiorMes), frequency=12)
    linear <- tslm(serie ~ trend + season)
    previsao <- predict(linear$x, h=input$months, level = as.numeric(input$interval))
    
    # Recupera o período da previsão
    periodo <- as.Date(previsao$mean)
    
    # Encontra a maior data
    maiorData <- max(as.Date(periodo))
    maiorAno <- as.numeric(format(maiorData, "%Y"))
    maiorMes <- as.numeric(format(maiorData, "%m"))
    
    # Encontra a menor data
    menorData <- min(as.Date(periodo))
    menorAno <- as.numeric(format(menorData, "%Y"))
    menorMes <- as.numeric(format(menorData, "%m"))
    
    # Recupera os dados da previsão
    fit <- as.data.frame(previsao$mean)
    fit <- as.numeric(fit$x)
    upper <- as.data.frame(previsao$upper)
    upper <- as.numeric(upper$'Series 1')
    lower <- as.data.frame(previsao$lower)
    lower <- as.numeric(lower$'Series 1')
    
    rmatrix <- matrix( c(fit, upper, lower), nrow=input$months, ncol=3)
    colnames(rmatrix) <- c("fit", "upr", "lwr")
    
    ts(rmatrix,start=c(menorAno,menorMes),end=c(maiorAno,maiorMes), frequency=12)
  })
  
  
  # Renderiza o gráfico de previsão
  output$previsao <- renderDygraph({
    
    if(input$metodo == "Suavização Exponencial"){
      if(input$decompor){
        dygraph(exponencialPredicted(), main = "Previsão") %>%
          dySeries(label = "fit") %>%
          dyOptions(drawGrid = input$showgrid)
      }else{
        dygraph(exponencialPredicted(), main = "Previsão") %>%
          dySeries(c("lwr", "fit", "upr"), label = "fit") %>%
          dyOptions(drawGrid = input$showgrid)
      }
    }else if(input$metodo == "Regressão Linear Simples"){
      if(input$decompor){
        dygraph(linearPredicted(), main = "Previsão") %>%
         dySeries(label = "fit") %>%
         dyOptions(drawGrid = input$showgrid)
      }else{
          dygraph(linearPredicted(), main = "Previsão") %>%
          dySeries(c("lwr", "fit", "upr"), label = "fit") %>%
          dyOptions(drawGrid = input$showgrid)
      }
   }

  })
  
})