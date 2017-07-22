if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(dygraphs)) { install.packages("dygraphs"); require(dygraphs)}
if(!require(devtools)) { install.packages("devtools"); require(devtools)}
if(!require(MASS)) { install.packages("MASS"); require(MASS)}
if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}
if(!require(plotly)) { devtools::install_github("ropensci/plotly"); require(plotly)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}


#Header
dashHeader <- dashboardHeader(title = "Sistema Analytics")

#sidebar
dashSidebar <- dashboardSidebar(        
  
  sidebarMenu(
    
    #Item Dashboard
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    
    #Item Suavização
    menuItem("Análises Temporais", icon = icon("bar-chart"), tabName = "temporais", badgeColor = "green"),
    
    #Item Regressão
    menuItem("Análises Geográficas", tabName = "regressao", icon = icon("random"), badgeColor = "green")
  )
) 


# Corpo
dashBody <- dashboardBody(
  
  #Header
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"), tags$title("Analytics")),
  
  #Itens do menu lateral
  tabItems(
    
    #Item Dashboard
    tabItem( tabName = "dashboard",
             fluidRow(box(title = "Analytcs", status = "primary", solidHeader = TRUE, width = 8,img(src = "favicon.png", height = 50, width = 50), h3("Bem Vindo ao Analytics!"),h4("Analytics é uma aplicação web ",a(href = 'http://shiny.rstudio.com', 'Shiny'),"desenvolvida para aplicar métodos estatísticos."),h4("Clique em uma das opções no painel lateral para começar.")))
    ),
    
    #Item Suavização
    tabItem(tabName = "temporais",
            
            fluidRow(
              br(),
              column(width=4,
                     box(title = "Template", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = NULL, height=140,
                         br(),p('Clique para baixar o template de um arquivo de série temporal:', downloadButton('downloadData', 'Template'))
                     )
              ),
              column(width=4,
                     box(title = "Arquivo", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = NULL, height=140,
                         fileInput('file', 
                                   strong('Arquivo:'),
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     'text/tab-separated-values',
                                     'text/plain',
                                     '.csv',
                                     '.tsv'
                                   )
                         )
                     )
              ),
              column(width=4,
                     box(title = "Configurações", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = NULL, height=140,
                         selectInput('sep', strong('Separador'),c('vírgula'=',','ponto e vírgula'=';','espaço'='\t'),',')
                     )
              )
            ),
            fluidRow(
              br(),
              tabBox(width=12,
                     tabPanel("Dados",
                              br(),       
                              fluidRow(column(width=12,dataTableOutput("dados")))
                     ),
                     tabPanel("Gráficos",
                              br(),
                              fluidRow(column(width=12,uiOutput("graficos")))
                     )
              )
            )
    )
  )
)


# Monta a interface
shinyUI(
  fluidPage(dashboardPage(skin="black",dashHeader,dashSidebar,dashBody))
)
