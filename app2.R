library(shiny)
library(shinyWidgets)
require(tidyverse)
require(PandemicLP)
require(shinycssloaders)
require(shinydashboard)
require(shinyBS)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  ## Add css style file
  theme = "styles.css",
  ## add favicon
  tags$head(tags$link(rel = "icon", href = "img/favicon.ico")),
  
  ## Título
  titlePanel("Evolução da previsão COVID-19 - DEST/UFMG"),
  # tagList(
  #   tags$head(
  #     tags$title(
  #       "Evolução da previsão COVID-19 - DEST/UFMG"
  #     )
  #   )),
 
  fluidRow(
    column(width = 10, offset = 1,
           wellPanel(
             h4("Selecione o país/estado que deseja investigar.", style = "text-align: center;"),
             h4("Select a country/state to analyse.", style = "text-align: center;"),
             HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>Sobre/About</a> | <a href = 'http://est.ufmg.br/covidlp/home/pt/metodologia' style = 'color: #00b1d8; text-decoration: underline;'>Metodologia/Methodology</a></center>"),
             # h5(sprintf("Última atualização/last update: %s", Sys.Date()), style = "text-align: center;"),
             # HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>CovidLP website</a> | <a href = 'https://github.com/CovidLP/app_COVID19' style = 'color: #00b1d8; text-decoration: underline;'>Source code</a> <br> <a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>Sobre/About</a> | <a href = 'http://est.ufmg.br/covidlp/home/pt/metodologia' style = 'color: #00b1d8; text-decoration: underline;'>Metodologia/Methodology</a></center>"),
             # HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>CovidLP website</a></center>"),
             style = "background-color: #ffffff; border-radius: 4px; box-shadow: 0 0 4px 0 rgba(69,69,69,.2);"
           )
    ),
    style = "padding: 20px"
  ),
  fluidRow(
    HTML('<center>'),
    column(width=5,
           align='right',
           div(
      class = "btn_div",
      shinyWidgets::pickerInput(inputId = "pais", label = NULL,
                                choices =c(
                                  "Argentina", "Australia","Belgium",
                                  "Bolivia","Brazil", "Canada",
                                  "Chile","China","Colombia",
                                  "Costa Rica","Ecuador","Ethiopia",
                                  "France","Germany","Greece","Guatemala",
                                  "Honduras","India","Indonesia",
                                  "Iraq","Ireland","Italy",
                                  "Japan","Mexico","Morocco",
                                  "Netherlands","New Zealand","Norway",
                                  "Panama","Paraguay","Peru",
                                  "Poland","Portugal","Romania",
                                  "Russia","Saudi Arabia","South Africa",
                                  "South Korea","Spain","Sweden",
                                  "Switzerland","Turkey","Ukraine",
                                  "United Kingdom","United States of America",
                                  "Uruguay","Venezuela"
                                ) ,
                                selected = "Brazil")
    )),
    column(width = 2,
           align="center",
           div(
             class = "btn_div",
             tipify(bsButton("pB2", "?", style = "inverse", size = "extra-small"), HTML("Selecione o país e o estado de interesse! <br> Select a country and a state!"))
           )),
    column(width=5,
           align='left',
           div(
      class = "btn_div",
      shinyWidgets::pickerInput(inputId = "state", label = NULL, choices = "<all>", selected = "<all>"),
    ))),
    
  fluidRow(
    align = "center",
    switchInput(
      inputId = "tipo_data",
      onLabel = "Confirmed",
      offLabel = "Deaths",
      offStatus = "danger",
      value = TRUE
    )
  ),
  
  # Show a plot of the generated distribution
  fluidRow(align = "center",
           plotOutput("distPlot")),
  fluidRow(
    align = "center",
    sliderTextInput(
      "bins",
      "Data:",
      choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/Brazil_n_graph.rds")))),
      animate = animationOptions(
        interval = 500,
        loop = FALSE,
        playButton = NULL,
        pauseButton = NULL
      )
      
    )
    
  ))


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  observeEvent(input$pais, {
    if(input$pais == "Brazil") {
      states <-c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
                 "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
    } else { 
        states <- NULL
    }
    
    states <- c("<all>", states)
    sel <- ifelse(input$state %in% states, input$state, "<all>")
    
    updatePickerInput(session = session, inputId = "state", choices = states, selected = sel)
  })
  
  
  observeEvent(c(input$pais,input$state,input$tipo_data), {

    if (input$pais!="Brazil") {
      if (input$tipo_data == TRUE) {
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",str_replace_all(input$pais," ","-"), "_n_graph.rds"))))
      } else{
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",str_replace_all(input$pais," ","-"), "_d_graph.rds"))))
      }
    }
    
    else if (input$state=="<all>") {
      if (input$tipo_data == TRUE) {
        
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",str_replace_all(input$pais," ","-"), "_n_graph.rds"))))
        
      } else{
        
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",str_replace_all(input$pais," ","-"), "_d_graph.rds"))))
        
      }
     
    } else{
      
      if (input$tipo_data == TRUE) {
        
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",input$pais,"_",input$state, "_ne_graph.rds"))))

      } else{
        choices = names(readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",input$pais,"_",input$state, "_de_graph.rds"))))
      }
    }
    updateSliderTextInput(
      session = session,
      inputId = "bins",
      choices = choices
    )
  }, ignoreInit = TRUE)
  
  
  arquivo = reactive({
    pais=as.character(input$pais)
    pais=str_replace_all(pais," ","-")
    if (pais!="Brazil") {
      if (input$tipo_data == TRUE) {
        endfile = "_n_graph.rds"
      } else{
        endfile = "_d_graph.rds"
      }
      arquivo = paste0(pais, endfile)
    }
    else if (input$state=="<all>") {
      if (input$tipo_data == TRUE) {
        endfile = "_n_graph.rds"
      } else{
        endfile = "_d_graph.rds"
      }
      arquivo = paste0(pais, endfile)
    } else{
      startfile = paste0(pais,"_")
      state=as.character(input$state)
      if (input$tipo_data == TRUE) {
        endfile = "_ne_graph.rds"
      } else{
        endfile = "_de_graph.rds"
      }
      
      arquivo = paste0(startfile, state, endfile)
    }
    return(arquivo)
  })
  output$distPlot <- renderPlot({
    ajustes <- readRDS(url(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/Graphs/",as.character(arquivo()))))
    seqDate = names(ajustes)
    ajustes[[input$bins]]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
