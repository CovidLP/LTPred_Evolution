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
  
  ## Titulo
  titlePanel("Evolucao da previsao COVID-19 - DEST/UFMG"),
  # tagList(
  #   tags$head(
  #     tags$title(
  #       "Evolucao da previsao COVID-19 - DEST/UFMG"
  #     )
  #   )),
  
  fluidRow(
    column(width = 10, offset = 1,
           wellPanel(
             h4("Selecione o pais/estado que deseja investigar.", style = "text-align: center;"),
             h4("Select a country/state to analyse.", style = "text-align: center;"),
             HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>Sobre/About</a> | <a href = 'http://est.ufmg.br/covidlp/home/pt/metodologia' style = 'color: #00b1d8; text-decoration: underline;'>Metodologia/Methodology</a></center>"),
             # h5(sprintf("Ultima atualizacao/last update: %s", Sys.Date()), style = "text-align: center;"),
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
             tipify(bsButton("pB2", "?", style = "inverse", size = "extra-small"),
                    HTML("Selecione o pais e o estado de interesse! <br> Select a country and a state!"))
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
           imageOutput("distPlot")),
  fluidRow( 
    align = "center",
    sliderTextInput(
      "bins",
      "Data:",
      choices = read.table(paste0("GraphsPng/Brazil_n/date.vector.txt"))$V1,
      animate = animationOptions(
        interval = 750,
        loop = FALSE,
        playButton = NULL,
        pauseButton = NULL
      )
      
    )
    
  ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
        choices = read.table(paste0("GraphsPng/", str_replace_all(input$pais," ","-"), "_n/date.vector.txt"))$V1
      } else{
        choices = read.table(paste0("GraphsPng/", str_replace_all(input$pais," ","-"), "_d/date.vector.txt"))$V1
      }
    }
    
    else if (input$state=="<all>") {
      if (input$tipo_data == TRUE) {
        
        choices = read.table(paste0("GraphsPng/", str_replace_all(input$pais," ","-"), "_n/date.vector.txt"))$V1
        
      } else{
        
        choices = read.table(paste0("GraphsPng/", str_replace_all(input$pais," ","-"), "_d/date.vector.txt"))$V1
        
      }
      
    } else{
      
      if (input$tipo_data == TRUE) {
        
        choices = read.table(paste0("GraphsPng/", input$pais,"_",input$state, "_ne/date.vector.txt"))$V1
        
      } else{
        
        choices = read.table(paste0("GraphsPng/", input$pais,"_",input$state, "_de/date.vector.txt"))$V1
        
      }
    }
    updateSliderTextInput(
      session = session,
      inputId = "bins",
      choices = choices
    )
  }, ignoreInit = TRUE)
  
  outfile <- reactive({
    
    url_init = "https://raw.githubusercontent.com/CovidLP/LTPred_Evolution/main/GraphsPng/"
    
    state = as.character(input$state)
    
    if (state != "<all>") {
      local = paste0("Brazil_",
                     as.character(input$state),
                     "_",
                     ifelse(input$tipo_data, "ne", "de"))
    } else{
      local = paste0(as.character(input$pais),
                     "_",
                     ifelse(input$tipo_data, "n", "d"))
    }
    
    out = paste0(url_init, local, "/", as.character(input$bins), ".png")
    return(out)
    
  })
  
  
  output$distPlot <- renderImage({
    img_file <- tempfile(fileext = ".png")
    download.file(url = outfile(), destfile = img_file, mode = "wb", quiet = TRUE)
    list(src = img_file)
  }, deleteFile = TRUE)
  
}

# Run the application
shinyApp(ui = ui, server = server)
