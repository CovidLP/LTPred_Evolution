library(shiny)
library(shinyWidgets)
require(tidyverse)
require(PandemicLP)
require(shinycssloaders)
require(shinydashboard)
require(shinyBS)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidPage(
    ## Add css style file
    theme = "styles.css",
    
    ## Include Google Analytics
    # tags$head(includeHTML(("html/google-analytics.html"))),
    
    ## Habilitando o shinyjs
    useShinyjs(),
    
    ## add favicon
    tags$head(tags$link(rel = "icon", href = "img/favicon.ico")),
    
    ## Título
    tagList(
      tags$head(
        tags$title(
          "Previsão COVID-19 - DEST/UFMG"
        )
      ),
      
      fluidRow(
        # Logo
        column(
          width = 2,
          tags$a(
            href = "http://est.ufmg.br/covidlp/home/pt/",
            tags$img(src = "img/logo_covid.png", title = "DEST/UFMG", height = "40px", style = "margin-top: 10px; margin-left: 20px")
          )
        ),
        ## Título
        column(
          width = 8,
          h4(
            strong("Evolução das previsões longo prazo para COVID-19"),
            style = "text-align: center"
          ),
          h5("Long term prediction evolution for COVID-19", style = "text-align: center")
        ),
        ## github, webpage, email
        column(
          width = 1, offset = 1,
          br(),
          tags$a(icon("github"), href = "https://github.com/CovidLP/app_COVID19"),
          tags$a(icon("globe"),  href = "http://est.ufmg.br/covidlp/home/pt/"),
          tags$a(icon("envelope"),  href = "mailto:covidlp.team@gmail.com")
        ),
        style = "background-color: white; margin: 0; padding: 0;",
      )
    )
  ),
  
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
           )),
    
    HTML("</center>"),
    style = "background-color: #00b1d8;
               border-radius: 4px;
               box-shadow: 0 0 4px 0 rgba(69, 69, 69, 0.2);
               margin: 0px 10px 30px 10px; padding: 10px;"
    
  ),
  
  HTML("<center>"),
  HTML("<h4 style = 'text-align: center;'>Selecione o tipo de caso que deseja investigar.<br>Select the kind of data to analyze.</h4>"),
  div(
    class = "btn_div",
    radioGroupButtons(
      inputId = "metrics",
      label = NULL,
      choices = c("Confirmados/Confirmed" = "Confirmed", "Mortes/Deaths" = "Deaths" ),
      status = "primary", 
      individual = TRUE,
      checkIcon = list(
        yes = icon("ok", lib = "glyphicon"),
        no = icon("remove",lib = "glyphicon")
      )
    )
    # bsTooltip(id = "metrics",
    #           title = HTML("Selecione uma das métricas para ser apresentada. <br> Select one metric to display."),
    #           placement = "right", 
    #           options = list(container = "body")
    # )
  ),
  
  
  # fluidRow(
  #   align = "center",
  #   switchInput(
  #     inputId = "tipo_data",
  #     onLabel = "Confirmed",
  #     offLabel = "Deaths",
  #     offStatus = "danger",
  #     value = TRUE
  #   )
  # ),
  
  fluidRow(align = "center",
           sliderTextInput(
             "bins",
             "Data:",
             choices = read.table(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/GraphsPng/Argentina_n/date.vector.txt"))$V1,
             animate = animationOptions(
               interval = 750,
               loop = FALSE,
               playButton = NULL,
               pauseButton = NULL
             ),
             width = "40%"
           )
  ),
  
  # Show a plot of the generated distribution
  fluidRow(align = "center",
           imageOutput("distPlot")
           
  )
  
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    
    if (as.character(input$state) != "<all>") {
      local = paste0("Brazil_",
                     as.character(input$state),
                     "_",
                     ifelse(input$metrics == "Confirmed", "ne", "de"))
    } else{
      local = paste0(str_replace_all(input$pais," ","-"),
                     "_",
                     ifelse(input$metrics == "Confirmed", "n", "d"))
    }
    
    choices = read.table(paste0("https://github.com/CovidLP/LTPred_Evolution/raw/main/GraphsPng/", local, "/date.vector.txt"))$V1
    
    updateSliderTextInput(
      session = session,
      inputId = "bins",
      choices = choices,
      selected = choices[1]
    )
    
  })
  
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
  
  outfile <- reactive({
    
    url_init = "https://raw.githubusercontent.com/CovidLP/LTPred_Evolution/main/GraphsPng/"
    
    state = as.character(input$state)
    
    if (state != "<all>") {
      local = paste0("Brazil_",
                     as.character(input$state),
                     "_",
                     ifelse(input$metrics == "Confirmed", "ne", "de"))
    } else{
      local = paste0(str_replace_all(input$pais," ","-"),
                     "_",
                     ifelse(input$metrics == "Confirmed", "n", "d"))
    }
    
    out = paste0(url_init, local, "/", as.character(input$bins), ".png")
    return(out)
    
  })
  
  output$distPlot <- renderImage({
    img_file <- tempfile(fileext = ".png")
    download.file(url = outfile(), destfile = img_file, mode = "wb", quiet = TRUE)
    list(src = img_file, width = "70%")
  }, deleteFile = TRUE)
  
}

# Run the application
shinyApp(ui = ui, server = server)
