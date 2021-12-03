#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Evolução das Previsões"),
    
    # Sidebar with a slider input for number of bins 
    
        fluidRow(align= "center",
            # sliderInput(
            #     inputId = "slider",
            #     label = "",
            #     min = Sys.Date()-1,
            #     max = Sys.Date(),
            #     value = Sys.Date(),
            #     animate = TRUE
            # )
            selectInput(
                inputId="pais",
                label="Selecione um pais:",
                choices=sort(c("Venezuela","Ethiopia","Peru")),
                selected = "Venezuela",
            )
            
            
            
            
            
            
        ),
    fluidRow(align="center",
             switchInput(
                 inputId = "tipo_data",
                 onLabel = "Confirmed",
                 offLabel = "Deaths",
                 offStatus = "danger",
                 value = TRUE
             )
    ),

        # Show a plot of the generated distribution
        fluidRow(align="center",
           plotOutput("distPlot")
        
    ),
    fluidRow(align="center",
             sliderTextInput("bins",
                             "Data:",
                             choices = names(readRDS("Venezuela_n_graph.rds")),
                             animate = TRUE
                             #grid=TRUE
    ),
    animationOptions(
      interval = 1,
      loop = FALSE,
      playButton = NULL,
      pauseButton = NULL
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    arquivo=reactive({
        arquivo=as.character(input$pais)
        if (input$tipo_data==TRUE) {
            endfile="_n_graph.rds"
        } else{
            endfile="_d_graph.rds"
        }
        arquivo=paste0(arquivo,endfile)
        return(arquivo)
    })
    
    
    output$distPlot <- renderPlot({
        ajustes <- readRDS(as.character(arquivo()))
        
        seqDate = names(ajustes)
        ajustes[[input$bins]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
