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
                label="Selecione um país:",
                choices=sort(c("Venezuela","Ethiopia","Peru")),
                selected = "Venezuela",
            )
            
            
            
            
            # sliderInput("bins",
            #             "Data:",
            #             min = 1,
            #             max = 89,
            #             value = 1,
            #             step=1,
            #             label = as.Date(c("2021-01-15", "2021-01-19", "2021-01-20", "2021-01-21", "2021-01-22", "2021-01-25", "2021-01-29",
            #                               "2021-02-01", "2021-02-05", "2021-02-08", "2021-02-12", "2021-02-15", "2021-02-19", "2021-02-22",
            #                               "2021-02-26", "2021-03-01", "2021-03-05", "2021-03-08", "2021-03-12", "2021-03-15", "2021-03-19",
            #                               "2021-03-22", "2021-03-26", "2021-03-29", "2021-04-02", "2021-04-05", "2021-04-09", "2021-04-12",
            #                               "2021-04-16", "2021-04-19", "2021-04-23", "2021-04-26", "2021-04-30", "2021-05-03", "2021-05-07",
            #                               "2021-05-10", "2021-05-14", "2021-05-17", "2021-05-21", "2021-05-24", "2021-05-28", "2021-05-31",
            #                               "2021-06-04", "2021-06-07", "2021-06-11", "2021-06-14", "2021-06-18", "2021-06-21", "2021-06-25",
            #                               "2021-06-28", "2021-07-02", "2021-07-05", "2021-07-09", "2021-07-12", "2021-07-16", "2021-07-19",
            #                               "2021-07-23", "2021-07-26", "2021-07-30", "2021-08-02", "2021-08-06", "2021-08-09", "2021-08-13",
            #                               "2021-08-16", "2021-08-20", "2021-08-23", "2021-08-27", "2021-08-30", "2021-09-03", "2021-09-06",
            #                               "2021-09-10", "2021-09-13", "2021-09-17", "2021-09-20", "2021-09-24", "2021-09-27", "2021-10-01",
            #                               "2021-10-04", "2021-10-08", "2021-10-11", "2021-10-15", "2021-10-18",
            #                               "2021-10-21", "2021-10-24", "2021-10-28", "2021-10-31", "2021-11-04", "2021-11-07", "2021-11-11")),
            # 
            #             # value = as.Date(c("2021-01-15", "2021-01-19", "2021-01-20", "2021-01-21", "2021-01-22", "2021-01-25", "2021-01-29", "2021-02-01", "2021-02-05", "2021-02-08", "2021-02-12", "2021-02-15", "2021-02-19", "2021-02-22", "2021-02-26", "2021-03-01", "2021-03-05", "2021-03-08", "2021-03-12", "2021-03-15", "2021-03-19", "2021-03-22", "2021-03-26", "2021-03-29", "2021-04-02", "2021-04-05", "2021-04-09", "2021-04-12", "2021-04-16", "2021-04-19", "2021-04-23", "2021-04-26", "2021-04-30", "2021-05-03", "2021-05-07", "2021-05-10", "2021-05-14", "2021-05-17", "2021-05-21", "2021-05-24", "2021-05-28", "2021-05-31", "2021-06-04", "2021-06-07", "2021-06-11", "2021-06-14", "2021-06-18", "2021-06-21", "2021-06-25", "2021-06-28", "2021-07-02", "2021-07-05", "2021-07-09", "2021-07-12", "2021-07-16", "2021-07-19", "2021-07-23", "2021-07-26", "2021-07-30", "2021-08-02", "2021-08-06", "2021-08-09", "2021-08-13", "2021-08-16", "2021-08-20", "2021-08-23", "2021-08-27", "2021-08-30", "2021-09-03", "2021-09-06", "2021-09-10", "2021-09-13", "2021-09-17", "2021-09-20", "2021-09-24", "2021-09-27", "2021-10-01", "2021-10-04", "2021-10-08", "2021-10-11", "2021-10-15", "2021-10-18", "2021-10-21", "2021-10-24", "2021-10-28", "2021-10-31", "2021-11-04", "2021-11-07", "2021-11-11"),
            #             animate = TRUE)
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
                             choices = names(VenezuelaGraph),
                             animate = TRUE
                             #grid=TRUE
    ))
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
    # arquivo=paste0(input$pais,"Graph.rds")
     
    
    output$distPlot <- renderPlot({
        ajustes <- readRDS(as.character(arquivo()))
        #ajustes <- readRDS("VenezuelaGraph.rds")
        seqDate = names(ajustes)
        ajustes[[input$bins]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
