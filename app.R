

library(shiny)
library(shinyMatrix)
library(markovchain)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Markov Chain App"),

    
    sidebarLayout(
        sidebarPanel(
            #Se necesita indicar el numero de estados que existen en la tabla
            numericInput("numEstados","Cantidad de estados",3),
            h4("Ingrese los datos de transicion de estados"),
            #Se crea una entrada de matrices
            matrixInput(
                "datos",
                value = matrix("",3,3),
                inputClass = "",
                rows = list(
                    extend = TRUE
                ),
                cols = list(
                    extend = TRUE
                ),
                class = "numeric"
            ),
            h4("Ingrese el nombre de los estados"),
            matrixInput(
                "statesNames",
                value = matrix("",3,1),
                inputClass = "",
                rows = list(
                    extend = TRUE
                ),
                cols = list(
                    extend = FALSE
                ),
                class = "character"
            ),
            textInput("primero","Nombre del primer estado de transicion",""),
            textInput("segundo","Nombre del segundo estado de transicion",""),
            sliderInput("steps",
                        "Numero de pasos:",
                        min = 1,
                        max = 20,
                        value = 10),
            actionButton(
                "btnEnviar",
                "Calcular"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "chainplot", height = "500px"),
            uiOutput("mat"),
            uiOutput("steady"),
            uiOutput("st"),
            textOutput("tran")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    pruebaMat <- eventReactive(input$btnEnviar,{
        mc <- matrix(input$datos,nrow=input$numEstados ,ncol = input$numEstados,dimnames = list(as.list(input$statesNames),as.list(input$statesNames)))
        print("Matriz:")
        mc
    })
    pasos <- eventReactive(input$btnEnviar,{ statesNames= c(input$fnam,input$snam,input$tnam)
        mc<-new("markovchain",transitionMatrix=matrix(input$datos,nrow=input$numEstados ,ncol = input$numEstados,dimnames = list(as.list(input$statesNames),as.list(input$statesNames))))
        a<-mc^input$steps
        print("Steps Probabilities: ")
        print(a)
    })
    draw <- eventReactive(input$btnEnviar,{ statesNames= c(input$fnam,input$snam,input$tnam)
        mc<-new("markovchain",transitionMatrix=matrix(input$datos,nrow=input$numEstados ,ncol = input$numEstados,dimnames = list(as.list(input$statesNames),as.list(input$statesNames))))
        plot(mc)
    })
    steadys <- eventReactive(input$btnEnviar,{ statesNames= c(input$fnam,input$snam,input$tnam)
        output$steadys <- renderText("SteadyStates:")
        mc<-new("markovchain",transitionMatrix=matrix(input$datos,nrow=input$numEstados ,ncol = input$numEstados,dimnames = list(as.list(input$statesNames),as.list(input$statesNames))))
        a<- steadyStates(mc)
        print("SteadyStates Probabilities: ")
        print(a)
    })
    transp <- eventReactive(input$btnEnviar,{ statesNames= c(input$fnam,input$snam,input$tnam)
        mc<-new("markovchain",transitionMatrix=matrix(input$datos,nrow=input$numEstados ,ncol = input$numEstados,dimnames = list(as.list(input$statesNames),as.list(input$statesNames))))
        a<- transitionProbability(mc,t0=input$primero,t1=input$segundo)
        print("Transition Probability: ")
        print(a)
    })
    
    
    
    output$mat <- renderPrint({pruebaMat()})
    output$st <- renderPrint({pasos()})
    output$steady <- renderPrint({steadys()})
    output$chainplot <- renderPlot({draw()}) 
    output$tran <- renderText({transp()})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
