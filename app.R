library(shiny)
library(dplyr)

dataSet <- read.csv("taco2.csv", stringsAsFactors = FALSE)
dataSet <- dataSet[1:597,1:20]
dataSet[dataSet == "*"] <- 0

dataSet$Calorias <- as.numeric(as.character(dataSet$Energia))
dataSet$Proteína <- as.numeric(as.character(dataSet$Proteína))
dataSet$Lipídeos <- as.numeric(as.character(dataSet$Lipídeos))
dataSet$Carboidrato <- as.numeric(as.character(dataSet$Carboidrato))
dataSet$Colesterol <- as.numeric(as.character(dataSet$Colesterol))
dataSet$Fibra.Alimentar <- as.numeric(as.character(dataSet$Fibra.Alimentar))
dataSet$Cálcio <- as.numeric(as.character(dataSet$Cálcio))
dataSet$Magnésio <- as.numeric(as.character(dataSet$Magnésio))
dataSet$Manganês <- as.numeric(as.character(dataSet$Manganês))
dataSet$Fósforo <- as.numeric(as.character(dataSet$Fósforo))
dataSet$Ferro <- as.numeric(as.character(dataSet$Ferro))
dataSet$Sódio <- as.numeric(as.character(dataSet$Sódio))
dataSet$Potássio <- as.numeric(as.character(dataSet$Potássio))
dataSet$Cobre <- as.numeric(as.character(dataSet$Cobre))
dataSet$Zinco <- as.numeric(as.character(dataSet$Zinco))
dataSet$Alimento <- factor(dataSet$Alimento)
dataSet$Grupo <- factor(dataSet$Grupo)

dataSet$Umidade <- NULL
dataSet$Energia <- NULL
dataSet$Cinzas <- NULL

dataSet <- dataSet[, c(-1)]
dataSet <- dataSet[, c(1, 17, 4, 2:3, 5:16)]

ui <- fluidPage(
  headerPanel('Food Item Substitution Helper (FISH)'),
  sidebarPanel(
    selectInput('showGroup', 'Grupo:', levels(dataSet$Grupo)),
    uiOutput('baseFood'),
    sliderInput('calDev', 'Desvio Padrão Calorias:', min = 0, max = max(dataSet$Calorias, na.rm = T), 10),
    sliderInput('choDev', 'Desvio Padrão Carboidrato:', min = 0, max = max(dataSet$Carboidrato, na.rm = T), 5),
    sliderInput('ptnDev', 'Desvio Padrão Proteína:', min = 0, max = max(dataSet$Proteína, na.rm = T), 5),
    sliderInput('lipDev', 'Desvio Padrão Lipídeos:', min = 0, max = max(dataSet$Lipídeos, na.rm = T), 5),
    checkboxGroupInput('showCols', 'Ver:', choices = names(dataSet)[c(2:16)],
                       selected = c('Calorias', 'Carboidrato', 'Proteína', 'Lipídeos'))
    ),
  mainPanel(
    dataTableOutput("mytable")
    )
)

server <- function(input, output){
  
  output$baseFood <- renderUI({
    selectInput('baseFood','Alimento base:', dataSet[dataSet$Grupo == input$showGroup, ,]$Alimento)
  })

  filtered <- reactive({
    if (is.null(input$showGroup)) {
      return(NULL)
    } else if(is.null(input$baseFood)){
      return(NULL)
    } else if(is.null(input$showCols)){
      return(NULL)
    }
    
    dataSet %>%
      select(Alimento, input$showCols, Grupo) %>%
      filter(Grupo == input$showGroup,
             Calorias >= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Calorias-input$calDev,
             Calorias <= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Calorias+input$calDev,
             Carboidrato >= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Carboidrato-input$choDev,
             Carboidrato <= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Carboidrato+input$choDev,
             Proteína >= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Proteína-input$ptnDev,
             Proteína <= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Proteína+input$ptnDev,
             Lipídeos >= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Lipídeos-input$lipDev,
             Lipídeos <= dataSet[dataSet$Alimento == input$baseFood, , drop = F]$Lipídeos+input$lipDev
      )
  })

  output$mytable <- renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)