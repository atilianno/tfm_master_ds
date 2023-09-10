library(shiny)
library(jsonlite)
library(dplyr)

data <- fromJSON("nutrientes.json")
source("funcionesPropias.R")

# Vista Web
ui <- fluidPage(

  # Titulo
  titlePanel("Aporte Nutricional (valor medio en 100g) "),

  sidebarLayout(

    # Panel Ingreso de Aporte Nutricional
    sidebarPanel(
      
      # Ingreso Tipo de Comida
      selectInput("foodType", "Tipo de alimento:",
                  choices = data$alimentos$nombre,
                  selected = data$alimentos$nombre[1]),

      # Ingreso Carbohidratos
      sliderInput("carbohidratos", "Carbohidratos (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Ingreso Proteínas
      sliderInput("proteinas", "Proteínas (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Ingreso Grasas
      sliderInput("grasas", "Grasas (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Ingreso Calorías
      sliderInput("calorias", "Calorías (Kcal):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

    ),

    mainPanel(
      column(6,tableOutput("values"),
      actionButton("evaluar_btn", "Evaluar")),
      column(6,uiOutput("resultado_evaluacion"))

    )
  )
)

# Componente Servidor
server <- function(input, output, session) {

  sliderValues <- reactive({
    
    data.frame(
      Nutriente = c("Tipo de alimento", "Carbohidratos (g)",
                    "Proteínas (g)",
                    "Grasas (g)",
                    "Calorías (Kcal)"),
      Valor = as.character(c(input$foodType, input$carbohidratos,
                             input$proteinas,
                             input$grasas,
                             input$calorias)),
      stringsAsFactors = FALSE)
    
  })
  
  observar_resultado <- function(alimento, carbo, prot, gras, cal) {
    result <- encontrar_cercano_cluster(alimento, carbo, prot, gras, cal)
    return(result)
  }

  # Variable reactiva para almacenar resultado
  resultadoReactive <- reactiveVal(0)
  
  # Variable para controlar su visibilidad
  resultadoVisible <- reactiveVal(FALSE)
    
  # Oculta el resultado por modificación de parámetros de consulta
  observe({
    alimentoSeleccionado <- data$alimentos[data$alimentos$nombre == input$foodType, ]
    
    updateSliderInput(session, "carbohidratos", min = alimentoSeleccionado$nutrientes$Carbohidratos$min, max = alimentoSeleccionado$nutrientes$Carbohidratos$max, value=0)
    updateSliderInput(session, "proteinas", min = alimentoSeleccionado$nutrientes$Proteínas$min, max = alimentoSeleccionado$nutrientes$Proteínas$max, value=0)
    updateSliderInput(session, "grasas", min = alimentoSeleccionado$nutrientes$Grasas$min, max = alimentoSeleccionado$nutrientes$Grasas$max, value=0)
    updateSliderInput(session, "calorias", min = alimentoSeleccionado$nutrientes$Calorías$min, max = alimentoSeleccionado$nutrientes$Calorías$max, value=0)
  })
  
  # Reinicia la visibilidad del resultado por modificación de parámetros de consulta
  observe({
    input$foodType
    input$carbohidratos
    input$proteinas
    input$grasas
    input$calorias
    
    resultadoVisible(FALSE)
  })
  
  # Muestra el resultado al presionar "Evaluar"
  observeEvent(input$evaluar_btn, {
    resultado <- observar_resultado(input$foodType, input$carbohidratos, input$proteinas, input$grasas, input$calorias)
    resultadoReactive(resultado)
    resultadoVisible(TRUE)
  })
    
  output$resultado_evaluacion <- renderUI({
    if (!resultadoVisible()) return(NULL)  # Termina si no se muestra resultado
    
    resultado <- resultadoReactive()
    color <- resultado[[1]]
    descripcion <- resultado[[2]]
    HTML(paste0('<div style="border: 5px solid ', color, '; padding: 10px;">',
                '<span style="font-weight:bold; font-size:20px;">',
                "El resultado de la evaluación para la consulta de ", input$foodType, " es: ", descripcion, '</span></div>'))
  })
  
  sliderValues <- reactive({
    
    data.frame(
      Nutriente = c("Tipo de alimento", "Carbohidratos (g)",
                    "Proteínas (g)",
                    "Grasas (g)",
                    "Calorías (Kcal)"),
      Valor = as.character(c(input$foodType, input$carbohidratos,
                             input$proteinas,
                             input$grasas,
                             input$calorias)),
      stringsAsFactors = FALSE)
    
  })
  
  # Despliega valores
  output$values <- renderTable({
    sliderValues()
  })

}

# Crea aplicación Shiny
shinyApp(ui, server)
