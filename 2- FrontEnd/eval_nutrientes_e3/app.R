library(shiny)
library(jsonlite)
library(dplyr)

data <- fromJSON("nutrientes.json")
source("funcionesPropias.R")


# Define UI for slider demo app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Aporte Nutricional (valor medio en 100g) "),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Select food type ----
      selectInput("foodType", "Tipo de alimento:",
                  choices = data$alimentos$nombre,
                  selected = data$alimentos$nombre[1]),

      # Input: Carbohidratos ----
      sliderInput("carbohidratos", "Carbohidratos (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Input: Proteinas ----
      sliderInput("proteinas", "Proteínas (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Input: Grasas ----
      sliderInput("grasas", "Grasas (g):",
                  min = 0, max = 500,
                  value = 0, step = 0.1),

      # Input: Calorías ----
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

# Define server logic for slider examples ----
server <- function(input, output, session) {

  # Reactive expression to create data frame of all input values ----
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
  
  # Reemplaza la función evaluacion y colorDelBorde ya que ahora usarás encontrar_cercano_cluster
  observar_resultado <- function(alimento, carbo, prot, gras, cal) {
    # Llamar a la función personalizada
    result <- encontrar_cercano_cluster(alimento, carbo, prot, gras, cal)
    return(result)
  }

  # Introduce una variable reactiva para almacenar el resultado y otra para controlar su visibilidad
  resultadoReactive <- reactiveVal(0)
  resultadoVisible <- reactiveVal(FALSE)
    
  # Oculta el resultado cuando cualquier deslizante o el cuadro de lista desplegable sea ajustado
  observe({
    alimentoSeleccionado <- data$alimentos[data$alimentos$nombre == input$foodType, ]
    
    updateSliderInput(session, "carbohidratos", min = alimentoSeleccionado$nutrientes$Carbohidratos$min, max = alimentoSeleccionado$nutrientes$Carbohidratos$max, value=0)
    updateSliderInput(session, "proteinas", min = alimentoSeleccionado$nutrientes$Proteínas$min, max = alimentoSeleccionado$nutrientes$Proteínas$max, value=0)
    updateSliderInput(session, "grasas", min = alimentoSeleccionado$nutrientes$Grasas$min, max = alimentoSeleccionado$nutrientes$Grasas$max, value=0)
    updateSliderInput(session, "calorias", min = alimentoSeleccionado$nutrientes$Calorías$min, max = alimentoSeleccionado$nutrientes$Calorías$max, value=0)
  })
  
  # Este observe resetea la visibilidad del resultado cuando se cambie el alimento o algún nutriente.
  observe({
    # Estos inputs detectan cambios en los deslizantes o el selector de alimento
    input$foodType
    input$carbohidratos
    input$proteinas
    input$grasas
    input$calorias
    
    # Resetear la visibilidad del resultado
    resultadoVisible(FALSE)
  })
  
  # Actualiza la variable reactiva con el nuevo cálculo y muestra el resultado solo cuando se presiona el botón "Evaluar"
  observeEvent(input$evaluar_btn, {
    resultado <- observar_resultado(input$foodType, input$carbohidratos, input$proteinas, input$grasas, input$calorias)
    resultadoReactive(resultado)
    resultadoVisible(TRUE)
  })
    
  output$resultado_evaluacion <- renderUI({
    if (!resultadoVisible()) return(NULL)  # Si el resultado no debe mostrarse, termina aquí
    
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
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })

}

# Create Shiny app ----
shinyApp(ui, server)
