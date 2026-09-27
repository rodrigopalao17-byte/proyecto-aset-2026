library(shiny)
library(tidyverse)

# ------------------------------------------------------------
# DATOS
# ------------------------------------------------------------

base <- read_csv(
  "../resultados/base_dashboard.csv",
  show_col_types = FALSE
)


# ------------------------------------------------------------
# INTERFAZ
# ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .titulo-dashboard {
        margin-bottom: 5px;
      }

      .subtitulo-dashboard {
        color: #666666;
        margin-bottom: 25px;
      }

      .tarjeta {
        background-color: #eaf6fb;
        border-left: 5px solid #4aa3c7;
        padding: 18px;
        margin-bottom: 25px;
        border-radius: 5px;
      }

      .numero-principal {
      font-size: 36px;
      font-weight: bold;
      color: #287fa3;
      margin-bottom: 0px;
      }

      .texto-principal {
        font-size: 16px;
        color: #555555;
      }

      .nota {
        color: #666666;
        font-size: 13px;
        margin-top: 20px;
      }
    "))
  ),
  
  div(
    class = "titulo-dashboard",
    h2("Precariedad salarial entre trabajadores dependientes en el Perú")
  ),
  
  div(
    class = "subtitulo-dashboard",
    h4("Encuesta Nacional de Hogares (ENAHO), 2025")
  ),
  
  fluidRow(
    
    column(
      width = 3,
      
      wellPanel(
        
        h4("Opciones de análisis"),
        
        selectInput(
          inputId = "variable",
          label = "Analizar según:",
          choices = c(
            "Sexo" = "sexo",
            "Grupo de edad" = "grupo_edad",
            "Categoría ocupacional" = "categoria_ocupacional",
            "Tipo de contrato" = "tipo_contrato"
          ),
          selected = "sexo"
        ),
        
        hr(),
        
        strong("Indicador"),
        
        p(
          "Ingreso mensual inferior a la Remuneración Mínima Vital de S/ 1 130."
        )
        
      )
    ),
    
    
    column(
      width = 9,
      
      div(
        class = "tarjeta",
        
        div(
          class = "numero-principal",
          textOutput("porcentaje_general")
        ),
        
        div(
          class = "texto-principal",
          "de los trabajadores dependientes analizados presenta ingresos inferiores al SMV"
        )
      ),
      
      h3("Incidencia según características seleccionadas"),
      
      plotOutput(
        "grafico",
        height = "400px"
      ),
      
      h4("Resultados"),
      
      tableOutput("tabla"),
      
      div(
        class = "nota",
        p(
          "Nota: las estimaciones se encuentran ponderadas mediante el factor de expansión FAC500A de la ENAHO 2025. La población analizada comprende empleados, obreros y trabajadores del hogar."
        )
      )
    )
  )
)


# ------------------------------------------------------------
# SERVIDOR
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Incidencia general ponderada
  incidencia_general <- base %>%
    filter(!is.na(bajo_smv)) %>%
    summarise(
      porcentaje = weighted.mean(
        bajo_smv,
        FAC500A,
        na.rm = TRUE
      ) * 100
    ) %>%
    pull(porcentaje)
  
  
  # Tarjeta principal
  output$porcentaje_general <- renderText({
    
    paste0(
      round(incidencia_general, 1),
      " %"
    )
    
  })
  
  
  # Datos dinámicos según la variable seleccionada
  datos_grafico <- reactive({
    
    base %>%
      filter(
        !is.na(bajo_smv),
        !is.na(.data[[input$variable]])
      ) %>%
      group_by(
        categoria = .data[[input$variable]]
      ) %>%
      summarise(
        porcentaje = weighted.mean(
          bajo_smv,
          FAC500A,
          na.rm = TRUE
        ) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(porcentaje))
    
  })
  
  
  # Gráfico dinámico
  output$grafico <- renderPlot({
    
    ggplot(
      datos_grafico(),
      aes(
        x = reorder(categoria, porcentaje),
        y = porcentaje
      )
    ) +
      geom_col(
        width = 0.7,
        fill = "#4aa3c7"
      ) +
      geom_text(
        aes(
          label = paste0(
            round(porcentaje, 1),
            " %"
          )
        ),
        hjust = -0.15,
        size = 4
      ) +
      coord_flip() +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(
        x = NULL,
        y = "Porcentaje con ingreso inferior al SMV"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    
  })
  
  
  # Tabla dinámica
  output$tabla <- renderTable({
    
    datos_grafico() %>%
      mutate(
        porcentaje = round(porcentaje, 1)
      ) %>%
      rename(
        Categoría = categoria,
        `Ingreso inferior al SMV (%)` = porcentaje
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "s"
  )
  
}


# ------------------------------------------------------------
# EJECUCIÓN
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)