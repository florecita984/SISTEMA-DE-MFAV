library(shiny) #este no se 
library(DT)
library(ggplot2)
#interfaz----
ui <- fluidPage(
  titlePanel("Sistema de Matemáticas Financieras"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tema", "Selecciona un tema:",
                  choices = c("Tablas de Amortización / Ahorro", "Depreciación", "TIR", "Bonos (opcional)")),
      conditionalPanel(
        condition = "input.tema == 'Tablas de Amortización / Ahorro'",
        selectInput("tipo_operacion", "Tipo de operación:", choices = c("Deuda", "Ahorro")),
        conditionalPanel(
          condition = "input.tipo_operacion == 'Deuda'",
          checkboxInput("tasas_variables", "¿Usar tasas variables por periodo?", value = FALSE),
          
          conditionalPanel(
            condition = "input.tasas_variables == true",
            radioButtons("tipo_tasas_variables", "Tipo de tasas variables:",
                        choices = c("Nominal convertible", "Efectiva"),
                        selected = "Nominal convertible"),
            textInput("tasas_lista", "Tasas por periodo (%) separadas por comas:", "12,11,10,9"),
            helpText("Ingresa las tasas anuales. Por ejemplo: 12,11,10,9 para tasas decrecientes"),
            
            conditionalPanel(
              condition = "input.tipo_tasas_variables == 'Nominal convertible'",
              selectInput("frecuencia_conversion_var", "Frecuencia de conversión de las tasas nominales:",
                        choices = c("Anual" = 1, "Semestral" = 2, "Trimestral" = 4, "Bimestral"= 6, "Mensual" = 12), 
                        selected = 12)
            ),
            
            conditionalPanel(
              condition = "input.tipo_tasas_variables == 'Efectiva'",
              selectInput("unidad_tasa_var",
                        "Unidad de periodo de las tasas efectivas:",
                        choices = c("Anual", "Mensual", "Quincenal", "Diaria"))
              ),
            
            numericInput("num_periodos_tasas", "Número de periodos con tasas variables:", value = 4, min = 1)
          ),
          
          conditionalPanel(
            condition = "!input.tasas_variables",
            radioButtons("tipo_tasa", "Tipo de tasa:",
                        choices = c("Nominal convertible", "Efectiva"),
                        selected = "Nominal convertible"),
            
            conditionalPanel(
              condition = "input.tipo_tasa == 'Nominal convertible'",
              textInput("tasa", "Tasa nominal anual (%) (puedes poner 'Calculalo'):", value = "10"),
              selectInput("frecuencia_conversion", "Frecuencia de conversión de la tasa nominal:",
                        choices = c("Anual" = 1, "Semestral" = 2, "Trimestral" = 4, "Bimestral"= 6, "Mensual" = 12), 
                        selected = 2)
            ),
            
            conditionalPanel(
              condition = "input.tipo_tasa == 'Efectiva'",
              textInput("tasa", "Tasa efectiva por periodo (%):", value = "10"),
              selectInput("unidad_tasa",
                        "Unidad de periodo de la tasa efectiva:",
                        choices = c("Anual", "Mensual", "Quincenal", "Diaria"))
            )
          ),
          radioButtons("Tipo_anualidad", "Tipo de Anualidad:",choices = c("Vencida", "Anticipada", "Diferida")),
          conditionalPanel(
            condition="input.Tipo_anualidad ==  'Diferida'",
            numericInput("diferido", "periodos de diferimientos:", value=1, min=1)
          ),
          textInput("capital", "Monto presente o futuro (puede ser 'Calculalo'):", value = "10000"),
          textInput("periodos", "Número de periodos (puede ser 'Calculalo'):", value = "5"),
          conditionalPanel(
            condition = "!input.pagos_personalizados",
            selectInput("unidad_periodo", "Unidad de los periodos:",
                        choices = c("Años","Semestres","Bimestrales", "Meses", "Quincenas", "Días"))
          ),
          conditionalPanel(
            condition = "!input.pagos_personalizados",
            textInput("pago", "Pago por periodo (puede ser 'Calculalo'):", value = "Calculalo")
          ),
          checkboxInput("pagos_personalizados", "¿Usar pagos personalizados?", value = FALSE),
          
          conditionalPanel(
            condition = "input.pagos_personalizados == true",
            textInput("pagos_lista", "Pagos personalizados (separa por comas):", "30000,30000,30000"),
            numericInput("valor_final", "Pago final para saldar la deuda:", value = 10000),
            numericInput("periodos_totales", "Total de periodos:", value = 4)
          ),
          checkboxInput("ver_saldo_especifico", "¿Ver saldo insoluto en un periodo específico?", value = FALSE),
          
          conditionalPanel(
            condition = "input.ver_saldo_especifico",
            numericInput("periodo_consulta", "Periodo a consultar (por ejemplo, 120):", value = 120, min = 1)
          ),
          #fincaliz
          checkboxInput("ver_proporcion_amortizada", "¿Ver proporción amortizada en un periodo específico?", value = FALSE),
          
          conditionalPanel(
            condition = "input.ver_proporcion_amortizada",
            numericInput("periodo_amortizado", "Periodo a consultar (por ejemplo, 12):", value = 12, min = 1)
          ),
          
          
          
          conditionalPanel(
            condition = "input.tema == 'TIR'",
            textInput("flujos", "Flujos de efectivo separados por coma (ej. -1000,200,300,400):", value = "-1000,300,400,500")
          ),
          conditionalPanel(
            condition = "input.tema == 'Bonos (opcional)'",
            numericInput("valor_nominal", "Valor nominal del bono:", value = 1000),
            numericInput("cupon", "Tasa de cupón (%):", value = 5),
            numericInput("rendimiento", "Rendimiento exigido (%):", value = 4),
            numericInput("años_bono", "Años al vencimiento:", value = 5)
          )
        ),   
        #caliz
        conditionalPanel(
          condition = "input.tipo_operacion == 'Ahorro'",
          textInput("tipo_anualidad", "Tipo de anualidad:", value = "Vencida")
        ),
      ),
    ),
    mainPanel(
      h3("Resultado"),
      DTOutput("tabla"),
      plotOutput("grafica"),
      verbatimTextOutput("resultado")
    )
  )
)
#la lógica del servidor
server <- function(input, output) {
  datos <- reactive({
    if (input$tema == "Tablas de Amortización / Ahorro") {
      unidad_tasa_map <- c(Anual = 1, Bimestral= 6, Mensual = 12, Quincenal = 24, Diaria = 360)
      unidad_periodo_map <- c(Años = 1, Semestres = 2, Bimestrales = 6, Meses = 12, Quincenas = 24, Días = 360)
      # Leer inputs
      capital <- suppressWarnings(ifelse(tolower(input$capital) %in% c("calculalo", ""), NA, as.numeric(input$capital)))
      tasa    <- suppressWarnings(ifelse(tolower(input$tasa)    %in% c("calculalo", ""), NA, as.numeric(input$tasa))) / 100
      n       <- suppressWarnings(ifelse(tolower(input$periodos)%in% c("calculalo", ""), NA, as.numeric(input$periodos)))
      pago    <- suppressWarnings(ifelse(tolower(input$pago)    %in% c("calculalo", ""), NA, as.numeric(input$pago)))
      
      # Inicializa
      tasa_periodo <- NA
      n_periodos   <- if (!is.na(n)) n else NA
      
      # Solo calculas si tasa fue proporcionada
      if (!is.na(tasa)) {
        if (input$tipo_tasa == "Nominal convertible") {
          frecuencia   <- as.numeric(input$frecuencia_conversion)
          tasa_periodo <- tasa / frecuencia
        } else if (input$tipo_tasa == "Efectiva") {
          if (tasa < 1.5) {
            tasa_periodo <- tasa
          } else {
            tasa_periodo <- (1 + tasa)^(1 / unidad_tasa_map[input$unidad_tasa]) - 1
          }
        }
      }
      
      n_periodos <- if (!is.na(n)) n else NA
      #SOLO PUEDE HABER UN DATO FALTANTE
      datos_faltantes <- sum(is.na(c(capital, tasa_periodo, n_periodos, pago)))
      if (datos_faltantes != 1) {
        return(data.frame(Error = "Debes dejar solo un campo como 'Calculalo'"))
      }
      tipo <- input$tipo_operacion
      tabla <- NULL
      ## 🔹 BLOQUE PARA PAGOS PERSONALIZADOS
      if (tipo == "Deuda" && isTRUE(input$pagos_personalizados)) {
        # Leer lista de pagos ingresados manualmente
        pagos_vec <- as.numeric(unlist(strsplit(input$pagos_lista, ",")))
        
        # Procesar tasas variables si están activadas
        tasas_variables <- if(isTRUE(input$tasas_variables)) {
          tasas_base <- as.numeric(unlist(strsplit(input$tasas_lista, ","))) / 100
          
          if(input$tipo_tasas_variables == "Nominal convertible") {
            # Convertir tasas nominales a tasas por periodo
            tasas_base / as.numeric(input$frecuencia_conversion_var)
          } else {
            # Convertir tasas efectivas al periodo correspondiente
            (1 + tasas_base)^(1 / unidad_tasa_map[input$unidad_tasa_var]) - 1
          }
        } else {
          rep(tasa_periodo, input$periodos_totales)
        }
        
        # Inicializar tabla
        n_personalizado <- input$periodos_totales
        tabla <- data.frame(Periodo = 0:n_personalizado, 
                          Pago = NA, 
                          Interés = NA, 
                          Amortización = NA, 
                          Saldo = NA,
                          "Tasa %" = NA)
        
        # Establecer valores iniciales
        saldo <- capital
        tabla[1, ] <- c(0, 0, 0, 0, round(saldo, 2), round(tasas_variables[1] * 100, 2))
        
        # Procesar pagos conocidos
        for (t in 1:(length(pagos_vec))) {
          pago_t <- pagos_vec[t]
          tasa_actual <- tasas_variables[t]
          interes <- saldo * tasa_actual
          amort <- pago_t - interes
          saldo <- saldo - amort
          
          tabla[t + 1, ] <- c(t, 
                             round(pago_t, 2), 
                             round(interes, 2), 
                             round(amort, 2), 
                             round(saldo, 2),
                             round(tasa_actual * 100, 2))
        }
        
        # Si queda un período final, calcular el último pago
        if (length(pagos_vec) < n_personalizado) {
          t <- length(pagos_vec) + 1
          tasa_actual <- tasas_variables[t]
          interes <- saldo * tasa_actual
          pago_final <- saldo + interes
          amort <- saldo
          saldo <- 0
          
          tabla[t + 1, ] <- c(t, 
                             round(pago_final, 2), 
                             round(interes, 2), 
                             round(amort, 2), 
                             round(saldo, 2),
                             round(tasa_actual * 100, 2))
        }
        
        attr(tabla, "resultado") <- list(
          capital = capital,
          tasa = tasa_periodo * 100,
          n = n_personalizado,
          pago = paste(c(pagos_vec, if(length(pagos_vec) < n_personalizado) round(pago_final,2)), collapse = ", ")
        )
        
        return(tabla)
      }
      
      # 🔹 BLOQUE NORMAL PARA DEUDA (anualidades iguales)
      # BLOQUE NORMAL DEUDA
      if (tipo == "Deuda" && !isTRUE(input$pagos_personalizados)) {
        k <- if (input$Tipo_anualidad == "Diferida") as.numeric(input$diferido) else 0
        ultimo_periodo_incompleto <- FALSE
        
        # Cálculo de variable faltante
        if (is.na(pago)) {
          if (input$Tipo_anualidad == "Vencida") {
            pago <- capital * (tasa_periodo * (1 + tasa_periodo)^n_periodos) / ((1 + tasa_periodo)^n_periodos - 1)
          } else if (input$Tipo_anualidad == "Anticipada") {
            pago <- capital * (tasa_periodo * (1 + tasa_periodo)^n_periodos) / ((1 + tasa_periodo)^n_periodos - 1) / (1 + tasa_periodo)
          } else {
            pago <- capital * (tasa_periodo * (1 + tasa_periodo)^n_periodos) / ((1 + tasa_periodo)^n_periodos - 1) * (1 + tasa_periodo)^k
          }
        } else if (is.na(capital)) {
          if (input$Tipo_anualidad == "Vencida") {
            capital <- pago * ((1 + tasa_periodo)^n_periodos - 1) / (tasa_periodo * (1 + tasa_periodo)^n_periodos)
          } else if (input$Tipo_anualidad == "Anticipada") {
            capital <- pago * ((1 + tasa_periodo)^n_periodos - 1) / (tasa_periodo * (1 + tasa_periodo)^n_periodos) * (1 + tasa_periodo)
          } else {
            capital <- pago * ((1 + tasa_periodo)^n_periodos - 1) / (tasa_periodo * (1 + tasa_periodo)^n_periodos) / (1 + tasa_periodo)^k
          }
        } else if (is.na(tasa_periodo)) {
          if (input$Tipo_anualidad == "Vencida") {
            f <- function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) - pago
          } else if (input$Tipo_anualidad == "Anticipada") {
            f <- function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) / (1 + i) - pago
          } else {
            f <- function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) * (1 + i)^k - pago
          }
          tasa_periodo <- uniroot(f, c(0.000001, 1))$root
        } else if (is.na(n_periodos)) {
          if (input$Tipo_anualidad == "Vencida") {
            f <- function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) - pago
          } else if (input$Tipo_anualidad == "Anticipada") {
            f <- function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) / (1 + tasa_periodo) - pago
          } else {
            f <- function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) * (1 + tasa_periodo)^k - pago
          }
          n_decimal <- uniroot(f, c(1, 600))$root
          n_periodos <- floor(n_decimal)
          ultimo_periodo_incompleto <- TRUE
        }
        
        # Generar tabla
        saldo <- capital
        total_periodos <- n_periodos + k  # Añadimos los periodos diferidos
        tabla <- data.frame(Periodo = 0:total_periodos, Pago = NA, Interés = NA, Amortización = NA, Saldo = NA)
        tabla[1, ] <- c(0, 0, 0, 0, round(saldo, 2))
        
        # Manejar periodos diferidos
        for (t in 1:total_periodos) {
          if (t <= k) {
            # Durante el periodo de diferimiento, solo se acumula interés
            interes <- saldo * tasa_periodo
            saldo <- saldo + interes
            tabla[t + 1, ] <- c(t, 0, round(interes, 2), 0, round(saldo, 2))
          } else {
            # Después del diferimiento, pagos normales
            interes <- saldo * tasa_periodo
            amort <- pago - interes
            saldo <- saldo - amort
            tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
          }
        }
        
        if (isTRUE(ultimo_periodo_incompleto) && saldo > 1e-2) {
          interes <- saldo * tasa_periodo
          pago_extra <- saldo + interes
          saldo <- 0
          tabla[nrow(tabla) + 1, ] <- c(n_periodos + k + 1, round(pago_extra, 2), round(interes, 2), round(pago_extra - interes, 2), 0)
        }
        
        attr(tabla, "resultado") <- list(
          capital = capital,
          tasa = tasa_periodo * 100,
          n = if (exists("n_decimal")) n_decimal else n_periodos,
          pago = pago
        )
        
        return(tabla)
      }
      
      }
      
      else if (tipo == "Ahorro") {
        tipo_anualidad <- input$Tipo_anualidad
        k <- if (tipo_anualidad == "Diferida") as.numeric(input$diferido) else 0
        ajuste <- if (tipo_anualidad == "Anticipada") 1 + tasa_periodo else 1
        
        ultimo_periodo_incompleto <- FALSE
        
        if (is.na(pago)) {
          pago <- (capital * tasa_periodo) / (((1 + tasa_periodo)^(n_periodos + k) - 1) / tasa_periodo * ajuste)
        } else if (is.na(capital)) {
          capital <- pago * ((1 + tasa_periodo)^(n_periodos + k) - 1) / tasa_periodo * ajuste
        } else if (is.na(tasa_periodo)) {
          f <- function(i) pago * ((1 + i)^(n_periodos + k) - 1) / i * ajuste - capital
          tasa_periodo <- uniroot(f, c(0.000001, 1))$root
        } else if (is.na(n)) {
          f <- function(n) pago * ((1 + tasa_periodo)^(n + k) - 1) / tasa_periodo * ajuste - capital
          n_decimal <- uniroot(f, c(1, 600))$root
          n_periodos <- floor(n_decimal)
          ultimo_periodo_incompleto <- TRUE
        }
        
        saldo <- 0
        total_periodos <- n_periodos + k  # Añadimos los periodos diferidos
        tabla <- data.frame(Periodo = 0:total_periodos, Pago = NA, Interés = NA, Aporte = NA, Saldo = NA)
        tabla[1, ] <- c(0, 0, 0, 0, round(saldo, 2))
        
        # Manejar periodos diferidos
        for (t in 1:total_periodos) {
          if (t <= k) {
            # Durante el periodo de diferimiento, solo se acumula interés
            interes <- saldo * tasa_periodo
            saldo <- saldo + interes
            tabla[t + 1, ] <- c(t, 0, round(interes, 2), 0, round(saldo, 2))
          } else {
            # Después del diferimiento, pagos normales
            interes <- saldo * tasa_periodo
            amort <- pago - interes
            saldo <- saldo - amort
            tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
          }
        }
        
        if (isTRUE(ultimo_periodo_incompleto)) {
          interes <- saldo * tasa_periodo
          ultimo_pago <- saldo + interes
          saldo <- 0
          tabla[nrow(tabla) + 1, ] <- c(n_periodos + k + 1, round(ultimo_pago, 2), round(interes, 2), round(ultimo_pago, 2), round(saldo, 2))
        }
        
        attr(tabla, "resultado") <- list(
          capital = capital,
          tasa = tasa_periodo * unidad_tasa_map[input$unidad_tasa] * 100,
          n = if (exists("n_decimal")) n_decimal / unidad_periodo_map[input$unidad_periodo] else n_periodos / unidad_periodo_map[input$unidad_periodo],
          pago = pago
        )
        
        tabla <- na.omit(tabla)
        return(tabla)
      }
      
      
     else if (input$tema == "TIR") {
      flujos <- as.numeric(unlist(strsplit(input$flujos, ",")))
      tir <- uniroot(function(r) sum(flujos / (1 + r)^(0:(length(flujos) - 1))), c(-0.99, 1))$root
      return(data.frame("TIR estimada (%)" = round(tir * 100, 2)))
    } else if (input$tema == "Bonos (opcional)") {
      Vn <- input$valor_nominal
      c <- input$cupon / 100
      r <- input$rendimiento / 100
      n <- input$años_bono
      precio <- sum((Vn * c) / (1 + r)^(1:n)) + Vn / (1 + r)^n
      return(data.frame("Precio del bono" = round(precio, 2)))
    }
    return(NULL)
  })
  
  
  output$tabla <- renderDT({
    req(input$tema != "")
    datos()
  })
  output$grafica <- renderPlot({
    tabla <- datos()
    if (is.null(tabla) || "Error" %in% colnames(tabla) || !"Saldo" %in% colnames(tabla)) return(NULL)
    ggplot(tabla, aes(x = Periodo, y = Saldo)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point() +
      labs(title = "Evolución del Saldo", y = "Saldo ($)", x = "Periodo") +
      theme_minimal()
  })
  output$resultado <- renderPrint({
    if (input$tema == "TIR") {
      flujos <- as.numeric(unlist(strsplit(input$flujos, ",")))
      tir <- uniroot(function(r) sum(flujos / (1 + r)^(0:(length(flujos) - 1))), c(-0.99, 1))$root
      cat("La TIR es aproximadamente:", round(tir * 100, 2), "%")
    } else if (input$tema == "Tablas de Amortización / Ahorro") {
      tabla <- datos()
      if (is.null(tabla) || "Error" %in% colnames(tabla)) return(NULL)
      resumen <- attr(tabla, "resultado")
      
      cat("Resumen de la operación:\n")
      
      if (!is.null(resumen$capital) && is.numeric(resumen$capital)) {
        cat("Monto presente/futuro:", round(resumen$capital, 2), "\n")
      }
      
      if (!is.null(resumen$n) && is.numeric(resumen$n)) {
        cat("Número de periodos:", round(resumen$n, 2), "\n")
      }
      
      if (!is.null(resumen$pago)) {
        if (is.numeric(resumen$pago)) {
          cat("Pago periódico:", round(resumen$pago, 2), "\n")
        } else {
          cat("Pagos personalizados:", resumen$pago, "\n")
        }
      }
      
      # 🔹 Mostrar saldo insoluto si el usuario lo solicita
      if (isTRUE(input$ver_saldo_especifico)) {
        periodo <- input$periodo_consulta
        if (periodo <= nrow(tabla) - 1 && "Saldo" %in% colnames(tabla)) {
          saldo_periodo <- tabla[periodo + 1, "Saldo"]
          cat("\nSaldo insoluto al final del periodo", periodo, ":", round(saldo_periodo, 2))
        } else {
          cat("\nPeriodo fuera del rango o sin datos.")
        }
      }
      
      # 🔹 Mostrar proporción amortizada si el usuario lo solicita
      if (isTRUE(input$ver_proporcion_amortizada)) {
        periodo <- input$periodo_amortizado
        if (periodo <= nrow(tabla) - 1 && "Amortización" %in% colnames(tabla)) {
          amort_acum <- sum(tabla[2:(periodo + 1), "Amortización"])
          proporcion <- amort_acum / resumen$capital
          cat("\nProporción del saldo amortizado al periodo", periodo, ":", round(proporcion * 100, 2), "%")
          cat("\nMonto amortizado (derechos adquiridos): $", round(amort_acum, 2))
        } else {
          cat("\nPeriodo fuera del rango o sin datos para calcular la proporción amortizada.")
        }
      }
      
    }
    
  })
  
}
shinyApp(ui, server)