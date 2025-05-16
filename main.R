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
                                             radioButtons("Tipo_anualidad", "Tipo de Anualidad:",choices = c("Vencida", "Anticipada")),
        radioButtons("tipo_tasa", "Tipo de tasa:",
                     choices = c("Nominal convertible", "Efectiva"),
                     selected = "Nominal convertible"),
        conditionalPanel(
          condition = "input.tipo_tasa == 'Nominal convertible'",
          selectInput("frecuencia_conversion", "Frecuencia de conversión de la tasa nominal:",
                      choices = c("Anual" = 1, "Semestral" = 2, "Trimestral" = 4, "Bimestral"= 6, "Mensual" = 12), selected = 2)
        ),
        textInput("capital", "Monto presente o futuro (puede ser 'Calculalo'):", value = "10000"),
        textInput("tasa", "Tasa de interés (puede ser 'Calculalo'):", value = "10"),
        selectInput("unidad_tasa", "Unidad de la tasa de interés:",
                    choices = c("Anual", "Mensual", "Quincenal", "Diaria")),
        uiOutput("tasa_ui"),
        conditionalPanel(
          condition = "input.tipo_tasa == 'Efectiva'",
          selectInput("unidad_tasa", "Unidad del periodo de capitalización (para convertir la tasa efectiva anual):",
                      choices = c("Anual", "Mensual", "Quincenal", "Diaria"))
        ),
        textInput("periodos", "Número de periodos (puede ser 'Calculalo'):", value = "5"),
        selectInput("unidad_periodo", "Unidad de los periodos:",
                    choices = c("Años", "Meses", "Quincenas", "Días")),
                    choices = c("Años","Semestres","Bimestrales", "Meses", "Quincenas", "Días")),
        textInput("pago", "Pago por periodo (puede ser 'Calculalo'):", value = "Calculalo")
      ),   
          condition = "!input.pagos_personalizados",
          textInput("pago", "Pago por periodo (puede ser 'Calculalo'):", value = "Calculalo")
        )
        ),   
      checkboxInput("pagos_personalizados", "¿Usar pagos personalizados?", value = FALSE),
      
      
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
  output$tasa_ui <- renderUI({
    tipo <- input$tipo_tasa
    label <- if (tipo == "Nominal convertible") {
      "Tasa nominal anual (%) (puedes poner 'Calculalo'):"
    } else {
      "Tasa efectiva anual (%) (puedes poner 'Calculalo'):"
    }
    textInput("tasa", label, value = "10")
  })
  datos <- reactive({
    if (input$tema == "Tablas de Amortización / Ahorro") {
      unidad_tasa_map <- c(Anual = 1, Mensual = 12, Quincenal = 24, Diaria = 360)
      unidad_periodo_map <- c(Años = 1, Meses = 12, Quincenas = 24, Días = 360)
      # Obtener los valores de entrada o asignar NA si se usa "Calculalo"
      capital <- suppressWarnings(ifelse(tolower(input$capital) %in% c("calculalo", ""), NA, as.numeric(input$capital)))
      tasa <- suppressWarnings(ifelse(tolower(input$tasa) %in% c("calculalo", ""), NA, as.numeric(input$tasa))) / 100
      n <- suppressWarnings(ifelse(tolower(input$periodos) %in% c("calculalo", ""), NA, as.numeric(input$periodos)))
      pago <- suppressWarnings(ifelse(tolower(input$pago) %in% c("calculalo", ""), NA, as.numeric(input$pago)))
      if (!is.na(tasa)) {
        if (input$tipo_tasa == "Nominal convertible") {
          tasa_periodo <- tasa / unidad_tasa_map[input$unidad_tasa]
        } else if (input$tipo_tasa == "Efectiva") {
          tasa_periodo <- (1 + tasa)^(1 / unidad_tasa_map[input$unidad_tasa]) - 1
          # ✅ Solución inteligente: si tasa < 1.5, asumimos que ya es por periodo (como 0.4%)
          if (tasa < 1.5) {
            tasa_periodo <- tasa  # Ya es la tasa por periodo (ej. 0.004)
          } else {
            # Si se da como anual (ej. 12%), la convertimos al periodo seleccionado
            tasa_periodo <- (1 + tasa)^(1 / unidad_tasa_map[input$unidad_tasa]) - 1
          }
        }
      } else {
        tasa_periodo <- NA
      }
       n_periodos <- if (!is.na(n)) n else NA
      #SOLO PUEDE HABER UN DATO FALTANTE
      datos_faltantes <- sum(is.na(c(capital, tasa_periodo, n_periodos, pago)))
      if (datos_faltantes != 1) {
        return(data.frame(Error = "Debes dejar solo un campo como 'Calculalo'"))
      }
      tipo <- input$tipo_operacion
      tabla <- NULL
      if (tipo == "Deuda") {
        if (is.na(pago)) {
          pago <- capital * (tasa_periodo * (1 + tasa_periodo)^n_periodos) / ((1 + tasa_periodo)^n_periodos - 1)
        } else if (is.na(capital)) {
          capital <- pago * ((1 + tasa_periodo)^n_periodos - 1) / (tasa_periodo * (1 + tasa_periodo)^n_periodos)
        } else if (is.na(tasa_periodo)) {
          f <- function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) - pago
          tasa_periodo <- uniroot(f, c(0.000001, 1))$root
        } else if (is.na(n_periodos)) {
          f <- function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) - pago
          n_periodos <- round(uniroot(f, c(1, 600))$root)
        # Calcular pago final faltante si se pone 0 o "auto"
        if (tolower(input$valor_final) %in% c("auto", "calculalo", "0")) {
          saldo_temp <- capital
          for (pago in pagos_vec) {
            interes <- saldo_temp * tasa_periodo
            amort <- pago - interes
            saldo_temp <- saldo_temp - amort
          }
          pago_final <- saldo_temp * (1 + tasa_periodo)  # asumiendo que se paga con interés al siguiente periodo
        } else {
          pago_final <- as.numeric(input$valor_final)
        }
        
        
        saldo <- capital
        tabla <- data.frame(Periodo = 0:n_periodos, Pago = NA, Interés = NA, Amortización = NA, Saldo = NA)
        tabla[1, ] <- c(0, 0, 0, 0, round(saldo, 2))
        # Calcular los valores para cada periodo
        for (t in 1:n_periodos) {
          interes <- saldo * tasa_periodo
          amort <- pago - interes
          saldo <- saldo - amort
          tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
        }
      } else if (tipo == "Ahorro") {
        if (is.na(pago)) {
          pago <- capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1)
        } else if (is.na(capital)) {
          capital <- pago * ((1 + tasa_periodo)^n_periodos - 1) / tasa_periodo
        } else if (is.na(tasa_periodo)) {
          f <- function(i) pago * ((1 + i)^n_periodos - 1) / i - capital
          tasa_periodo <- uniroot(f, c(0.000001, 1))$root
        } else if (is.na(n_periodos)) {
          f <- function(n) pago * ((1 + tasa_periodo)^n - 1) / tasa_periodo - capital
          n_periodos <- round(uniroot(f, c(1, 600))$root)
        }
        saldo <- 0
        tabla <- data.frame(Periodo = 0:n_periodos, Pago = NA, Interés = NA, Amortización = NA, Saldo = NA)
        tabla[1, ] <- c(0, 0, 0, 0, round(saldo, 2))
        for (t in 1:n_periodos) {
          interes <- saldo * tasa_periodo
          amort <- pago + interes
          saldo <- saldo + amort
          tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
        }
      }
      attr(tabla, "resultado") <- list(
        capital = capital,
        tasa = tasa_periodo * unidad_tasa_map[input$unidad_tasa] * 100,
        n = n_periodos / unidad_periodo_map[input$unidad_periodo],
        pago = pago
      )
      tabla <- na.omit(tabla)  # Elimina las filas con NA
      
      return(tabla)
    } else if (input$tema == "TIR") {
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
      cat("Monto presente/futuro:", round(resumen$capital, 2), "\n")
      #cat("Tasa efectiva anual (%):", round(resumen$tasa, 2), "\n")
      cat("Número de periodos:", round(resumen$n, 2), "\n")
      cat("Pago periódico:", round(resumen$pago, 2), "\n")
    }
    
  })
}
shinyApp(ui, server)