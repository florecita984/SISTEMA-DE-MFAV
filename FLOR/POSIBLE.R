library(shiny) #este no se 
library(DT)
library(ggplot2)

# Constantes y mapeos globales
UNIDAD_TASA_MAP <- c(Anual = 1, Bimestral = 6, Mensual = 12, Quincenal = 24, Diaria = 360)
UNIDAD_PERIODO_MAP <- c(Años = 1, Semestres = 2, Bimestrales = 6, Meses = 12, Quincenas = 24, Días = 360)

# Funciones auxiliares para manejo de entrada
obtener_valores_entrada <- function(input) {
  # Leer inputs básicos
  capital <- suppressWarnings(ifelse(tolower(input$capital) %in% c("calculalo", ""), NA, as.numeric(input$capital)))
  tasa <- suppressWarnings(ifelse(tolower(input$tasa) %in% c("calculalo", ""), NA, as.numeric(input$tasa))) / 100
  n <- suppressWarnings(ifelse(tolower(input$periodos) %in% c("calculalo", ""), NA, as.numeric(input$periodos)))
  pago <- if (input$tipo_operacion == "Deuda") {
    suppressWarnings(ifelse(tolower(input$pago) %in% c("calculalo", ""), NA, as.numeric(input$pago)))
  } else {
    NA  # Para fondos de amortización, el pago siempre se calcula
  }
  
  # Leer enganche para deuda
  enganche <- if (!is.null(input$tiene_enganche) && input$tiene_enganche && input$tipo_operacion == "Deuda") {
    input$monto_enganche
  } else {
    0
  }
  
  # Calcular tasa por periodo
  tasa_periodo <- NA
  if (!is.na(tasa)) {
    if (!is.null(input$tipo_tasa) && input$tipo_tasa == "Nominal convertible") {
      frecuencia <- as.numeric(input$frecuencia_conversion)
      tasa_periodo <- tasa / frecuencia
    } else if (!is.null(input$tipo_tasa) && input$tipo_tasa == "Efectiva") {
      if (tasa < 1.5) {
        tasa_periodo <- tasa
      } else {
        tasa_periodo <- (1 + tasa)^(1 / UNIDAD_TASA_MAP[input$unidad_tasa]) - 1
      }
    }
  }
  
  # Retornar lista de valores
  list(
    capital = capital,
    tasa = tasa,
    tasa_periodo = tasa_periodo,
    n_periodos = if (!is.na(n)) n else NA,
    pago = pago,
    tipo_operacion = input$tipo_operacion,
    tipo_anualidad = input$Tipo_anualidad,
    diferido = if (!is.null(input$Tipo_anualidad) && input$Tipo_anualidad == "Diferida") as.numeric(input$diferido) else 0,
    enganche = enganche
  )
}

# Función para validar inputs
validar_inputs <- function(valores) {
  datos_faltantes <- sum(is.na(c(
    valores$capital, 
    valores$tasa_periodo, 
    valores$n_periodos, 
    valores$pago
  )))
  return(datos_faltantes == 1)
}

# Funciones para la tasa
calcular_tasa_periodo <- function(tasa, tipo_tasa, frecuencia_conversion = NULL, unidad_tasa = NULL) {
  if (tipo_tasa == "Nominal convertible") {
    return(tasa / as.numeric(frecuencia_conversion))
  } else {
    if (tasa < 1.5) {
      return(tasa)
    }
    return((1 + tasa)^(1 / UNIDAD_TASA_MAP[unidad_tasa]) - 1)
  }
}
#anualidades vencidas, anticipadas y diferidas
calcular_pago_deuda <- function(capital, tasa_periodo, n_periodos, tipo_anualidad, k = 0) {
  base <- capital * (tasa_periodo * (1 + tasa_periodo)^n_periodos) / ((1 + tasa_periodo)^n_periodos - 1)
  switch(tipo_anualidad,
         "Vencida" = base,
         "Anticipada" = base / (1 + tasa_periodo),
         "Diferida" = base * (1 + tasa_periodo)^k)
}

calcular_capital_deuda <- function(pago, tasa_periodo, n_periodos, tipo_anualidad, k = 0) {
  base <- pago * ((1 + tasa_periodo)^n_periodos - 1) / (tasa_periodo * (1 + tasa_periodo)^n_periodos)
  switch(tipo_anualidad,
         "Vencida" = base,
         "Anticipada" = base * (1 + tasa_periodo),
         "Diferida" = base / (1 + tasa_periodo)^k)
}

# Función para calcular tabla con amortización por porcentajes
calcular_tabla_deuda_porcentajes <- function(valores, input) {
  # Extraer valores básicos
  capital_original <- valores$capital
  capital <- capital_original - valores$enganche
  tasa_periodo <- valores$tasa_periodo
  
  # Procesar porcentajes
  porcentajes <- as.numeric(unlist(strsplit(input$porcentajes_lista, ","))) / 100
  if (abs(sum(porcentajes) - 1) > 0.0001) {
    return(data.frame(Error = "Los porcentajes deben sumar 100%"))
  }
  
  # Calcular montos de amortización por periodo
  amortizaciones <- capital * porcentajes
  n_pagos <- length(porcentajes)
  
  # Generar tabla
  tabla <- data.frame(
    Periodo = 0:n_pagos,
    Pago = NA,
    Interés = NA,
    Amortización = NA,
    Saldo = NA
  )
  
  # Establecer valores iniciales considerando el enganche
  saldo <- capital_original
  tabla[1, ] <- c(0, valores$enganche, 0, valores$enganche, round(capital, 2))
  
  # Calcular valores para cada periodo
  for (t in 1:n_pagos) {
    interes <- saldo * tasa_periodo
    amort <- amortizaciones[t]
    pago <- amort + interes
    saldo <- saldo - amort
    
    tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
  }
  
  # Agregar atributos con resultados
  attr(tabla, "resultado") <- list(
    capital = capital_original,
    capital_financiado = capital,
    enganche = valores$enganche,
    tasa = tasa_periodo * 100,
    n = n_pagos,
    pago = paste(round(tabla$Pago[-1], 2), collapse = ", "),
    porcentajes = paste(porcentajes * 100, "%", collapse = ", ")
  )
  
  return(tabla)
}

calcular_tabla_deuda <- function(valores, input) {
  if (!is.null(input$usar_porcentajes) && input$usar_porcentajes) {
    return(calcular_tabla_deuda_porcentajes(valores, input))
  } else if (!is.null(input$pagos_personalizados) && input$pagos_personalizados) {
    return(calcular_tabla_deuda_personalizada(valores, input))
  }
  
  # Extraer valores
  capital_original <- valores$capital
  capital <- capital_original - valores$enganche  # Ajustar capital por enganche
  tasa_periodo <- valores$tasa_periodo
  n_periodos <- valores$n_periodos
  pago <- valores$pago
  k <- valores$diferido
  
  # Calcular variable faltante
  if (is.na(pago)) {
    pago <- calcular_pago_deuda(capital, tasa_periodo, n_periodos, valores$tipo_anualidad, k)
  } else if (is.na(capital)) {
    capital <- calcular_capital_deuda(pago, tasa_periodo, n_periodos, valores$tipo_anualidad, k)
    capital_original <- capital + valores$enganche
  } else if (is.na(tasa_periodo)) {
    tasa_periodo <- calcular_tasa_deuda(capital, pago, n_periodos, valores$tipo_anualidad, k)
  } else if (is.na(n_periodos)) {
    n_periodos <- calcular_periodos_deuda(capital, pago, tasa_periodo, valores$tipo_anualidad, k)
  }
  
  # Generar tabla de amortización
  total_periodos <- n_periodos + k
  tabla <- data.frame(
    Periodo = 0:total_periodos,
    Pago = NA,
    Interés = NA,
    Amortización = NA,
    Saldo = NA
  )
  
  # Establecer valores iniciales considerando el enganche
  saldo <- capital_original
  tabla[1, ] <- c(0, valores$enganche, 0, valores$enganche, round(capital, 2))
  
  # Calcular valores para cada periodo
  for (t in 1:total_periodos) {
    if (t <= k) {
      # Periodo de diferimiento
      interes <- saldo * tasa_periodo
      saldo <- saldo + interes
      tabla[t + 1, ] <- c(t, 0, round(interes, 2), 0, round(saldo, 2))
    } else {
      # Periodo normal
      interes <- saldo * tasa_periodo
      amort <- pago - interes
      saldo <- saldo - amort
      tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(amort, 2), round(saldo, 2))
    }
  }
  
  # Agregar atributos con resultados
  attr(tabla, "resultado") <- list(
    capital = capital_original,
    capital_financiado = capital,
    enganche = valores$enganche,
    tasa = tasa_periodo * 100,
    n = n_periodos,
    pago = pago
  )
  
  return(tabla)
}

calcular_tabla_ahorro <- function(valores, input) {
  # Extraer valores básicos
  capital <- valores$capital  # Objetivo a acumular
  tasa_periodo <- valores$tasa_periodo
  n_periodos <- valores$n_periodos
  k <- if (!is.null(input$Tipo_anualidad) && input$Tipo_anualidad == "Diferida") valores$diferido else 0
  
  # Calcular el pago necesario según el tipo de anualidad
  if (is.na(pago)) {
    pago <- switch(input$Tipo_anualidad,
      "Vencida" = capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1),
      "Anticipada" = capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1) / (1 + tasa_periodo),
      "Diferida" = capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1) * (1 + tasa_periodo)^k
    )
  }
  
  # Generar tabla
  total_periodos <- n_periodos + k
  tabla <- data.frame(
    Periodo = 0:total_periodos,
    Depósito = NA,
    Interés = NA,
    Fondo_Acumulado = NA
  )
  
  # Establecer valores iniciales
  saldo <- 0
  tabla[1, ] <- c(0, 0, 0, round(saldo, 2))
  
  # Calcular valores para cada periodo
  for (t in 1:total_periodos) {
    if (t <= k) {
      # Periodo de diferimiento
      interes <- saldo * tasa_periodo
      saldo <- saldo + interes
      tabla[t + 1, ] <- c(t, 0, round(interes, 2), round(saldo, 2))
    } else {
      # Periodo normal
      if (input$Tipo_anualidad == "Anticipada") {
        saldo <- saldo + pago
        interes <- saldo * tasa_periodo
        saldo <- saldo + interes
      } else {
        interes <- saldo * tasa_periodo
        saldo <- saldo + interes + pago
      }
      tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(saldo, 2))
    }
  }
  
  # Agregar atributos con resultados
  attr(tabla, "resultado") <- list(
    objetivo = capital,
    tasa = tasa_periodo * 100,
    n = n_periodos,
    pago = pago,
    tipo_anualidad = input$Tipo_anualidad,
    periodos_diferidos = k
  )
  
  return(tabla)
}

# Funciones para mostrar resultados
mostrar_resultado_tir <- function(input) {
  flujos <- as.numeric(unlist(strsplit(input$flujos, ",")))
  tir <- uniroot(function(r) sum(flujos / (1 + r)^(0:(length(flujos) - 1))), c(-0.99, 1))$root
  cat("La TIR es aproximadamente:", round(tir * 100, 2), "%")
}

mostrar_resultado_amortizacion <- function(input, datos_tabla) {
  if (is.null(datos_tabla) || "Error" %in% colnames(datos_tabla)) return(NULL)
  resumen <- attr(datos_tabla, "resultado")
  
  cat("Resumen de la operación:\n")
  
  if (input$tipo_operacion == "Ahorro") {
    cat("=== FONDO DE AMORTIZACIÓN ===\n")
    cat("Objetivo a acumular: $", format(resumen$objetivo, big.mark = ","), "\n")
    cat("Depósito periódico necesario: $", format(round(resumen$pago, 2), big.mark = ","), "\n")
    cat("Tasa por periodo:", round(resumen$tasa, 4), "%\n")
    cat("Número de periodos:", resumen$n, "\n")
    cat("Tipo de anualidad:", resumen$tipo_anualidad, "\n")
    if (resumen$tipo_anualidad == "Diferida") {
      cat("Periodos de diferimiento:", resumen$periodos_diferidos, "\n")
    }
    cat("\nNota: La tabla muestra la evolución del fondo periodo a periodo\n")
  } else {
    if (!is.null(resumen$capital) && is.numeric(resumen$capital)) {
      cat("Monto total:", round(resumen$capital, 2), "\n")
      
      if (!is.null(resumen$enganche) && resumen$enganche > 0) {
        cat("Enganche:", round(resumen$enganche, 2), "\n")
        cat("Monto financiado:", round(resumen$capital_financiado, 2), "\n")
      }
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
    
    if (!is.null(input$ver_saldo_especifico) && input$ver_saldo_especifico) {
      periodo <- input$periodo_consulta
      if (periodo <= nrow(datos_tabla) - 1 && "Saldo" %in% colnames(datos_tabla)) {
        saldo_periodo <- datos_tabla[periodo + 1, "Saldo"]
        cat("\nSaldo insoluto al final del periodo", periodo, ":", round(saldo_periodo, 2))
      }
    }
    
    if (!is.null(input$ver_proporcion_amortizada) && input$ver_proporcion_amortizada) {
      periodo <- input$periodo_amortizado
      if (periodo <= nrow(datos_tabla) - 1 && "Amortización" %in% colnames(datos_tabla)) {
        amort_acum <- sum(datos_tabla[2:(periodo + 1), "Amortización"])
        proporcion <- amort_acum / resumen$capital
        cat("\nProporción del saldo amortizado al periodo", periodo, ":", round(proporcion * 100, 2), "%")
        cat("\nMonto amortizado: $", round(amort_acum, 2))
      }
    }
  }
}

# Funciones auxiliares para cálculos de tasas y periodos
calcular_tasa_deuda <- function(capital, pago, n_periodos, tipo_anualidad, k = 0) {
  f <- switch(tipo_anualidad,
    "Vencida" = function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) - pago,
    "Anticipada" = function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) / (1 + i) - pago,
    "Diferida" = function(i) capital * (i * (1 + i)^n_periodos) / ((1 + i)^n_periodos - 1) * (1 + i)^k - pago
  )
  uniroot(f, c(0.000001, 1))$root
}

calcular_periodos_deuda <- function(capital, pago, tasa_periodo, tipo_anualidad, k = 0) {
  f <- switch(tipo_anualidad,
    "Vencida" = function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) - pago,
    "Anticipada" = function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) / (1 + tasa_periodo) - pago,
    "Diferida" = function(n) capital * (tasa_periodo * (1 + tasa_periodo)^n) / ((1 + tasa_periodo)^n - 1) * (1 + tasa_periodo)^k - pago
  )
  n_decimal <- uniroot(f, c(1, 600))$root
  floor(n_decimal)
}

# Función para manejar pagos personalizados
calcular_tabla_deuda_personalizada <- function(valores, input) {
  # Extraer valores
  capital <- valores$capital
  tasa_periodo <- valores$tasa_periodo
  
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
      (1 + tasas_base)^(1 / UNIDAD_TASA_MAP[input$unidad_tasa_var]) - 1
    }
  } else {
    rep(tasa_periodo, input$periodos_totales)
  }
  
  # Inicializar tabla
  n_personalizado <- input$periodos_totales
  tabla <- data.frame(
    Periodo = 0:n_personalizado,
    Pago = NA,
    Interés = NA,
    Amortización = NA,
    Saldo = NA,
    "Tasa %" = NA
  )
  
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
    
    tabla[t + 1, ] <- c(
      t,
      round(pago_t, 2),
      round(interes, 2),
      round(amort, 2),
      round(saldo, 2),
      round(tasa_actual * 100, 2)
    )
  }
  
  # Si queda un período final, calcular el último pago
  if (length(pagos_vec) < n_personalizado) {
    t <- length(pagos_vec) + 1
    tasa_actual <- tasas_variables[t]
    interes <- saldo * tasa_actual
    pago_final <- saldo + interes
    amort <- saldo
    saldo <- 0
    
    tabla[t + 1, ] <- c(
      t,
      round(pago_final, 2),
      round(interes, 2),
      round(amort, 2),
      round(saldo, 2),
      round(tasa_actual * 100, 2)
    )
  }
  
  # Agregar atributos con resultados
  attr(tabla, "resultado") <- list(
    capital = capital,
    tasa = tasa_periodo * 100,
    n = n_personalizado,
    pago = paste(c(pagos_vec, if(length(pagos_vec) < n_personalizado) round(pago_final,2)), collapse = ", ")
  )
  
  return(tabla)
}

# Función para calcular el pago del fondo de amortización
calcular_pago_fondo <- function(capital, tasa_periodo, n_periodos, tipo_anualidad, k = 0) {
  base <- capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1)
  switch(tipo_anualidad,
         "Vencida" = base,
         "Anticipada" = base / (1 + tasa_periodo),
         "Diferida" = base * (1 + tasa_periodo)^k,
         base)  # default case
}

# Función para calcular tabla de fondo de amortización
calcular_tabla_fondo <- function(valores, input) {
  # Extraer valores básicos
  capital <- valores$capital  # Objetivo a acumular
  tasa_periodo <- valores$tasa_periodo
  n_periodos <- valores$n_periodos
  k <- valores$diferido
  
  # Calcular el pago necesario según el tipo de anualidad
  pago <- switch(valores$tipo_anualidad,
    "Vencida" = capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1),
    "Anticipada" = (capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1)) / (1 + tasa_periodo),
    "Diferida" = (capital * tasa_periodo / ((1 + tasa_periodo)^n_periodos - 1)) * (1 + tasa_periodo)^k
  )
  
  # Generar tabla
  total_periodos <- n_periodos + k
  tabla <- data.frame(
    Periodo = 0:total_periodos,
    Depósito = NA,
    Interés = NA,
    Fondo_Acumulado = NA
  )
  
  # Establecer valores iniciales
  saldo <- 0
  tabla[1, ] <- c(0, 0, 0, round(saldo, 2))
  
  # Calcular valores para cada periodo
  for (t in 1:total_periodos) {
    if (t <= k) {
      # Periodo de diferimiento
      interes <- saldo * tasa_periodo
      saldo <- saldo + interes
      tabla[t + 1, ] <- c(t, 0, round(interes, 2), round(saldo, 2))
    } else {
      # Periodo normal
      if (valores$tipo_anualidad == "Anticipada") {
        # Para anualidad anticipada, primero se hace el depósito
        saldo <- saldo + pago
        interes <- saldo * tasa_periodo
        saldo <- saldo + interes
      } else {
        # Para anualidad vencida, primero se calcula el interés
        interes <- saldo * tasa_periodo
        saldo <- saldo + interes + pago
      }
      tabla[t + 1, ] <- c(t, round(pago, 2), round(interes, 2), round(saldo, 2))
    }
  }
  
  # Agregar atributos con resultados
  attr(tabla, "resultado") <- list(
    objetivo = capital,
    tasa = tasa_periodo * 100,
    n = n_periodos,
    pago = pago,
    tipo_anualidad = valores$tipo_anualidad,
    periodos_diferidos = k
  )
  
  return(tabla)
}

#interfaz----
ui <- fluidPage(
  titlePanel("Sistema de Matemáticas Financieras"),
  
  # CSS personalizado para mensajes
  tags$head(
    tags$style(HTML("
      .error-message { 
        color: #d9534f; 
        background-color: #f2dede; 
        border: 1px solid #ebccd1;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .info-message {
        color: #31708f;
        background-color: #d9edf7;
        border: 1px solid #bce8f1;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .help-text {
        font-size: 0.9em;
        color: #666;
        font-style: italic;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tema", "Selecciona un tema:",
                  choices = c("Tablas de Amortización / Ahorro", "Depreciación", "TIR", "Bonos (opcional)")),
      
      conditionalPanel(
        condition = "input.tema == 'Tablas de Amortización / Ahorro'",
        selectInput("tipo_operacion", "Tipo de operación:", 
                   choices = c("Deuda" = "Deuda", "Fondo de Amortización" = "Ahorro")),
        
        # Panel para Deuda
        conditionalPanel(
          condition = "input.tipo_operacion == 'Deuda'",
          div(class = "info-message",
              "Calcula pagos, intereses y saldos de una deuda. Puedes dejar UN campo como 'Calculalo'."
          ),
          
          textInput("capital", "Monto del préstamo:", value = "10000"),
          checkboxInput("tiene_enganche", "¿Tiene enganche?", FALSE),
          conditionalPanel(
            condition = "input.tiene_enganche == true",
            numericInput("monto_enganche", "Monto del enganche:", value = 0, min = 0)
          ),
          
          radioButtons("tipo_pago", "Tipo de pago:",
                      choices = c(
                        "Regular" = "regular",
                        "Porcentajes de amortización" = "porcentajes",
                        "Pagos personalizados" = "personalizados"
                      )),
          
          conditionalPanel(
            condition = "input.tipo_pago == 'porcentajes'",
            textInput("porcentajes_lista", "Porcentajes (deben sumar 100%):", "50,30,20"),
            div(class = "help-text", "Ejemplo: 50,30,20 para tres pagos que amortizan 50%, 30% y 20% del capital")
          ),
          
          conditionalPanel(
            condition = "input.tipo_pago == 'personalizados'",
            textInput("pagos_lista", "Lista de pagos:", "30000,30000,30000"),
            div(class = "help-text", "Ingresa los pagos separados por comas")
          ),
          
          conditionalPanel(
            condition = "input.tipo_pago == 'regular'",
            textInput("pago", "Pago periódico:", value = "Calculalo")
          )
        ),
        
        # Panel para Fondo de Amortización
        conditionalPanel(
          condition = "input.tipo_operacion == 'Ahorro'",
          div(class = "info-message",
              "Calcula el depósito periódico necesario para acumular un objetivo"
          ),
          numericInput("capital", "Monto objetivo:", value = 100000, min = 0),
          numericInput("periodos", "Número de periodos:", value = 12, min = 1)
        ),
        
        # Campos comunes para ambos tipos
        radioButtons("Tipo_anualidad", "Tipo de Anualidad:",
                    choices = c("Vencida", "Anticipada", "Diferida")),
        
        conditionalPanel(
          condition = "input.Tipo_anualidad == 'Diferida'",
          numericInput("diferido", "Periodos de diferimiento:", value = 1, min = 1)
        ),
        
        radioButtons("tipo_tasa", "Tipo de tasa:",
                    choices = c("Nominal convertible", "Efectiva")),
        
        conditionalPanel(
          condition = "input.tipo_tasa == 'Nominal convertible'",
          textInput("tasa", "Tasa nominal anual (%):", value = "10"),
          selectInput("frecuencia_conversion", "Frecuencia de conversión:",
                     choices = c("Anual" = 1, "Semestral" = 2, "Trimestral" = 4, 
                               "Bimestral" = 6, "Mensual" = 12),
                     selected = 12)
        ),
        
        conditionalPanel(
          condition = "input.tipo_tasa == 'Efectiva'",
          textInput("tasa", "Tasa efectiva (%):", value = "10"),
          selectInput("unidad_tasa", "Unidad de periodo:",
                     choices = c("Anual", "Mensual", "Quincenal", "Diaria"))
        ),
        
        selectInput("unidad_periodo", "Unidad de los periodos:",
                    choices = c("Años", "Semestres", "Bimestrales", "Meses", "Quincenas", "Días"))
      )
    ),
    mainPanel(
      h3("Resultado"),
      # Panel de mensajes de error/información
      uiOutput("mensajes"),
      # Resultados
      DTOutput("tabla"),
      plotOutput("grafica"),
      verbatimTextOutput("resultado")
    )
  )
)
#la lógica del servidor
server <- function(input, output, session) {
  # Función reactiva principal para cálculos
  datos <- reactive({
    req(input$tema)
    
    if (input$tema == "Tablas de Amortización / Ahorro") {
      valores <- obtener_valores_entrada(input)
      
      if (input$tipo_operacion == "Deuda") {
        # Para deudas, validar que solo un campo sea "Calculalo"
        if (!validar_inputs(valores)) {
          return(data.frame(Error = "Debes dejar solo un campo como 'Calculalo'"))
        }
        
        if (isTRUE(input$pagos_personalizados)) {
          return(calcular_tabla_deuda_personalizada(valores, input))
        } else if (isTRUE(input$usar_porcentajes)) {
          return(calcular_tabla_deuda_porcentajes(valores, input))
        } else {
          return(calcular_tabla_deuda(valores, input))
        }
      } else {
        # Para fondos de amortización, no necesitamos validación de "Calculalo"
        return(calcular_tabla_fondo(valores, input))
      }
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
  
  output$mensajes <- renderUI({
    if (input$tema != "Tablas de Amortización / Ahorro") return(NULL)
    
    valores <- obtener_valores_entrada(input)
    
    if (input$tipo_operacion == "Deuda") {
      datos_faltantes <- sum(is.na(c(valores$capital, valores$tasa_periodo, valores$n_periodos, valores$pago)))
      
      if (datos_faltantes > 1) {
        div(class = "error-message",
            "Error: Has dejado más de un campo como 'Calculalo'. Solo debe haber uno.")
      } else if (datos_faltantes == 0) {
        div(class = "error-message",
            "Error: Debes dejar exactamente un campo como 'Calculalo'.")
      } else if (input$tipo_pago == "porcentajes") {
        porcentajes <- as.numeric(unlist(strsplit(input$porcentajes_lista, ",")))
        if (abs(sum(porcentajes) - 100) > 0.01) {
          div(class = "error-message",
              "Error: Los porcentajes deben sumar exactamente 100%")
        }
      }
    }
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
      mostrar_resultado_tir(input)
    } else if (input$tema == "Tablas de Amortización / Ahorro") {
      mostrar_resultado_amortizacion(input, datos())
    }
  })
  
}
shinyApp(ui, server)