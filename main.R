library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sistema de Matemáticas Financieras"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tema", "Selecciona un tema:",
                  choices = c("Tablas de Amortización / Ahorro", "Depreciación", "TIR", "Bonos (opcional)")),
      conditionalPanel(
        condition = "input.tema == 'Tablas de Amortización / Ahorro'",
        selectInput("tipo_operacion", "Tipo de operación:", choices = c("Deuda", "Ahorro")),
        radioButtons("Tipo_anualidad", "Tipo de Anualidad:", choices = c("Vencida", "Anticipada", "Diferida")),
        conditionalPanel(
          condition = "input.Tipo_anualidad == 'Diferida'",
          numericInput("diferido", "Periodos de diferimiento:", value = 1, min = 1)
        ),
        radioButtons("tipo_tasa", "Tipo de tasa:",
                     choices = c("Nominal convertible", "Efectiva"),
                     selected = "Nominal convertible"),
        conditionalPanel(
          condition = "input.tipo_tasa == 'Nominal convertible'",
          selectInput("frecuencia_conversion", "Frecuencia de conversión de la tasa nominal:",
                      choices = c("Anual" = 1, "Semestral" = 2, "Trimestral" = 4,
                                  "Bimestral" = 6, "Mensual" = 12), selected = 2)
        ),
        textInput("capital", "Monto presente o futuro (puede ser 'Calculalo'):", value = "10000"),
        uiOutput("tasa_ui"),
        conditionalPanel(
          condition = "input.tipo_tasa == 'Efectiva'",
          selectInput("unidad_tasa", "Unidad de periodo de la tasa efectiva:",
                      choices = c("Anual", "Mensual", "Quincenal", "Diaria"))
        ),
        textInput("periodos", "Número de periodos (puede ser 'Calculalo'):", value = "5"),
        conditionalPanel(
          condition = "!input.pagos_personalizados",
          selectInput("unidad_periodo", "Unidad de los periodos:",
                      choices = c("Años","Semestres","Bimestrales", "Meses", "Quincenas", "Días"))
        ),
        conditionalPanel(
          condition = "!input.pagos_personalizados",
          textInput("pago", "Pago por periodo (puede ser 'Calculalo'):", value = "Calculalo")
        )
      ),
      checkboxInput("pagos_personalizados", "¿Usar pagos personalizados?", value = FALSE),
      conditionalPanel(
        condition = "input.pagos_personalizados",
        textInput("pagos_lista", "Pagos personalizados (separa por comas):", "30000,30000,30000"),
        numericInput("valor_final", "Pago final para saldar la deuda:", value = 10000),
        numericInput("periodos_totales", "Total de periodos:", value = 4)
      ),
      checkboxInput("ver_saldo_especifico", "¿Ver saldo insoluto en un periodo específico?", value = FALSE),
      conditionalPanel(
        condition = "input.ver_saldo_especifico",
        numericInput("periodo_consulta", "Periodo a consultar:", value = 120, min = 1)
      ),
      checkboxInput("ver_proporcion_amortizada", "¿Ver proporción amortizada en un periodo específico?", value = FALSE),
      conditionalPanel(
        condition = "input.ver_proporcion_amortizada",
        numericInput("periodo_amortizado", "Periodo a consultar:", value = 12, min = 1)
      ),
      conditionalPanel(
        condition = "input.tema == 'TIR'",
        textInput("flujos", "Flujos de efectivo separados por coma:", value = "-1000,300,400,500")
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

server <- function(input, output) {
  output$tasa_ui <- renderUI({
    tipo <- input$tipo_tasa
    label <- if (tipo == "Nominal convertible") {
      "Tasa nominal anual (%) (puedes poner 'Calculalo'):"
    } else {
      "Tasa por periodo (%) si ya la tienes (ej. 0.4 si quincenal):"
    }
    textInput("tasa", label, value = "10")
  })
  
  datos <- reactive({
    unidad_tasa_map <- c(Anual=1, Bimestral=6, Mensual=12, Quincenal=24, Diaria=360)
    unidad_periodo_map <- c(Años=1, Semestres=2, Bimestrales=6, Meses=12, Quincenas=24, Días=360)
    
    capital <- ifelse(tolower(input$capital)%in%c("calculalo",""), NA, as.numeric(input$capital))
    tasa_in <- ifelse(tolower(input$tasa)%in%c("calculalo",""), NA, as.numeric(input$tasa))/100
    n       <- ifelse(tolower(input$periodos)%in%c("calculalo",""), NA, as.numeric(input$periodos))
    pago    <- ifelse(tolower(input$pago)%in%c("calculalo",""), NA, as.numeric(input$pago))
    
    # Calcular tasa_periodo
    if (!is.na(tasa_in)) {
      if (input$tipo_tasa=="Nominal convertible") {
        tasa_periodo <- tasa_in / as.numeric(input$frecuencia_conversion)
      } else {
        tasa_periodo <- if (tasa_in < 1.5) tasa_in
        else (1+tasa_in)^(1/unidad_tasa_map[input$unidad_tasa]) - 1
      }
    }
    n_periodos <- if (!is.na(n)) n else NA
    # Verificar un dato faltante
    if (sum(is.na(c(capital, tasa_periodo, n_periodos, pago))) != 1) {
      return(data.frame(Error="Debes dejar sólo un campo como 'Calculalo'"))
    }
    
    tipo <- input$tipo_operacion
    # BLOQUE PAGOS PERSONALIZADOS para Deuda
    if (tipo=="Deuda" && input$pagos_personalizados) {
      pagos_vec <- as.numeric(strsplit(input$pagos_lista,",")[[1]])
      # recalcular tasa_periodo si era nominal
      if (input$tipo_tasa=="Nominal convertible") {
        tasa_periodo <- tasa_in / as.numeric(input$frecuencia_conversion)
      }
      if (length(pagos_vec)<input$periodos_totales) {
        if (tolower(input$valor_final)%in%c("auto","calculalo","0")) {
          s_tmp <- capital
          for (p in pagos_vec) {
            i <- s_tmp * tasa_periodo
            a <- p - i
            s_tmp <- s_tmp - a
          }
          pago_final <- s_tmp*(1+tasa_periodo)
        } else pago_final <- as.numeric(input$valor_final)
        pagos_vec <- c(pagos_vec,pago_final)
      }
      # armar tabla
      saldo <- capital
      nper <- input$periodos_totales
      tabla <- data.frame(Periodo=0:nper, Pago=NA, Interés=NA, Amortización=NA, Saldo=NA)
      tabla[1,] <- c(0,0,0,0,round(saldo,2))
      for (t in 1:nper) {
        pmt <- pagos_vec[t]
        int <- saldo*tasa_periodo
        amt <- pmt-int
        if (t==nper) { amt<-saldo; int<-pmt-amt; saldo<-0 }
        else saldo<-saldo-amt
        tabla[t+1,] <- c(t,round(pmt,2),round(int,2),round(amt,2),round(saldo,2))
      }
      attr(tabla,"resultado") <- list(capital=capital, tasa=tasa_periodo*100,
                                      n=nper, pago=paste(pagos_vec,collapse=", "))
      return(tabla)
    }
    
    # BLOQUE DEUDA (anualidades iguales)
    if (tipo=="Deuda") {
      # calcular el pago si falta
      if (is.na(pago)) {
        k <- if (input$Tipo_anualidad=="Diferida") as.numeric(input$diferido) else 0
        if (input$Tipo_anualidad=="Vencida") {
          pago <- capital*(tasa_periodo*(1+tasa_periodo)^n_periodos)/((1+tasa_periodo)^n_periodos-1)
        } else if (input$Tipo_anualidad=="Anticipada") {
          pago <- capital*(tasa_periodo*(1+tasa_periodo)^n_periodos)/((1+tasa_periodo)^n_periodos-1)/(1+tasa_periodo)
        } else {
          pago <- capital*(tasa_periodo*(1+tasa_periodo)^n_periodos)/((1+tasa_periodo)^n_periodos-1)*(1+tasa_periodo)^k
        }
      }
      # cuando n se calcula decimal
      if (is.na(n_periodos)) {
        f <- function(x) capital*((tasa_periodo*(1+tasa_periodo)^x)/((1+tasa_periodo)^x-1)) - pago
        n_decimal <- uniroot(f,c(1,600))$root
        n_periodos <- floor(n_decimal)
        ultimo_periodo_incompleto <- TRUE
      } else ultimo_periodo_incompleto <- FALSE
      # generar tabla
      saldo <- capital
      tabla <- data.frame(Periodo=0:n_periodos, Pago=NA, Interés=NA, Amortización=NA, Saldo=NA)
      tabla[1,] <- c(0,0,0,0,round(saldo,2))
      for (t in 1:n_periodos) {
        int<-saldo*tasa_periodo
        amt<-pago-int
        saldo<-saldo-amt
        tabla[t+1,] <- c(t,round(pago,2),round(int,2),round(amt,2),round(saldo,2))
      }
      if (isTRUE(ultimo_periodo_incompleto)&&saldo>1e-2) {
        int<-saldo*tasa_periodo; pft<-saldo+int; amt<-pft-int; saldo<-0
        tabla[nrow(tabla)+1,] <- c(n_periodos+1,round(pft,2),round(int,2),round(amt,2),round(saldo,2))
      }
      attr(tabla,"resultado") <- list(capital=capital,tasa=tasa_periodo*100,
                                      n=ifelse(exists("n_decimal"),n_decimal, n_periodos),
                                      pago=pago)
      return(tabla)
    }
    
    # BLOQUE AHORRO
    if (tipo=="Ahorro") {
      k<- if(input$Tipo_anualidad=="Diferida") as.numeric(input$diferido) else 0
      ajuste<- if(input$Tipo_anualidad=="Anticipada") 1+tasa_periodo else 1
      if (is.na(pago)) {
        pago<- (capital*tasa_periodo)/(((1+tasa_periodo)^(n_periodos+k)-1)/tasa_periodo*ajuste)
      }
      if (is.na(n)) {
        f<- function(x) pago*(((1+tasa_periodo)^(x+k)-1)/tasa_periodo*ajuste)-capital
        n_decimal<- uniroot(f,c(1,600))$root
        n_periodos<- floor(n_decimal); ultimo_periodo_incompleto<-TRUE
      } else ultimo_periodo_incompleto<-FALSE
      saldo<-0
      tabla<- data.frame(Periodo=0:(n_periodos+k),Pago=NA,Interés=NA,Aporte=NA,Saldo=NA)
      tabla[1,]<-c(0,0,0,0,round(saldo,2))
      for(t in 1:(n_periodos+k)) {
        pmt<- if(t<=k) 0 else if(input$Tipo_anualidad=="Anticipada") pago else pago
        if(input$Tipo_anualidad=="Anticipada" && t>k) {
          saldo<-saldo+pmt; int<-saldo*tasa_periodo; saldo<-saldo+int
          tabla[t+1,]<-c(t,round(pmt,2),round(int,2),round(pmt,2),round(saldo,2))
          next
        }
        int<-saldo*tasa_periodo; saldapre<-saldo; aporte<-pmt; saldo<-saldo+aporte+int
        tabla[t+1,]<-c(t,round(pmt,2),round(int,2),round(aporte,2),round(saldo,2))
      }
      if (isTRUE(ultimo_periodo_incompleto)) {
        int<-saldo*tasa_periodo; pft<-saldo+int; saldo<-0
        tabla[nrow(tabla)+1,] <- c(n_periodos+k+1,round(pft,2),round(int,2),round(pft,2),round(saldo,2))
      }
      attr(tabla,"resultado") <- list(capital=capital,
                                      tasa=tasa_periodo*unidad_tasa_map[input$unidad_tasa]*100,
                                      n=if(exists("n_decimal")) n_decimal/unidad_periodo_map[input$unidad_periodo]
                                      else n_periodos/unidad_periodo_map[input$unidad_periodo],
                                      pago=pago)
      return(tabla)
    }
  })
  
  output$tabla <- renderDT({ datos() })
  output$grafica <- renderPlot({
    tab<-datos()
    if(!("Saldo" %in% colnames(tab))) return()
    ggplot(tab,aes(Periodo,Saldo))+
      geom_line(size=1.2)+geom_point()+
      labs(title="Evolución del Saldo",x="Periodo",y="Saldo")+
      theme_minimal()
  })
  output$resultado <- renderPrint({
    if(input$tema=="Tablas de Amortización / Ahorro") {
      res<-attr(datos(),"resultado")
      cat("Resumen:\n")
      cat("Capital:",round(res$capital,2),"\n")
      cat("Tasa (%):",round(res$tasa,2),"\n")
      cat("Periodos:",round(res$n,2),"\n")
      cat("Pago:",res$pago,"\n")
    } else if(input$tema=="TIR") {
      flujos<- as.numeric(strsplit(input$flujos,",")[[1]])
      tir<- uniroot(function(r) sum(flujos/(1+r)^(0:(length(flujos)-1))), c(-0.99,1))$root
      cat("TIR:",round(tir*100,2),"%")
    } else if(input$tema=="Bonos (opcional)") {
      Vn<-input$valor_nominal; c<-input$cupon/100; r<-input$rendimiento/100; n<-input$años_bono
      precio<-sum((Vn*c)/(1+r)^(1:n))+Vn/(1+r)^n
      cat("Precio:",round(precio,2))
    }
  })
}

shinyApp(ui, server)
