# Instala los paquetes necesarios si no los tienes
# install.packages("shiny")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Distribuciones de Probabilidad Discretas y Continuas"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribucion", "Selecciona la distribución:",
                  choices = c("Binomial", "Poisson", "Hipergeométrica", "Binomial Negativa", "Geométrica", "Normal", "Uniforme", "Beta", "Gamma", "Weibull", "Exponencial", "Chi-Cuadrada", "t de Student", "F")),
      conditionalPanel(
        condition = "input.distribucion == 'Binomial'",
        numericInput("n", "Número de ensayos (n):", value = 10, min = 1),
        numericInput("p", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1),
        numericInput("x_bin", "Valor para calcular P(X = x):", value = 5, min = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Poisson'",
        numericInput("lambda", "Parámetro (λ):", value = 5, min = 0),
        numericInput("x_poisson", "Valor para calcular P(X = x):", value = 5, min = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Hipergeométrica'",
        numericInput("N", "Tamaño de la población (N):", value = 20, min = 1),
        numericInput("K", "Número de éxitos en la población (K):", value = 10, min = 0),
        numericInput("n_hyper", "Tamaño de la muestra (n):", value = 5, min = 1),
        numericInput("x_hyper", "Valor para calcular P(X = x):", value = 3, min = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Binomial Negativa'",
        numericInput("r", "Número de éxitos deseados (r):", value = 5, min = 1),
        numericInput("p_neg", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1),
        numericInput("x_neg", "Valor para calcular P(X = x):", value = 3, min = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Geométrica'",
        numericInput("p_geom", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1),
        numericInput("x_geom", "Valor para calcular P(X = x):", value = 3, min = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Normal'",
        numericInput("mu", "Media (μ):", value = 0),
        numericInput("sigma", "Desviación estándar (σ):", value = 1, min = 0.01),
        numericInput("x_norm", "Valor para calcular P(X = x):", value = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Uniforme'",
        numericInput("a", "Mínimo (a):", value = 0),
        numericInput("b", "Máximo (b):", value = 1),
        numericInput("x_unif", "Valor para calcular P(X = x):", value = 0.5)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Beta'",
        numericInput("alpha", "Parámetro α:", value = 2, min = 0.01),
        numericInput("beta", "Parámetro β:", value = 5, min = 0.01),
        numericInput("x_beta", "Valor para calcular P(X = x):", value = 0.5)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Gamma'",
        numericInput("k", "Forma (k):", value = 2, min = 0.01),
        numericInput("theta", "Escala (θ):", value = 1, min = 0.01),
        numericInput("x_gamma", "Valor para calcular P(X = x):", value = 5)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Weibull'",
        numericInput("shape", "Forma (k):", value = 1, min = 0.01),
        numericInput("scale", "Escala (λ):", value = 1, min = 0.01),
        numericInput("x_weibull", "Valor para calcular P(X = x):", value = 5)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Exponencial'",
        numericInput("lambda_exp", "Tasa (λ):", value = 1, min = 0.01),
        numericInput("x_exp", "Valor para calcular P(X = x):", value = 2)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'Chi-Cuadrada'",
        numericInput("df", "Grados de libertad (df):", value = 2, min = 1),
        numericInput("x_chisq", "Valor para calcular P(X = x):", value = 2)
      ),
      conditionalPanel(
        condition = "input.distribucion == 't de Student'",
        numericInput("df_t", "Grados de libertad (df):", value = 2, min = 1),
        numericInput("x_t", "Valor para calcular P(X = x):", value = 0)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'F'",
        numericInput("df1", "Grados de libertad del numerador (df1):", value = 2, min = 1),
        numericInput("df2", "Grados de libertad del denominador (df2):", value = 2, min = 1),
        numericInput("x_f", "Valor para calcular P(X = x):", value = 1)
      ),
      actionButton("plot", "Generar Gráfico"),
      actionButton("calc_prob", "Calcular Probabilidad")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("probabilidad")
    )
  )
)

# Define la lógica del servidor
server <- function(input, output) {
  observeEvent(input$plot, {
    req(input$distribucion)
    
    if (input$distribucion == "Binomial") {
      x <- 0:input$n
      prob <- dbinom(x, size = input$n, prob = input$p)
      data <- data.frame(x, prob)
      title <- "Distribución Binomial"
      
    } else if (input$distribucion == "Poisson") {
      x <- 0:20 # Rango de valores para Poisson
      prob <- dpois(x, lambda = input$lambda)
      data <- data.frame(x, prob)
      title <- "Distribución de Poisson"
      
    } else if (input$distribucion == "Hipergeométrica") {
      x <- 0:min(input$K, input$n_hyper) # Posibles valores de éxito
      prob <- dhyper(x, m = input$K, n = input$N - input$K, k = input$n_hyper)
      data <- data.frame(x, prob)
      title <- "Distribución Hipergeométrica"
      
    } else if (input$distribucion == "Binomial Negativa") {
      x <- 0:30 # Rango de valores (ajusta según necesites)
      prob <- dnbinom(x, size = input$r, prob = input$p_neg)
      data <- data.frame(x, prob)
      title <- "Distribución Binomial Negativa"
      
    } else if (input$distribucion == "Geométrica") {
      x <- 0:30 # Rango de valores (ajusta según necesites)
      prob <- dgeom(x, prob = input$p_geom)
      data <- data.frame(x, prob)
      title <- "Distribución Geométrica"
      
    } else if (input$distribucion == "Normal") {
      x <- seq(input$mu - 4 * input$sigma, input$mu + 4 * input$sigma, length.out = 100)
      prob <- dnorm(x, mean = input$mu, sd = input$sigma)
      data <- data.frame(x, prob)
      title <- "Distribución Normal"
      
    } else if (input$distribucion == "Uniforme") {
      x <- seq(input$a, input$b, length.out = 100)
      prob <- dunif(x, min = input$a, max = input$b)
      data <- data.frame(x, prob)
      title <- "Distribución Uniforme"
      
    } else if (input$distribucion == "Beta") {
      x <- seq(0, 1, length.out = 100)
      prob <- dbeta(x, shape1 = input$alpha, shape2 = input$beta)
      data <- data.frame(x, prob)
      title <- "Distribución Beta"
      
    } else if (input$distribucion == "Gamma") {
      x <- seq(0, 30, length.out = 100) # Ajusta el rango según sea necesario
      prob <- dgamma(x, shape = input$k, scale = input$theta)
      data <- data.frame(x, prob)
      title <- "Distribución Gamma"
      
    } else if (input$distribucion == "Weibull") {
      x <- seq(0, 30, length.out = 100) # Ajusta el rango según sea necesario
      prob <- dweibull(x, shape = input$shape, scale = input$scale)
      data <- data.frame(x, prob)
      title <- "Distribución Weibull"
      
    } else if (input$distribucion == "Exponencial") {
      x <- seq(0, 30, length.out = 100) # Ajusta el rango según sea necesario
      prob <- dexp(x, rate = input$lambda_exp)
      data <- data.frame(x, prob)
      title <- "Distribución Exponencial"
      
    } else if (input$distribucion == "Chi-Cuadrada") {
      x <- seq(0, 30, length.out = 100)
      prob <- dchisq(x, df = input$df)
      data <- data.frame(x, prob)
      title <- "Distribución Chi-Cuadrada"
      
    } else if (input$distribucion == "t de Student") {
      x <- seq(-4, 4, length.out = 100)
      prob <- dt(x, df = input$df_t)
      data <- data.frame(x, prob)
      title <- "Distribución t de Student"
      
    } else if (input$distribucion == "F") {
      x <- seq(0, 5, length.out = 100) # Ajusta el rango según sea necesario
      prob <- df(x, df1 = input$df1, df2 = input$df2)
      data <- data.frame(x, prob)
      title <- "Distribución F"
    }
    
    # Crear el gráfico
    output$distPlot <- renderPlot({
      ggplot(data, aes(x = x, y = prob)) +
        geom_line() +
        labs(title = title, x = "Valor", y = "Densidad de Probabilidad") +
        theme_minimal()
    })
  })
  
  observeEvent(input$calc_prob, {
    req(input$distribucion)
    prob_value <- NULL
    
    if (input$distribucion == "Binomial") {
      prob_value <- dbinom(input$x_bin, size = input$n, prob = input$p)
      
    } else if (input$distribucion == "Poisson") {
      prob_value <- dpois(input$x_poisson, lambda = input$lambda)
      
    } else if (input$distribucion == "Hipergeométrica") {
      prob_value <- dhyper(input$x_hyper, m = input$K, n = input$N - input$K, k = input$n_hyper)
      
    } else if (input$distribucion == "Binomial Negativa") {
      prob_value <- dnbinom(input$x_neg, size = input$r, prob = input$p_neg)
      
    } else if (input$distribucion == "Geométrica") {
      prob_value <- dgeom(input$x_geom, prob = input$p_geom)
      
    } else if (input$distribucion == "Normal") {
      prob_value <- dnorm(input$x_norm, mean = input$mu, sd = input$sigma)
      
    } else if (input$distribucion == "Uniforme") {
      prob_value <- dunif(input$x_unif, min = input$a, max = input$b)
      
    } else if (input$distribucion == "Beta") {
      prob_value <- dbeta(input$x_beta, shape1 = input$alpha, shape2 = input$beta)
      
    } else if (input$distribucion == "Gamma") {
      prob_value <- dgamma(input$x_gamma, shape = input$k, scale = input$theta)
      
    } else if (input$distribucion == "Weibull") {
      prob_value <- dweibull(input$x_weibull, shape = input$shape, scale = input$scale)
      
    } else if (input$distribucion == "Exponencial") {
      prob_value <- dexp(input$x_exp, rate = input$lambda_exp)
      
    } else if (input$distribucion == "Chi-Cuadrada") {
      prob_value <- dchisq(input$x_chisq, df = input$df)
      
    } else if (input$distribucion == "t de Student") {
      prob_value <- dt(input$x_t, df = input$df_t)
      
    } else if (input$distribucion == "F") {
      prob_value <- df(input$x_f, df1 = input$df1, df2 = input$df2)
    }
    
    output$probabilidad <- renderText({
      paste("La probabilidad calculada es:", round(prob_value, 5))
    })
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)

    
               