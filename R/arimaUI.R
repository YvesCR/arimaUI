#' Gadget allowing to choose easily the parameter of an ARIMA model.
#' Some help to the decision making process is displayed as wikipedia link

#' @param ts.ar A time series
#' @return The function open a viewer panel. It allows to choose the arima parameters. The value return is a list with the arima parameter and the coefficients of a fit of the time serie.

#' @example 
#' sim.ts <- arima.sim(
#' n = 500, 
#' model = list(
#'   order = c(1, 0, 2), 
#'   ar = 0.8
#' , ma = c(0.2, 0.5)
#' ))
#'
#' list.res <- arimaUI(sim.ts)
#' #' # Comparison with auto.arima from the forecast package:
#' #' 
#' library(forecast)
#' auto.arima(sim.ts)

arimaUI <- function(ts.ar) { 
  
# ui part
  ui <- miniPage(
    gadgetTitleBar("Arima parameter picker"),
      miniTabstripPanel(
      # Raw plot tab: Line chart of the time serie
        miniTabPanel( "Raw plot", icon = icon("area-chart"), 
          miniContentPanel(
            plotOutput("plot.ts")
          )
        )
      , miniTabPanel("Identification", icon = icon("key"), 
        miniTabstripPanel(
          miniTabPanel("ACF",
            miniContentPanel(
              # source of the code: analysis with programming http://alstatr.blogspot.co.uk
              a("ACF: correlation between values of the process at different times",
                href="https://en.wikipedia.org/wiki/Autocorrelation",
                target = "_blank"),
              plotOutput("plot.acf", height = "100%")
            )
          ),
          miniTabPanel("PACF",
            miniContentPanel(
              a("PACF: Given a time series z(t), the partial autocorrelation of lag k, is the autocorrelation between z(t) and z(t+k) with the linear dependence of z(t) on z(t+1) through z(t+k-1) removed",
                href="https://en.wikipedia.org/wiki/Partial_autocorrelation_function",
                target = "_blank"),
              plotOutput("plot.pacf", height = "90%")
            )
          )
        )
      )
      , miniTabPanel("Parameter picker", icon = icon("gears"), 
        miniContentPanel(
          sliderInput(
            inputId = "ar",
            label = "Autoregressive",
            min = 0,
            max = 2,
            value = 1)
          ,sliderInput(
            inputId = "df",
            label = "Differencing",
            value = 0,
            min = 0,
            max = 2)
          , sliderInput(
            inputId = "ma",
            label = "Moving-Average",
            min = 0,
            max = 2,
            value = 1)
          , htmlOutput("fit.text2")
        )
      )
      , miniTabPanel("Fit", icon = icon("line-chart"), 
        miniContentPanel(padding = 5,
          htmlOutput("fit.text")
          , plotOutput("fit.res", height = "65%")
        )
      )
      , miniTabPanel("Diagnostic", icon = icon("user-md"), 
        miniTabstripPanel(
          miniTabPanel("Residus",
            miniContentPanel(
              plotOutput("fit.residus")
            )
          )
          , miniTabPanel("Residus Lag plot",
            miniContentPanel(
              plotOutput("fit.lag", height = "80%"),
              sliderInput(inputId = "lag", label = "Lag", min = 1, max = 12, value = 1)
            )
          )
          , miniTabPanel("Ljung-Box test",
            miniContentPanel(
              helpText(strong("HYPOTHESES:"),"from ",
              a("Wikipedia",
                href="http://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test"),
                br(),
                strong("H0:"),
                "The data are independently distributed (i.e. the correlations in
                  the population from which the sample is taken are 0, so that any
                  observed correlations in the data result from randomness of the
                  sampling process.",br()
                , strong("H1:"),
                "The data are not independently distributed.",br(),br(),br(),
                strong("COMPUTATION:"),br(),
                "The computed statistics is:",
                textOutput("diagx2"),
                "The computed p-value is:",
                textOutput("diagpv"),br(),br(),
                strong("DECISION:"),textOutput("diagdc"),br(),br(),
                strong("CONCLUSION:"),textOutput("diagcn")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    
    # Render the time serie line chart
    output$plot.ts <- renderPlot({
      plot(ts.ar, main = "My time serie", ylab = "Frequency")
    })
    
    # Render the ACF graph
    output$plot.acf <- renderPlot({
      acf.res <- acf(ts.ar, plot = F)
      plot(acf.res, main = "")
    })
    
    # Render the PACF graph
    output$plot.pacf <- renderPlot({
      pacf.res <- pacf(ts.ar, plot = F)
      plot(pacf.res, main = "")
    })
    
    # Estimation of the function
    estimate <- reactive({
      ar <- input$ar ; df <- input$df ; ma <- input$ma
      k <- arima(
        ts.ar,
        order = c(ar, df, ma))
    })
    
    # Render the output of the arima function
    output$fit.text <- renderUI({
      k <- estimate()
      HTML(gsub(" ", "&nbsp;", paste(capture.output(k)[-c(1, 4)], collapse = "<br/>")))
    })
    
    # Render the output of the arima function (2)
    output$fit.text2 <- renderUI({
      k <- estimate()
      HTML(gsub(" ", "&nbsp;", paste(capture.output(k), collapse = "<br/>")))
    })
    
    # Render the residus plot
    output$fit.residus <- renderPlot({ 
      k <- estimate()
      plot(k$residuals, type = "h", ylab = "Residus", xlab = "Time"
           , main = "Residus over time")
      abline(h = 0)
    })
    
    # render the residus lag plot
    output$fit.lag <- renderPlot({
      lag <- input$lag
      
      length.ts <- length(estimate()$residuals)
      lag.ts <- data.frame(ts = estimate()$residuals[(lag+1):length.ts]
                           , lag.ts = estimate()$residuals[1:(length.ts-lag)])
      plot(x = lag.ts$lag.ts, lag.ts$ts, ylab = "Residus"
           , xlab = paste0("lag", lag))
    })
    
    # Render the time series graph + the fitted values
    output$fit.res <- renderPlot({
      fit.pred.res <- estimate()$residuals + as.numeric(ts.ar)
      plot(ts.ar, col = "blue")
      lines(fit.pred.res, col = "red")
    })
    
    # Render the test
    output$diagx2 <- renderPrint({
      m <- Box.test(estimate()$residuals, type = "Ljung-Box")
      as.numeric(as.matrix(m$statistic))
    })
    
    # Render the test
    output$diagpv <- renderPrint({
      m <- Box.test(estimate()$residuals, type = "Ljung-Box")
      as.numeric(as.matrix(m$p.value))
    })
    
    # Render the test
    output$diagdc <- renderPrint({
      m <- Box.test(estimate()$residuals, type = "Ljung-Box")
      j <- as.numeric(as.matrix(m$p.value))
      if(j >= 0.05){
        print("Do not reject the null hypothesis, since the p-value is greater than 0.05")
      }
      if(j < 0.05){
        print("Reject the null hypothesis, since the p-value is less than 0.05")
      }
    })
    
    # Render the test
    output$diagcn <- renderPrint({
      ar <- input$ar ; df <- input$df ; ma <- input$ma
      fit <- arima(
        ts.ar, order = c(ar, df, ma),
        include.mean = FALSE)
      m <- Box.test(fit$residuals, type = "Ljung-Box")
      j <- as.numeric(as.matrix(m$p.value))
      if(j >= 0.05){
        print("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the residuals of the model exhibits randomness")
      }
      if(j < 0.05){
        print("Therefore, the residuals of the model are not independently distributed.")
      }
    })
    
    # Handle the Done button being pressed
    observeEvent(input$done, {
      k <- estimate()
      stopApp(returnValue = list(ar = input$ar, df = input$df, ma = input$ma
                                 , coefs = data.frame(k$coef)))
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("myGadgetFunc"))
}
