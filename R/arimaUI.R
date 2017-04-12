#' ARIMA parameter picker
#'
#' Gadget allowing to choose easily the parameter of an ARIMA model.
#' Some help to the decision making process is displayed as wikipedia link
#'
#' @param ts.ar A time series
#' @return
#' The function open a viewer panel.
#' It allows to choose the arima parameters.
#' The value returned is a list with the arima parameter and the coefficients of a fit of the time serie.
#'
#' @export
#'
#' @examples
#' sim.ts <- arima.sim(
#' n = 500,
#' model = list(
#'   order = c(1, 0, 2),
#'   ar = 0.8
#' , ma = c(0.2, 0.5)
#' ))
#'
#' list.res <- arimaUI(sim.ts)
#'
#' # comparison of manual selection with automatic one
#' library(forecast)
#' auto.arima(sim.ts)
#'
#' @note
#' The Ljung-Box tab panel come from an article of http://alstatr.blogspot.co.uk/.
#' It had been kindly made available by @alstated.
arimaUI <- function(ts.ar) {

  # ui part
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Arima parameter picker"),
    miniUI::miniTabstripPanel(
      # Raw plot tab: Line chart of the time serie
      miniUI::miniTabPanel( "Raw plot", icon = shiny::icon("area-chart"),
                            miniUI::miniContentPanel(
                              shiny::plotOutput("plot.ts")
                            )
      )
      , miniUI::miniTabPanel("Identification", icon = shiny::icon("key"),
                             miniUI::miniTabstripPanel(
                               miniUI::miniTabPanel("ACF",
                                                    miniUI::miniContentPanel(
                                                      # source of the code: analysis with programming http://alstatr.blogspot.co.uk
                                                      shiny::a("ACF: correlation between values of the process at different times",
                                                        href="https://en.wikipedia.org/wiki/Autocorrelation",
                                                        target = "_blank"),
                                                      shiny::plotOutput("plot.acf", height = "100%")
                                                    )
                               ),
                               miniUI::miniTabPanel("PACF",
                                                    miniUI::miniContentPanel(
                                                      shiny::a("PACF: Given a time series z(t), the partial autocorrelation of lag k, is the autocorrelation between z(t) and z(t+k) with the linear dependence of z(t) on z(t+1) through z(t+k-1) removed",
                                                        href="https://en.wikipedia.org/wiki/Partial_autocorrelation_function",
                                                        target = "_blank"),
                                                      shiny::plotOutput("plot.pacf", height = "90%")
                                                    )
                               )
                             )
      )
      , miniUI::miniTabPanel("Parameter picker", icon = shiny::icon("gears"),
                             miniUI::miniContentPanel(
                               shiny::sliderInput(
                                 inputId = "ar",
                                 label = "Autoregressive",
                                 min = 0,
                                 max = 2,
                                 value = 1)
                               ,shiny::sliderInput(
                                 inputId = "df",
                                 label = "Differencing",
                                 value = 0,
                                 min = 0,
                                 max = 2)
                               , shiny::sliderInput(
                                 inputId = "ma",
                                 label = "Moving-Average",
                                 min = 0,
                                 max = 2,
                                 value = 1)
                               , shiny::htmlOutput("fit.text2")
                             )
      )
      , miniUI::miniTabPanel("Fit", icon = shiny::icon("line-chart"),
                             miniUI::miniContentPanel(padding = 5,
                                                      shiny::htmlOutput("fit.text")
                                                      , shiny::plotOutput("fit.res", height = "65%")
                             )
      )
      , miniUI::miniTabPanel("Diagnostic", icon = shiny::icon("user-md"),
                             miniUI::miniTabstripPanel(
                               miniUI::miniTabPanel("Residus",
                                                    miniUI::miniContentPanel(
                                                      shiny::plotOutput("fit.residus")
                                                    )
                               )
                               , miniUI::miniTabPanel("Residus Lag plot",
                                                      miniUI::miniContentPanel(
                                                        shiny::plotOutput("fit.lag", height = "80%"),
                                                        shiny::sliderInput(inputId = "lag", label = "Lag", min = 1, max = 12, value = 1)
                                                      )
                               )
                               , miniUI::miniTabPanel("Ljung-Box test",
                                                      miniUI::miniContentPanel(
                                                        shiny::helpText(shiny::strong("HYPOTHESES:"),"from ",
                                                                        shiny::a("Wikipedia",
                                                                                 href="http://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test"),
                                                                        shiny::br(),
                                                                        shiny::strong("H0:"),
                                                                        "The data are independently distributed (i.e. the correlations in
                                       the population from which the sample is taken are 0, so that any
                                       observed correlations in the data result from randomness of the
                                       sampling process.", shiny::br()
                                                                        , shiny::strong("H1:"),
                                                                        "The data are not independently distributed.",shiny::br(),shiny::br(),shiny::br(),
                                                                        shiny::strong("COMPUTATION:"),shiny::br(),
                                                                        "The computed statistics is:",
                                                                        shiny:: textOutput("diagx2"),
                                                                        "The computed p-value is:",
                                                                        shiny:: textOutput("diagpv"),shiny::br(),shiny::br(),
                                                                        shiny::strong("DECISION:"),shiny:: textOutput("diagdc"),shiny::br(),shiny::br(),
                                                                        shiny::strong("CONCLUSION:"),shiny:: textOutput("diagcn")
                                                        )
                                                      )
                               )
                             )
      )
    )
  )

  server <- function(input, output, session) {

    # Render the time serie line chart
    output$plot.ts <- shiny::renderPlot({
      plot(ts.ar, main = "My time serie", ylab = "Frequency")
    })

    # Render the ACF graph
    output$plot.acf <- shiny::renderPlot({
      acf.res <- acf(ts.ar, plot = F)
      plot(acf.res, main = "")
    })

    # Render the PACF graph
    output$plot.pacf <- shiny::renderPlot({
      pacf.res <- pacf(ts.ar, plot = F)
      plot(pacf.res, main = "")
    })

    # Estimation of the function
    estimate <- shiny::reactive({
      ar <- input$ar ; df <- input$df ; ma <- input$ma
      k <- arima(
        ts.ar,
        order = c(ar, df, ma))
    })

    # Render the output of the arima function
    output$fit.text <- shiny::renderUI({
      k <- estimate()
      HTML(gsub(" ", "&nbsp;", paste(capture.output(k)[-c(1, 4)], collapse = "<br/>")))
    })

    # Render the output of the arima function (2)
    output$fit.text2 <- shiny::renderUI({
      k <- estimate()
      HTML(gsub(" ", "&nbsp;", paste(capture.output(k)[-1], collapse = "<br/>")))
    })

    # Render the residus plot
    output$fit.residus <- shiny::renderPlot({
      k <- estimate()
      plot(k$residuals, type = "h", ylab = "Residus", xlab = "Time"
           , main = "Residus over time")
      abline(h = 0)
    })

    # render the residus lag plot
    output$fit.lag <- shiny::renderPlot({
      lag <- input$lag

      length.ts <- length(estimate()$residuals)
      lag.ts <- data.frame(ts = estimate()$residuals[(lag+1):length.ts]
                           , lag.ts = estimate()$residuals[1:(length.ts-lag)])
      plot(x = lag.ts$lag.ts, lag.ts$ts, ylab = "Residus"
           , xlab = paste0("lag", lag))
    })

    # Render the time series graph + the fitted values
    output$fit.res <- shiny::renderPlot({
      fit.pred.res <- estimate()$residuals + as.numeric(ts.ar)
      plot(ts.ar, col = "blue")
      lines(fit.pred.res, col = "red")
    })

    # Render the test
    output$diagx2 <- shiny::renderPrint({
      m <- Box.test(estimate()$residuals, type = "Ljung-Box")
      as.numeric(as.matrix(m$statistic))
    })

    # Render the test
    output$diagpv <- shiny::renderPrint({
      m <- Box.test(estimate()$residuals, type = "Ljung-Box")
      as.numeric(as.matrix(m$p.value))
    })

    # Render the test
    output$diagdc <- shiny::renderPrint({
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
    output$diagcn <- shiny::renderPrint({
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
    shiny::observeEvent(input$done, {
      k <- estimate()
      shiny::stopApp(returnValue = list(ar = input$ar, df = input$df, ma = input$ma
                                        , coefs = data.frame(k$coef)))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("myGadgetFunc"))
}

#' @import rstudioapi
arima_addin = function(){

  selected_text = rstudioapi::getActiveDocumentContext()$selection[[1]]$text
  ts.ar <- eval(parse(text = selected_text))
  arimaUI::arimaUI(ts.ar)

}
