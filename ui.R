shinyUI(fluidPage( 
  sidebarLayout(
    sidebarPanel(
      br(), br(),
      h4("Choose Brand name from the list:"), br(),
      selectInput('keyword', 'Brand name', 
        c("Costco" = "costco",
          "Lowes" = "lowes",
          "Safeway" = "safeway",
          "Target" = "target",
          "Kroger" = "kroger",
          "Walmart" = "walmart",
          "Sprouts" = "sprouts"
        )
      ),
      br(),
      h4("Click to view the Time series plot"), br(),
      actionButton("timeSeries", "Time Series"), br(),br (),
      h4("Specify Desired Forecast Parameter Values"), br(),
      numericInput('forecasting_horizon', 'Forecasting Horizon', 3, min = 1, max = 10),
      numericInput("PI_l", "Prediction Interval (wide)", 95), 
      numericInput("PI_s", "Prediction Interval (narrow)", 80),br(),br(),
      h4("Choose a Forecasting Model"),br(),
      actionButton("simpleForecast", "Simple Smoothing Method"), br(),br(),
      actionButton("additiveHoltWintersForecast", "Holt Winters Method"), br(),br(),
      actionButton("arimaForecast", "ARIMA Model"),
      conditionalPanel(
        condition <- "input.arimaForecast > 0",
        br(),
        numericInput('ppp', '(p,_,_)(_,_,_)', 0, min = 0, max = 2),
        numericInput('ddd', '(_,d,_)(_,_,_)', 1, min = 0, max = 1),
        numericInput('qqq', '(_,_,q)(_,_,_)', 0, min = 0, max = 2),
        numericInput('PPP', '(_,_,_)(P,_,_)', 1, min = 0, max = 1),
        numericInput('DDD', '(_,_,_)(_,D,_)', 1, min = 0, max = 2),
        numericInput('QQQ', '(_,_,_)(_,_,Q)', 0, min = 0, max = 2)
      ),
      br(),br(),
      actionButton("autoArimaForecast", "Auto ARIMA Model")
    ),
    mainPanel(
      titlePanel("Sentiment Analysis and Forecast"), 
      plotOutput("distPlot"),br(),
      textOutput("timeSeriesPlot"),br(),
      plotOutput("plotTs"),
      textOutput("acfTimeSeriesPlot"),br(),
      plotOutput("plotAcfTs"),
      textOutput("pacfTimeSeriesPlot"),br(),
      plotOutput("plotPacfTs")
      )
    )
)
)