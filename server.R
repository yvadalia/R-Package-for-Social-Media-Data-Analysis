
library(shiny)
library(tm)
library(wordcloud)
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(forecast)

# Declare Twitter API Credentials
api_key <- "BmOC3DSqY1DnmTumWpT3UYnly"
api_secret <- "oUBV8UpMN9TMlcDfM3cYIpTZMTil7QEOaZQImb4eEI8VuONAMn"
token <- "715198199103930369-fLjli3SnM22jkLk3hFT1DPyiKcYHMMz" 
token_secret <- "xtCKxFSLdmijJedTcWwyf0IgUMQyzjPKXZid7Bax4Up4O" 

shinyServer(
  function(input, output, session) {
    # Create Twitter Connection
    setup_twitter_oauth(api_key, api_secret, token, token_secret)
    
    
    selectedData <- reactive({input$keyword})
    #get data from twitter
    output$distPlot <- renderPlot({  
      startDate <- as.character(Sys.Date()-1)
      endDate <- as.character(Sys.Date())
      list <- searchTwitter(selectedData(), n=5, since =startDate, until = endDate)
      df <- twListToDF(list)
      df <- df[, order(names(df))]
      df$created <- strftime(df$created, '%Y-%m-%d:%H')
      if (file.exists(paste('datafiles/',selectedData(),'_twitterdata.csv'))==FALSE) write.csv(df, file=paste('datafiles/',selectedData(),'_twitterdata.csv'), row.names=F)
      
      #update CSV file with new data exclusing duplicates
      twitterdata <- read.csv(file=paste('datafiles/',selectedData(),'_twitterdata.csv'))
      twitterdata <- rbind(twitterdata, df)
      twitterdata <- subset(twitterdata, !duplicated(twitterdata$text))
      write.csv(twitterdata, file=paste('datafiles/',selectedData(),'_twitterdata.csv'), row.names=F)
      
      #initialte positive and negative word list
      pos <- scan('sentiment-lexicon/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
      neg <- scan('sentiment-lexicon/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
      
      
      Dataset <- twitterdata
      Dataset$text <- as.factor(Dataset$text)
      sentimentScores <- score.sentiment(Dataset$text, Dataset$created, pos, neg, .progress='text')
      write.csv(sentimentScores, file=paste('datafiles/',selectedData(), '_sentimentScores.csv'), row.names=TRUE) #save evaluation results
      
      forecastScores <- group_by(sentimentScores, created)
      forecastScores <- aggregate(forecastScores$score, by=list(created=forecastScores$created), FUN=sum)
      write.csv(forecastScores, file=paste('datafiles/',selectedData(), '_forecastScores.csv'), row.names=TRUE) #save evaluation results
      
      #total score calculation: positive / negative
      statistics <- sentimentScores
      statistics$created <- twitterdata$created
      statistics <- mutate(statistics, tweet=ifelse(statistics$score > 0, 'positive', ifelse(statistics$score < 0, 'negative', 'neutral')))
      by.tweet <- group_by(statistics, tweet, created)
      by.tweet <- summarise(by.tweet, number=n())
      by.tweet <- filter(by.tweet, tweet=="positive" | tweet =="negative" )
      write.csv(by.tweet, file=paste('datafiles/',selectedData(), '_tweetCount.csv'), row.names=TRUE)
      
      #chart
      ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=1) +
        geom_point(aes(group=tweet, color=tweet), size=1) +
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) +
        stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
        ggtitle(selectedData())
      
    })
    #Time Series Plot
    observeEvent(input$timeSeries, { 
      forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
      tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=24)
      
      output$timeSeriesPlot <- reactive({
        output$plotTs <- renderPlot({  
          plot(tweetScore.ts)
        })
        output$timeSeriesPlot <- reactive({
          outText <- "Time Series Plot"
        })
        output$acfTimeSeriesPlot <- reactive({
          output$plotAcfTs <- renderPlot({  
            # plot the time series
            acf(tweetScore.ts)
          })
          outText <- "ACF Time Series Plot"
        })
        output$pacfTimeSeriesPlot <- reactive({
          output$plotPacfTs <- renderPlot({  
            # plot the time series
            pacf(tweetScore.ts)
          })
          outText <- "PACF Time Series Plot"
        })
        
      })
  })
    
    #Simple Forecast
    observeEvent(input$simpleForecast, { 
      forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
      tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=8640)
      tweetScore.simple <- ses( 
        x = tweetScore.ts,      
        h = input$forecasting_horizon*24,             
        level = 95,     
        fan = FALSE,    
        alpha = 0.2,    
        initial = "simple"
      )    
      
      output$timeSeriesPlot <- reactive({
        output$plotTs <- renderPlot({  
          plot(tweetScore.simple)
        })
        output$timeSeriesPlot <- reactive({
          outText <- "Time Series Plot"
        })
        output$acfTimeSeriesPlot <- reactive({
          output$plotAcfTs <- renderPlot({  
            # plot the time series
            acf(tweetScore.simple$residuals)
          })
          outText <- "ACF Time Series Plot"
        })
        output$pacfTimeSeriesPlot <- reactive({
          output$plotPacfTs <- renderPlot({  
            # plot the time series
            pacf(tweetScore.simple$residuals)
          })
          outText <- "PACF Time Series Plot"
        })
        
      })
    })
    
    #Additive Holt Winters Forecast
    observeEvent(input$additiveHoltWintersForecast, { 
      forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
      tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=24)
      tweetScore.winters<-hw(              
        x = tweetScore.ts,           
        h = input$forecasting_horizon*24,            
        seasonal = "additive",
        level=c(input$PI_s,input$PI_l),       
        fan=FALSE,            
        initial = "simple",   
        exponential=FALSE,    
        alpha=NULL            
      )
      
      output$timeSeriesPlot <- reactive({
        output$plotTs <- renderPlot({  
          plot(tweetScore.winters)
        })
        output$timeSeriesPlot <- reactive({
          outText <- "Time Series Plot"
        })
        output$acfTimeSeriesPlot <- reactive({
          output$plotAcfTs <- renderPlot({  
            # plot the time series
            acf(tweetScore.winters$residuals)
          })
          outText <- "ACF Time Series Plot"
        })
        output$pacfTimeSeriesPlot <- reactive({
          output$plotPacfTs <- renderPlot({  
            # plot the time series
            pacf(tweetScore.winters$residuals)
          })
          outText <- "PACF Time Series Plot"
        })
        
      })
    })
    
#     #Multiplicative Hot Winters Forecast - Will not work because of negative value in the score
#     observeEvent(input$multiplicativeHotWintersForecast, { 
#       forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
#       tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=24)
#       tweetScore.winters<-hw(              
#         x = tweetScore.ts,           
#         h = 3*24,            
#         seasonal = "multiplicative",
#         level=c(80,95),       
#         fan=FALSE,            
#         initial = "simple",   
#         exponential=FALSE,    
#         alpha=NULL            
#       )
#       
#       output$timeSeriesPlot <- reactive({
#         output$plotTs <- renderPlot({  
#           plot(tweetScore.winters)
#         })
#         output$timeSeriesPlot <- reactive({
#           outText <- "Time Series Plot"
#         })
#         output$acfTimeSeriesPlot <- reactive({
#           output$plotAcfTs <- renderPlot({  
#             # plot the time series
#             acf(tweetScore.winters$residuals)
#           })
#           outText <- "ACF Time Series Plot"
#         })
#         output$pacfTimeSeriesPlot <- reactive({
#           output$plotPacfTs <- renderPlot({  
#             # plot the time series
#             pacf(tweetScore.winters$residuals)
#           })
#           outText <- "PACF Time Series Plot"
#         })
#         
#       })
#     })
    
    #Arima Forecast
    observeEvent(input$arimaForecast, { 
      forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
      tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=24)
      tweetScore.arima<-Arima(
        x = tweetScore.ts,
        order = c(input$ppp,input$ddd,input$qqq), 
        seasonal=c(input$PPP,input$DDD,input$QQQ),
        transform.pars=TRUE, 
        fixed=NULL, 
        init=NULL, 
        method=c("CSS-ML","ML","CSS"), 
        kappa=1e6)
      
      output$timeSeriesPlot <- reactive({
        output$plotTs <- renderPlot({  
          plot(forecast(tweetScore.arima,level=c(input$PI_s,input$PI_l)))
        })
        output$timeSeriesPlot <- reactive({
          outText <- "Time Series Plot"
        })
        output$acfTimeSeriesPlot <- reactive({
          output$plotAcfTs <- renderPlot({  
            # plot the time series
            acf(tweetScore.arima$residuals)
          })
          outText <- "ACF Time Series Plot"
        })
        output$pacfTimeSeriesPlot <- reactive({
          output$plotPacfTs <- renderPlot({  
            # plot the time series
            pacf(tweetScore.arima$residuals)
          })
          outText <- "PACF Time Series Plot"
        })
        
      })
    })
    
    #Auto Arima Forecast
    observeEvent(input$autoArimaForecast, { 
      forecastScores <- read.csv(file=paste('datafiles/',selectedData(), '_forecastScores.csv'))
      tweetScore.ts<-ts(forecastScores$x,start=c(2016,4,10,0),freq=24)
      tweetScore.auto.arima<-auto.arima(
        x = tweetScore.ts,
        d=NA, 
        D=NA, 
        max.p=5, 
        max.q=5,
        max.P=2, 
        max.Q=2, 
        max.order=5,
        max.d=2,    
        max.D=1,    
        start.p=2,       
        start.q=2, 
        start.P=1, 
        start.Q=1,
        stationary=FALSE, 
        ic=c("aicc", "aic", "bic"), 
        stepwise=TRUE, 
        test=c("kpss","adf","pp"), 
        seasonal.test=c("ocsb","ch"),
        parallel=FALSE,num.cores=2)
      
      output$timeSeriesPlot <- reactive({
        output$plotTs <- renderPlot({  
          plot(forecast(tweetScore.auto.arima))
        })
        output$timeSeriesPlot <- reactive({
          outText <- "Time Series Plot"
        })
        output$acfTimeSeriesPlot <- reactive({
          output$plotAcfTs <- renderPlot({  
            # plot the time series
            acf(tweetScore.auto.arima$residuals)
          })
          outText <- "ACF Time Series Plot"
        })
        output$pacfTimeSeriesPlot <- reactive({
          output$plotPacfTs <- renderPlot({  
            # plot the time series
            pacf(tweetScore.auto.arima$residuals)
          })
          outText <- "PACF Time Series Plot"
        })
        
      })
    })
})
    
   
  
#Calculate Sentiment based on positive and negative words
score.sentiment <- function(sentences, created, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  sentimentScores <- laply(sentences, function(sentence, pos.words, neg.words){
    #clean code
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", sentence)
    sentence <- gsub("@\\w+", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("[[:punct:]]", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("[[:digit:]]", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("http\\w+", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("[ \t]{2,}", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("^\\s+|\\s+$", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("[[:cntrl:]]", " ", sentence, ignore.case=TRUE)
    sentence <- gsub("\\d+", " ", sentence, ignore.case=TRUE)
    
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  sentimentScores.df <- data.frame(score=sentimentScores, text=sentences, created=created)
  return(sentimentScores.df)
}
  
