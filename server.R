
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

shinyServer(function(input, output) {
  
  # gather the data and make it nice
  inputData <- reactive({
    tibble(x = input$x_corr %>%
             str_trim() %>%
             str_split('(\n|,|[:space:])+') %>%
             unlist() %>%
             as.numeric() %>%
             na.omit(),
           y = input$y_corr %>%
             str_trim() %>%
             str_split('(\n|,|[:space:])+') %>%
             unlist() %>%
             as.numeric() %>%
             na.omit()
    )})
  # create a reactive values group
  v <- reactiveValues(
    pearsonr = NULL,
    xmedian = NULL,
    ymedian = NULL,
    xmean = NULL,
    ymean = NULL,
    stdev = NULL
    )


# Bootstrap ---------------------------------------------------------------

  # when the button is hit, 
  observeEvent(input$runCorr, {
    # reset pearsonr
    v$pearsonr <- NULL
    # reset medians
    v$xmedian <- NULL
    v$ymedian <- NULL
    # bootstrap it
    for (iter in 1:input$n_samples_corr){
      index <- sample(nrow(inputData()), nrow(inputData()), replace = TRUE)
      v$pearsonr <- c(v$pearsonr, cor(inputData()$x[index], inputData()$y[index], method='pearson'))
      # v$xmedian <- c(v$xmedian, median(inputData()$x[index]))
      # v$ymedian <- c(v$ymedian, median(inputData()$y[index]))
      v$xmean <- c(v$xmean, mean(inputData()$x[index]))
      v$ymean <- c(v$ymean, mean(inputData()$y[index]))
      v$xsd <- c(v$xsd, sd(inputData()$x[index]))
      v$ysd <- c(v$ysd, sd(inputData()$y[index]))
    }
  })

# Correlation coefficient -------------------------------------------------
    
  output$correlationHistogram <- renderPlot({
    if (is.null(v$pearsonr)) {return()}
    p <- ggplot(data=NULL, aes(x=v$pearsonr)) +
      # full histogram
      geom_histogram(bins=200, alpha=0.7) +
      # histogram of only inside 95% ci
      geom_histogram(aes(x=v$pearsonr[v$pearsonr < quantile(v$pearsonr, 0.975) & v$pearsonr > quantile(v$pearsonr, 0.025)]),
                     bins=200, fill='red', alpha=0.7) +
      # geom_density(color='blue')
      labs(x = "Pearson r", y = "Density") +
      geom_vline(xintercept=mean(v$pearsonr)) +
      geom_vline(xintercept=cor(inputData()$x, inputData()$y, method='pearson'), color='red')
    p
  })
  output$scatterPlot <- renderPlot({
    # Plot a scatterplot
    inputData() %>%
      ggplot(aes(x=x, y=y, color=x)) +
      geom_point(size=2) +
      viridis::scale_color_viridis() +
      geom_smooth(method=lm, color='gray')
  })

# Means -------------------------------------------------------------------

  output$xMean <- renderPlot({
    if (is.null(v$xmean)) {return()}
    # isolate data
    dat <- v$xmean
    # plot histogram
    p <- ggplot(data=NULL, aes(x=dat)) +
      geom_histogram(bins=200, alpha=0.7) +
      geom_histogram(aes(x=dat[dat < quantile(dat, 0.975) & dat > quantile(dat, 0.025)]),
                     bins=200, alpha=0.7, fill='red') +
      labs(x = "Mean of X", y = "Density") +
      geom_vline(xintercept=mean(v$xmean)) +
      geom_vline(xintercept=mean(inputData()$x), color='red')
    p
  })
  
  output$yMean <- renderPlot({
    if (is.null(v$ymean)) {return()}
    # isolate data
    dat <- v$ymean
    # plot histogram
    p <- ggplot(data=NULL, aes(x=dat)) +
      geom_histogram(bins=200, alpha=0.7) +
      geom_histogram(aes(x=dat[dat < quantile(dat, 0.975) & dat > quantile(dat, 0.025)]),
                     bins=200, alpha=0.7, fill='red') +
      labs(x = "Mean of Y", y = "Density") +
      geom_vline(xintercept=mean(v$ymean)) +
      geom_vline(xintercept=mean(inputData()$y), color='red')
    p
  })
  

# Standard Deviation ------------------------------------------------------

  output$xSd <- renderPlot({
    if (is.null(v$xsd)) {return()}
    # isolate data
    dat <- v$xsd
    # plot histogram
    p <- ggplot(data=NULL, aes(x=dat)) +
      geom_histogram(bins=200, alpha=0.7) +
      geom_histogram(aes(x=dat[dat < quantile(dat, 0.975) & dat > quantile(dat, 0.025)]),
                     bins=200, alpha=0.7, fill='red') +
      labs(x = "SD(X)", y = "Density") +
      geom_vline(xintercept=mean(dat)) +
      geom_vline(xintercept=sd(inputData()$x), color='red')
    p
  })
  
  output$ySd <- renderPlot({
    if (is.null(v$ysd)) {return()}
    # isolate data
    dat <- v$ysd
    # plot histogram
    p <- ggplot(data=NULL, aes(x=dat)) +
      geom_histogram(bins=200, alpha=0.7) +
      geom_histogram(aes(x=dat[dat < quantile(dat, 0.975) & dat > quantile(dat, 0.025)]),
                     bins=200, alpha=0.7, fill='red') +
      labs(x = "SD(Y)", y = "Density") +
      geom_vline(xintercept=mean(dat)) +
      geom_vline(xintercept=sd(inputData()$y), color='red')
    p
  })
  

})
