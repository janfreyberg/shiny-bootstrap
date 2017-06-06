
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readr)

data <- read_csv('rest.csv')
init_x <- paste0(data$age, collapse = ', ')
init_y <- paste0(data$average.slope, collapse = ', ')

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bootstrapping statistics"),
  fluidRow(
    column(10,
           p("This app is for experimenting how many bootstrapped samples you need to get a good estimate of the variation in ",
             "summary stats such as a correlation coefficient, a mean, or a standard deviation.",
             "Please note that bootstrapping is quite a slow procedure, so it can take a while ",
             "once you hit the 'Bootstrap! button."),
           p("Find the source code ", a(href="http://github.com/janfreyberg/shiny-bootstrap", "here.")),
           p("To help you get started, I have added some dummy data below, but paste your own into the text fields."))
  ),
  hr(),
  fluidRow(
    column(3, sliderInput("n_samples_corr",
                          "Number of bootstrap samples:",
                          min = 50,
                          max = 5000,
                          value = 1000)),
    column(3, textAreaInput("x_corr",
                            "Variable 1 (x):",
                            value=init_x,
                            width='100%')),
    column(3, textAreaInput("y_corr",
                            "Variable 2 (y):",
                            value=init_y,
                            width='100%')),
    column(1, br(), actionButton("runCorr", "Bootstrap!"))
  ),
  # Tab for bootstrapping correlations
  tabsetPanel(
    # the panel for bootstrapped medians
    tabPanel('Pearson r',
             p("Values inside the bootstrapped confidence interval ",
               "of this estimate are shown in red.",
               "Try different numbers for the number of samples to try ",
               "and decide when the bootstrapped distribution of correlation ",
               "coefficients seems continuous enough that you are satisfied ",
               "it gives you a good estimate of the variation in the correlation coefficient."),
             hr(),
             # Show the plots
             fluidRow(
               # distribution of pearson R
               column(6, p("This is what the actual correlation looks like:"), plotOutput('scatterPlot')),
               column(6, p("And this is the bootstrapped distribution of the correlation coefficient:"),
                      plotOutput('correlationHistogram'))
             )
    ),
    tabPanel('Mean',
             fluidRow(
               column(8,
                      p("Bootstrapping the mean of a sample can be helpful to get a more precise ",
                        "estimate of the variation in the sample mean. It can help reduce the effect ",
                        "of outliers, and gives you a confidence interval for your mean.")
               ),
               # distribution of pearson R
               column(6, p("The bootstrapped distribution of the mean of X is shown below.",
                           " Values inside the bootstrapped 95% confidence interval are shown in red."),
                      plotOutput('xMean')),
               column(6, p("The bootstrapped distribution of the mean of Y is shown below.",
                           " Values inside the bootstrapped 95% confidence interval are shown in red."),
                      plotOutput('yMean'))
             )
    ),
    tabPanel('Standard Deviation',
             fluidRow(
               p("Bootstrapping the standard deviation of a sample can be helpful to estimate ",
                 "variation in the SD with sampling. Without bootstrapping, this is not really ",
                 "possible."),
               # distribution of pearson R
               column(6, p("The bootstrapped distribution of the standard deviation of X is shown below.",
                           " Values inside the bootstrapped 95% confidence interval are shown in red."),
                      plotOutput('xSd')),
               column(6, p("The bootstrapped distribution of the standard deviation of Y is shown below.",
                           " Values inside the bootstrapped 95% confidence interval are shown in red."),
                      plotOutput('ySd'))
             )
    )
  )
)
)
