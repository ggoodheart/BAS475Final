library(shiny)
library(fpp3)
library(readr)
library(plotly)
library(ggplot2)
library(dtplyr)
library(seasonal)
library(ggpubr)
library(shinythemes)
library(forecast)

timeseries <- aus_accommodation %>%
  filter(State %in% c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"))

decomp <- timeseries %>%
  model(x11 = X_13ARIMA_SEATS(Takings ~ x11())) %>%
  components()



ui <- fluidPage(theme = shinytheme("cyborg"),
                
                h1("Takings by State in Australia"),
                
                textOutput("Instructions"),
                
                h2("Plots & Interpretations"),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Time Series Plot",
                             checkboxGroupInput(inputId = "select_ts",
                                                label = "Select Country",
                                                choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                                selected = "Australian Capital Territory"),
                             checkboxInput(inputId = "trendline",
                                           label = "Show trend line",
                                           value = TRUE),
                             plotlyOutput("ts_plot"),
                             textOutput("ts_interpretation")),
                    
                    tabPanel("Decompostion Plot", 
                             plotOutput("decomp_plot"), 
                             textOutput("decomp_interpretation")),
                    
                    tabPanel("Seasonality Plot", 
                             radioButtons(inputId = "select_season",
                                          label = "Select Country",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotlyOutput("seasonal_plot"), 
                             textOutput("seasonal_interpretation")),
                    
                    tabPanel("AutoCorrelation Plot",
                             radioButtons(inputId = "select_Auto",
                                          label = "Select Country",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotOutput("ACF_plot"), 
                             textOutput("ACF_interpretation")),
                    
                    
                   tabPanel("Subseries Plot",
                             radioButtons(inputId = "select_subseries",
                                          label = "Select Region",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotlyOutput("subseries_plot"), 
                             textOutput("subseries_interpretation")),
                    
                    tabPanel("Naive Forecasting Model",
                             radioButtons(inputId = "select_naive",
                                          label = "Select Region",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotOutput("naive_model")),
                    
                    tabPanel("Seasonal Naive Forecasting Model",
                             radioButtons(inputId = "select_seasonal_naive",
                                          label = "Select Region",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotOutput("seasonal_naive_model")),
                    
                    tabPanel("Mean Forecasting Model",
                             radioButtons(inputId = "select_mean",
                                          label = "Select Region",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotOutput("mean_model")),
                    
                    tabPanel("Drift Forecasting Model",
                             radioButtons(inputId = "select_drift",
                                          label = "Select Region",
                                          choices = c("Australian Capital Territory", "Western Australia", "Northern Territory", "South Australia"),
                                          selected = "Australian Capital Territory"),
                             plotOutput("drift_model"))
                    
                  )
                ),
)

server <- function(input, output){
  
  output$Instructions <- renderText({
    "
    
    The information you are looking at features data coming from four seperate regions of Australia, and discuses their takings quarterly from the years 1998 to 2016
    
    For each tab please select which region you would like to see the data for.
    "
  })
  
  output$ts_plot <- renderPlotly({
    plot1 <- timeseries %>%
      filter(State == input$select_ts) %>%
      ggplot(aes(x = Date, y = Takings, color = factor(State))) +
      geom_line()
    
    if(input$trendline == TRUE) {
      plot1 <- plot1 + geom_smooth(method = lm)
    }
    plot1
  })
  
  output$ts_interpretation <- renderText({
    "The time series plot is showing how the takings have changed over time for each region. For all of the regions you can see that over time the takings for each state has steadily gone up. The trend line helps give you a strong idea by just how much the takings have increased for each state. "
  })
  
  output$decomp_plot <- renderPlot({
    autoplot(decomp) +
      labs(title = "Takings by Region Decomposition")
  })
  
  output$decomp_interpretation <- renderText({
    "The decompositon plot is giving a break down of each of the states in specific categories like seasonality, what we can see is all the states besides Western Austrialia seem to be following the same trends in each of the different categories."
  })
  
  output$seasonal_plot <- renderPlotly({
    timeseries %>% 
      filter(State == input$select_season) %>%
      gg_season(Takings) +
      labs(title = "Takings by State in Australia Seasonality Graph")
  }) 
  
  output$seasonal_interpretation <- renderText({
    "The seasonality of all of the differnet states are going up troughout the years, but Western Austrialia is the only one that appears that the most recent years are not the highest in takings."
  })
  
  output$ACF_plot <- renderPlot({
    timeseries %>%
      filter(State == input$select_Auto) %>%
      ACF(Takings) %>%
      autoplot() +
      labs(title = "Autocorrelation of Takings by State")
  })
  
  output$ACF_interpretation <- renderText({
    "For all but the Northern Territory all correlation seems to be positive, but for the Northern Territory there are multiple quarters where the correlation is negative."
  })
    
  output$subseries_plot <- renderPlotly({
    timeseries %>%
      filter(State == input$select_subseries) %>%
      gg_subseries() +
      labs(title = "Subseries Plot of Takings by State in Australia")
  })
  
  output$subseries_interpretation <- renderText({
    "All States beside Western Australia appear to be continuing to grow quarter aftet quarter. Western Australia has recently began to decline in quarterly Takings."
  })
  
  output$naive_model <- renderPlot({
    
    naive_data <- timeseries %>%
      filter(State == input$select_naive)
    
    train_naive <- naive_data %>%
      filter_index("2000 Q1" ~ "2009 Q4")
    
    naive_fit <- naive_data %>%
      model(NAIVE(Takings)) %>%
      forecast(h = 12)
    
    naive_fit %>%
      autoplot(train_naive, level = NULL) + 
      autolayer(
        filter_index(naive_data, "2009 Q1" ~ .),
        colour = "black") +
      labs(title = "Naive Forecasting Model of Takings by State")
  })
  
   output$seasonal_naive_model <- renderPlot({
    
    seasonalnaive_data <- timeseries %>%
      filter(State == input$select_seasonal_naive)
    
    train_seasonalnaive <- seasonalnaive_data %>%
      filter_index("2000 Q1" ~ "2009 Q4")
    
    seasonalnaive_fit <- seasonalnaive_data %>%
      model(SNAIVE(Takings ~lag("year"))) %>%
      forecast(h = 12)
    
    seasonalnaive_fit %>%
      autoplot(train_seasonalnaive, level = NULL) + 
      autolayer(
        filter_index(seasonalnaive_data, "2009 Q1" ~ .),
        colour = "black") +
      labs(title = "Seasonal Naive Forecasting Model of Takings by State")
  })
  
  output$mean_model <- renderPlot({
    
    mean_data <- timeseries %>%
      filter(State == input$select_mean)
    
    train_mean <- mean_data %>%
      filter_index("2000 Q1" ~ "2009 Q4")
    
    mean_fit <- mean_data %>%
      model(MEAN(Takings)) %>%
      forecast(h = 12)
    
    mean_fit %>%
      autoplot(train_mean, level = NULL) + 
      autolayer(
        filter_index(mean_data, "2009 Q1" ~ .),
        colour = "black") +
      labs(title = "Mean Forecasting Model of Takings by State")
  })
  
  output$drift_model <- renderPlot({
    
    drift_data <- timeseries %>%
      filter(State == input$select_drift)
    
    train_drift <- drift_data %>%
      filter_index("2000 Q1" ~ "2009 Q4")
    
    drift_fit <- drift_data %>%
      model(RW(Takings ~ drift())) %>%
      forecast(h = 12)
    
    drift_fit %>%
      autoplot(train_drift, level = NULL) + 
      autolayer(
        filter_index(drift_data, "2009 Q1" ~ .),
        colour = "black") +
      labs(title = "Drift Forecasting Model of Takings by State")
  })
  
}

shinyApp(ui = ui, server = server)
