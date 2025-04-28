# Load libraries needed for the Jemisco Trading Dashboard 
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(httr)
library(jsonlite)


# Creating the UI layout for the Jemisco Trading Dashboard 



ui <- fluidPage(
  
  tags$style(HTML("
  .navbar-default .navbar-nav > li > a {
    font-size: 20px; 
    font-weight: bold;
  }
")),
  
  div(
    style = "text-align:center; margin-top:20px; margin-bottom:30px;",
    
    # Big Main Title
    div(
      style = "font-size:36px; font-weight:900; color:#155724; text-shadow: 1px 1px 2px #c3e6cb;",
      "🚛 Jemisco Logistics Crypto Investment Dashboard"
    ),
    
    # Subheading / Slogan
    div(
      style = "font-size:18px; font-weight:400; color:#6c757d; margin-top:10px;",
      em("Moving Your Investments Forward, One Step at a Time.")
    )
  ),
  
  #New Welcome/Instruction Box
  fluidRow(
    column(12,
           div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
               h4("Welcome!"),
               p("This dashboard helps you estimate how long it might take to grow your cryptocurrency investment to a desired target amount."),
               p("Here's how to use it:"),
               tags$ul(
                 tags$li("Select your preferred cryptocurrency (only 5 crypto coins available at the moment)."),
                 tags$li("Enter the amount you want to invest (in ZAR)."),
                 tags$li("Optionally, enter your target return amount."),
                 tags$li("Click 'Calculate' to predict your maturity time."),
                 tags$li("See past year performance and your projected investment growth!")
               )
           )
    )
  ),
  
  navbarPage(
    theme = shinythemes::shinytheme("sandstone"),
    "",
    
    tabPanel("Investment Calculator",
             sidebarLayout(
               
               # Sidebar panel to collect user inputs 
               sidebarPanel(
                 
                 # User selects which coin they want to invest in
                 selectInput("coin", "Select Cryptocurrency:",
                             choices = c("Bitcoin" = "bitcoin",
                                         "Ethereum" = "ethereum",
                                         "Solana" = "solana",
                                         "XRP" = "ripple",
                                         "Cardano" = "cardano")),
                 
                 # User inputs the amount they want to invest with default amount being R1000
                 numericInput("investment", "Investment Amount (ZAR):", value = 1000),
                 
                 # User inputs the target amount they want to reach and if not, 
                 #We calculate the 10% gain of the investment amount 
                 numericInput("target", "Target Return Amount (ZAR):", value = NA),
                 
                 # Button for the calculation
                 actionButton("calculate", "Calculate", icon = icon("play"))
               ),
               
               # Main panel to show outputs 
               mainPanel(
                 h3("Maturity Prediction"),
                 uiOutput("maturityTime"),
                 br(),
                 h3("Past Year Performance"),
                 plotOutput("yearPlot"),
                 br(),
                 h3("Projected Investment Growth"),
                 plotOutput("growthPlot")
               )
             )
    )
  )
)

#  Creating the Server Logic for the Jemisco Trading Dashboard 



server <- function(input, output, session) {
  
  # This observeEvent will only trigger once the user clicks the calculate button
  observeEvent(input$calculate, {
    
    # Collect inputs from the user
    investment <- input$investment
    target <- input$target
    coin <- input$coin
    
    # If the target amount is not entered, assume and calculate 10% growth
    if (is.na(target) || target <= investment) {
      target <- investment * 1.10
    }
    
    # Pulling live USD to ZAR exchange rate using Open ER API
    exchange_url <- "https://open.er-api.com/v6/latest/USD"
    exchange_response <- GET(exchange_url)
    exchange_data <- content(exchange_response, as = "parsed")
    
    # Check if the exchange rate was fetched successfully
    if (is.null(exchange_data$rates$ZAR)) {
      showNotification("Error: Unable to fetch exchange rate from Open ER API.", type = "error")
      return(NULL)
    }
    
    usd_to_zar <- exchange_data$rates$ZAR
    
    # Pulling 1 year historical crypto price data from CoinGecko 
    url <- paste0("https://api.coingecko.com/api/v3/coins/", coin, 
                  "/market_chart?vs_currency=usd&days=365")
    response <- GET(url)
    data <- content(response, as = "parsed")
    
    prices <- data$prices
    
    # Check if we got valid price data because sometimes,
    #CoinGeko does not give valid price data 
    if (is.null(prices) || length(prices) == 0) {
      showNotification(paste0("Error: No price data available for ", coin), type = "error")
      return(NULL)
    }
    
    #Building a clean dataframe for prices
    price_list <- lapply(prices, function(x) {
      if (length(x) >= 2 && !is.null(x[[1]]) && !is.null(x[[2]])) {
        data.frame(
          timestamp = as.numeric(x[[1]]),
          price_usd = as.numeric(x[[2]])
        )
      } else {
        NULL
      }
    })
    
    # Remove NULL entries
    price_list <- Filter(Negate(is.null), price_list)
    
    # If no good data after filtering, exit
    if (length(price_list) == 0) {
      showNotification(paste0("Error: No usable price data found for ", coin), type = "error")
      return(NULL)
    }
    
    # Bind the cleaned list into a single dataframe
    price_df <- do.call(rbind, price_list)
    
    # Converting timestamps into readable date format
    price_df$date <- as.POSIXct(price_df$timestamp/1000, origin = "1970-01-01", tz = "UTC")
    
    # Keeping only the columns we need
    price_df <- price_df[, c("date", "price_usd")]
    
    # Converting the price from USD to ZAR
    price_df$price_zar <- price_df$price_usd * usd_to_zar
    
    # Calculating the investment maturity details
    first_price <- price_df$price_zar[1]
    last_price <- tail(price_df$price_zar, 1)
    
    # If prices are invalid, exit
    if (first_price <= 0 || last_price <= 0) {
      showNotification("Error: Invalid price data for maturity calculation.", type = "error")
      return(NULL)
    }
    
    daily_growth_rate <- (last_price/first_price)^(1/365) - 1
    
    if (daily_growth_rate <= 0) {
      showNotification("Warning: No positive growth over the past year.", type = "warning")
      return(NULL)
    }
    
    days_needed <- log(target / investment) / log(1 + daily_growth_rate)
    
    # Output the estimated time/days to reach target
    output$maturityTime <- renderUI({
      gain <- target - investment
      HTML(paste0("<div style='text-align:center; font-size:20px; font-weight:bold; color:#155724; background-color:#d4edda; padding:15px; border-radius:8px; border: 2px solid #c3e6cb;'>",
                  "Estimated time to reach ZAR ", round(target, 2),
                  " is about ", round(days_needed), " days.<br>",
                  "💸 Your estimated profit gain is R", round(gain, 2), ".</div>"))
    })
    coin_color <- switch(coin,
                         "bitcoin" = "orange",
                         "ethereum" = "purple",
                         "solana" = "deepskyblue",
                         "ripple" = "dodgerblue",
                         "cardano" = "mediumseagreen",
                         "gray")
    
    # Plotting the Past Year Performance of the coin
    
    output$yearPlot <- renderPlot({
      
      ggplot(price_df, aes(x = date, y = price_zar))+
        geom_line(color = coin_color,size=1) +
        labs(title = paste0(coin, " Price in ZAR Over the Past Year"),
             x = "Date",
             y = "Price (ZAR)") +
        theme_minimal(base_size = 16)
      
    })
    
    # Plotting the Projected Investment Growth 
    output$growthPlot <- renderPlot({
      
      days <- 0:round(days_needed)
      balance <- investment * (1 + daily_growth_rate)^days
      growth_df <- data.frame(Day = days, Balance = balance)
      
     ggplot(growth_df, aes(x = Day, y = Balance)) +
        geom_line(color = coin_color,size=1) +
        labs(title = "Projected Investment Growth",
             x = "Days", 
             y = "Balance (ZAR)") +
        theme_minimal(base_size=16)+
        coord_cartesian(ylim=c(investment,target))
        
      
    })
    
  })
}

# Running the App
shinyApp(ui = ui, server = server)
