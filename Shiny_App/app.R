# ---
# title: "S&P 500 Dashboard Shiny App"
# author: "Logan Anderson"
# format: Shiny App
# ---

# Deployed at https://lcanderson.shinyapps.io/S-P-500_Dashboard/
# Source code at GitHub: https://github.com/logan-c-anderson/S-P-500-Shiny-App/tree/main


# load libraries
library(shinydashboard)
library(shiny)
library(gt)
library(dplyr)
library(readxl)
library(GGally)
library(ggridges)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(forcats)
library(MASS)
library(DT)
library(plotly)
library(RColorBrewer)

#load data
stocks <- read_excel("SP500_stocks.xlsx")

stocks$Founded <- as.numeric(stocks$Founded)
stocks$Founded[is.na(stocks$Founded)] = 0
stocks$`Buffet holdings (12/31/2019)`[is.na(stocks$`Buffet holdings (12/31/2019)`)] = 0

clean_stocks <- na.omit(stocks)

clean_column_names <- function(names) {
  # Replace spaces with underscores
  names <- gsub(" ", "_", names)
  # Remove other special characters
  names <- make.names(names)
  return(names)
}

Data1 <- read_excel("SP500_stocks.xlsx")

Data1 <- Data1[!is.na(Data1$'%YTD'), ]
colnames(Data1)[1] <- "GICSSectors"
colnames(Data1)[7] <- "High52w"
colnames(Data1)[8] <- "Low52w"

Data1 <- mutate(Data1, Cap = case_when(
  Data1$`Market Cap (B)` <= 5 ~ "SmallCap",
  Data1$`Market Cap (B)` > 5 & Data1$`Market Cap (B)` < 15 ~ "MidCap",
  Data1$`Market Cap (B)` >= 15 ~ "LargeCap"
  ))  #split EPS into three groups based on Market Cap




# Define UI for application for Shiny App
# Define UI for the dashboard application
ui <- dashboardPage(
  dashboardHeader(title = "S&P 500 Dashboard"),   #define dashboard header
  dashboardSidebar(
    #Set tabs for sidebar
    sidebarMenu(
      menuItem("Homepage", tabName = "tab1"),
      menuItem("Data Dictionary", tabName = "tab2"),
      menuItem("Stock Distributions", tabName = "tab3"),
      menuItem("EPS Per Market Cap", tabName = "tab4")
    )
  ),  #define dashboard side bar  #define dashboard side bar
  
  #body of tabs
  dashboardBody(
    tags$nav(class = "navbar navbar-expand-lg bg-body-tertiary",
      tags$div(class = "container-fluid",
        tags$a(class = "navbar-brand", href = "https://www.spglobal.com/spdji/en/indices/equity/sp-500/#overview", 
               tags$img(src = "https://easyportfol.io/blog/spx/thumb.svg", alt = "Brand Logo", style = "vertical-align: top; width: 80px;")),
      )
    ),
    tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www.cnbc.com/quotes/.SPX", "S&P 500 Live Updates"),
            style = "margin-right: 30px;",
    ),
    tabItems(
      #Tab 1 - Home
      tabItem(tabName = "tab1",
              h3("Logan's Get Rich Quick Dashboard Home Page"),
              p("🎉 Welcome to Logan's Get Rich Quick Dashboard: Where Fortunes Begin! 🚀"),
              p("Are you ready to turn stock market chaos into cash flow? Well, you came to the right place!
               While I may have little-to-none stock market experience and knowledge, be assured that I can confidently pretend to. 
               No I may have not mastered the art of transforming financial jargon into your own personal goldmine - I think you will 
               still find this Shiny App kinda cool. So, whether you're a seasoned investor or just dipped your toes into the S&P 500 waters, 
               you've just stumbled upon the secret sauce to financial success. ***DISCLAIMER*** YOU HAVE NOT JUST STUMBLED UPON THE SECRET SAUCE 
               TO FINANICAL SUCCESS
               "), p("🔮 Why Logan's Get Rich Quick Dashboard? Because you're broke, need cash quick, and this was an assignment! 
               Everybody knows the stock makret is wildy unpredictable, so how can you tell the future of it?
               Sure, crystal balls are fun, but I prefer cold, hard data. Dive into this state-of-the-art analysis tool, 
               and you'll be making investment decisions that'll have your friends wondering if you've got a direct line to Warren Buffett.📈"),
              p("No PhD in finance? No problem! This user-friendly interface takes you from stock market rookie to Wall Street wizard in just 
               a few clicks. Say goodbye to confusing charts and hello to your new best friend: intuitive analytics that even your grandma 
               could understand. 💸 Unleash Your Inner Wolf of Wall Street (Without the Wolf Attitude)"),
              p("🌐 Your Passport to Prosperity
               Logan's Get Rich Quick Dashboard isn't just a website; it's a ticket to financial freedom. Join us on this journey, and together, 
               we'll ride the waves of the S&P 500 straight to the land of yachts and caviar. Well, maybe not caviar, but you get the point.🚀✨") ,
               p("Logan's Get Rich Quick Dashboard: Because Being Rich is More Fun Than Not Being Rich")
              ),
      #Tab 2 - Data Dictionary
      tabItem(
        tabName = "tab2",
        h3("S&P 500 Dashboard Data Dictionary"),
        p("These variables will be used throught the analysis. Feel free to refrernce back to this page as often as possible."),
        tabsetPanel(
          tabPanel(
            fluidPage(gt_output("table_plot"))
            ) #tabsPanel
          ) #tabsetPanel
        ),
      tabItem(
        tabName = "tab3",
        h3("Distributions"),
        tabsetPanel(
          tabPanel(
              titlePanel("Configure Ridgeline"),
              tags$h1(""),
              selectInput("group", "Choose Group:", names(clean_stocks[, c(1, 2, 4, 5)])),
              selectInput("ridge", "Choose Column:", names(clean_stocks[, c(6:21)])),
              sliderInput("bins", "Number of Bins:", min = 10, max = 150, value = 20),
              sliderInput("minfilt", "Minimum Filter:", min = -1, max = 3000, value = 0),
              sliderInput("maxfilt", "Maximum Filter:", min = -1, max = 3000, value = 1000),
            fluidPage(title = "Ridgeline", width = 12, height = 12, plotOutput("ridgeline_plot"),
            )
          ),
        )
      ), #tabItem
      tabItem(
        tabName = "tab4",
        h3("EPS Per Market Cap Size"),
        sidebarLayout(
          sidebarPanel(
            selectInput("CapSize", "Select Cap Size", choices = c("Veiw All Cap Sizes", "Quit Showing Me Data"))
          ),
          mainPanel(
            plotOutput("conditionalBoxPlot")
          )
        )
      )
      #   tabsetPanel(
      #     tabPanel(
      #       titlePanel("Earnings Per Share")
      #       ,
      #       
      #     
      #   )
      # )
      ) #tabItems
    ) #dashboardBody
  ) #dashboardPage






server <- function(input, output) {
  
  #data dictionary
  output$table_plot <- render_gt({
    data.frame(
      Variables = c(
        "GICS Sector",
        "GICS Sub Industry",
        "Founded",
        "Symbol",
        "Security",
        "52w high",
        "52w low",
        "%YTD",
        "Market Cap",
        "Beta",
        "EPS",
        "PE"
      ),
      Description = c(
        "Global Industry Classification Standard (GICS) sector to which the company belongs. 
        GICS is a system for categorizing stocks into sectors and industries.",
        "Further classification of the company's industry within the GICS sector.",
        "The year the company was founded.",
        "Stock symbol that uniquely identifies the company's stock on the stock exchange.",
        "The name or description of the financial security (stock) being traded.",
        "The highest stock price the company reached in the last 52 weeks.",
        "The percentage difference between the current stock price and the 52-week low.",
        "Percent change in price from the start of the year.",
        "Market capitalization of the company in billions.",
        "Measure of volatility. It indicates the stock's sensitivity to market movements.",
        "Earnings Per Share.",
        "Price to Earnings Ratio."
      )
    ) %>%
      gt() %>%
      tab_header(title = md("Dashborad Variables"),
                 subtitle = md("S&P 500"))
  }) #data dictionary
  
  
  #construct ridgeline plot 
  output$ridgeline_plot <- renderPlot({
    clean_stocks <-
      filter(clean_stocks, get(input$ridge) <= input$maxfilt)
    clean_stocks <-
      filter(clean_stocks, get(input$ridge) >= input$minfilt)
    
    clean_stocks %>%
      ggplot(aes(
        y = get(input$group),
        x = get(input$ridge),
        fill = get(input$group)
      )) +
      geom_density_ridges(alpha = 0.6,
                          stat = "binline",
                          bins = input$bins) +
      theme_ridges() +
      theme(
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab(paste(input$ridge)) +
      ylab(paste(input$group))
  }) #ridgeline plot
  
  #Boxplot for EPS per cap size
  output$boxPlot <- renderPlot({
    # data <- Data1[Data1$Cap == input$LargeCap,]
    # 
    # if (nrow(data) == 0) {
    #   return(NULL)
    # }
    ggplot(Data1, aes(x = Cap, y = EPS, fill = Cap)) +
      geom_boxplot() +
      labs(x = "MarketCap", y = "EPS", title = "Boxplot of Cap vs EPS") +
      theme_minimal() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) +
      scale_fill_brewer(palette = "Blues") +
      ylim(-10, 45)
  }) #EPS boxPlot
  
  
  output$conditionalBoxPlot <- renderPlot({
    if (input$CapSize == "Veiw All Cap Sizes") {
      # Render Plot A
      plot(ggplot(Data1, aes(x = Cap, y = EPS, fill = Cap)) +
             geom_boxplot() +
             labs(x = "MarketCap", y = "EPS", title = "Boxplot of Cap vs EPS") +
             theme_minimal() +
             theme_fivethirtyeight() +
             theme(axis.title = element_text()) +
             scale_fill_brewer(palette = "Blues") +
             ylim(-10, 45), main = "Veiw All Capsizes")
    } 
    else if (input$CapSize == "Quit Showing Me Data") {
      # Display something that's not data
      img(src = "https://media3.giphy.com/media/3o6gDSdED1B5wjC2Gc/giphy.gif")
    } 
  })

}




# Run the application
shinyApp(ui = ui, server = server)


