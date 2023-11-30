# ---
# title: "Stocks"
# author: "Logan Anderson"
# format: Shiny App


# ---

# load libraries
library(bslib)
library(OSUICode)
library(shiny)
library(shinythemes)
library(gt)
library(dplyr)
library(readxl)
library(GGally)
library(ggridges)
library(ggplot2)
library(tidyr)
library(forcats)
library(MASS)
library(DT)

# data
stocks <- read_excel("SP500_stocks.xlsx")

stocks$Founded <- as.numeric(stocks$Founded)

stocks$Founded[is.na(stocks$Founded)] = 0
stocks$Buffet[is.na(stocks$Buffet)] = 0

clean_stocks <- na.omit(stocks)

clean_column_names <- function(names) {
  # Replace spaces with underscores
  names <- gsub(" ", "_", names)
  # Remove other special characters
  names <- make.names(names)
  return(names)
}




library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    tags$nav(
      class = "navbar navbar-expand-lg bg-body-tertiary",
      tags$div(
        class = "container-fluid",
        tags$a(class = "navbar-brand", href = "#", "Navbar"),
        actionButton(
          class = "navbar-toggler",
          "Toggle navigation",
          icon("bars")
        ),
        tags$div(
          class = "collapse navbar-collapse",
          id = "navbarSupportedContent",
          tags$ul(
            class = "navbar-nav me-auto mb-2 mb-lg-0",
            tags$li(class = "nav-item", tags$a(class = "nav-link active", `aria-current` = "page", href = "#", "Home")),
            tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www.spglobal.com/spdji/en/indices/equity/sp-500/#overview", "S$P 500 Live")),
            tags$li(
              class = "nav-item dropdown",
              tags$a(
                class = "nav-link dropdown-toggle",
                href = "#",
                role = "button",
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false",
                "Dropdown"
              ),
              style = "margin-right: 50px;",
              tags$ul(
                class = "dropdown-menu",
                tags$li(tags$a(class = "dropdown-item", href = "#", "Action")),
                tags$li(tags$a(class = "dropdown-item", href = "#", "Another action")),
                tags$li(class = "dropdown-divider"),
                tags$li(tags$a(class = "dropdown-item", href = "#", "Something else here"))
              )
            ),
            tags$li(class = "nav-item", tags$a(class = "nav-link disabled", `aria-disabled` = "true", "Disabled"))
          ),
          tags$form(
            class = "d-flex",
            role = "search",
            tags$input(
              class = "form-control me-2",
              type = "search",
              placeholder = "Search",
              `aria-label` = "Search"
            ),
            tags$button(class = "btn btn-outline-success", type = "submit", "Search")
          )
        )
      )
    )
  )
)

server <- function(input, output) {

}

shinyApp(ui, server)










library(shiny)
library(bslib)

# # Ensure htmltools is updated
# if ("htmltools" %in% installed.packages()[, "Package"] &&
#     packageVersion("htmltools") < "0.5.7") {
#   install.packages("htmltools")
# }

ui <- navbarPage(
    theme = bs_theme(
      version = 5,
      bg = "#202123",
      fg = "#B8BCC2",
      primary = "#EA80FC",
      secondary = "#00DAC6",
      base_font = font_google("Prompt"),
      heading_font = font_google("Proza Libre")#,
      # "navbar-bg" = "green"
    ),
    title = "My Website",
    tabPanel("A", "Page A"),
    tabPanel("B", "Page B")
)
shinyApp(ui, function(...) {})


# Define UI for application for Shiny App
# ui <- fluidPage(theme = bslib_neon_theme,
#                 # Theme
#                 navbarPage(
#                   "Stocks (S&P 500)",
#                   
#                   # App title
#                   tabPanel(
#                     "Understanding Variables",
#                     sidebarPanel(
#                       titlePanel("Correlation Variables"),
#                       tags$h1(""),
#                       selectInput(
#                         "vars",
#                         "Choose Variables:",
#                         names(clean_stocks),
#                         multiple = TRUE,
#                         selected = "Sector"
#                       )
#                     ),
#                     mainPanel(
#                       title = "Correlation Plot & Descriptions",
#                       width = 12,
#                       height = 12,
#                       splitLayout(
#                         cellWidths = c("50%", "50%"),
#                         plotOutput("correlation_plot"),
#                         gt_output("table_plot"),
#                       )
#                     )
#                   ),
#                   tabPanel(
#                     "Distribution",
#                     sidebarPanel(
#                       titlePanel("Configure Ridgeline"),
#                       tags$h1(""),
#                       selectInput("group", "Choose Group:", names(clean_stocks[, c(1, 2, 4, 5)])),
#                       selectInput("ridge", "Choose Column:", names(clean_stocks[, c(6:21)])),
#                       sliderInput(
#                         "bins",
#                         "Number of Bins:",
#                         min = 10,
#                         max = 150,
#                         value = 20
#                       ),
#                       sliderInput(
#                         "minfilt",
#                         "Minimum Filter:",
#                         min = -1,
#                         max = 3000,
#                         value = 0
#                       ),
#                       sliderInput(
#                         "maxfilt",
#                         "Maximum Filter:",
#                         min = -1,
#                         max = 3000,
#                         value = 1000
#                       ),
#                     ),
#                     mainPanel(
#                       title = "Ridgeline",
#                       width = 12,
#                       height = 12,
#                       plotOutput("ridgeline_plot"),
#                     )
#                   ),
#                   tabPanel(
#                     "Linear Regression",
#                     sidebarPanel(
#                       # Input fields for user to choose columns
#                       selectInput(
#                         "dependent_var",
#                         "Dependent Variable",
#                         choices = clean_column_names(names(clean_stocks)),
#                         selected = "Price_Today"
#                       ),
#                       selectInput(
#                         "independent_vars",
#                         "Independent Variables",
#                         choices = clean_column_names(names(clean_stocks[, -c(4, 5)])),
#                         multiple = TRUE
#                       ),
#                       actionButton("run_analysis", "Run Analysis")
#                     ),
#                     mainPanel(
#                       title = "Linear Model",
#                       width = 12,
#                       height = 12,
#                       verbatimTextOutput("anova_output"),
#                     )
#                   )
#                 ))
# 
# 
# 
# 
# # Define server logic required for Shiny App
# server <- function(input, output) {
#   # Render correlation plot
#   output$correlation_plot <- renderPlot({
#     clean_stocks$Sector <- as.numeric(clean_stocks$Sector)
#     clean_stocks$Industry <- as.numeric(clean_stocks$Industry)
#     clean_stocks$Symbol <- as.numeric(clean_stocks$Symbol)
#     clean_stocks$Security <- as.numeric(clean_stocks$Security)
#     
#     ggcorr(clean_stocks[, input$vars], method = c("everything", "pearson"))
#   })
#   
#   output$table_plot <- render_gt({
#     data.frame(
#       Variables = c(
#         "GICS Sector",
#         "GICS Sub Industry",
#         "Founded",
#         "Symbol",
#         "Security",
#         "52w high",
#         "52w low",
#         "%YTD",
#         "Market Cap",
#         "Beta",
#         "EPS",
#         "PE"
#       ),
#       Description = c(
#         "Type of Sector the stock is in",
#         "Type of Industry in a certain sector",
#         "Founded Year",
#         "Acronym of the company name",
#         "Company Name",
#         "Highest sell value in the last 52 weeks",
#         "Lowest sell value in the last 52 weeks",
#         "Percent change in price from the start of the year",
#         "The total amount of shares a company has. The higher is usually more stable",
#         "Measure of volatility",
#         "Earnings Per Share",
#         "Price to Earnings Ratio"
#       )
#     ) %>%
#       gt() %>%
#       tab_header(title = md("What are the Variables"),
#                  subtitle = md("S&P 500"))
#   })
#   
#   
#   output$ridgeline_plot <- renderPlot({
#     clean_stocks <-
#       filter(clean_stocks, get(input$ridge) <= input$maxfilt)
#     clean_stocks <-
#       filter(clean_stocks, get(input$ridge) >= input$minfilt)
#     
#     clean_stocks %>%
#       ggplot(aes(
#         y = get(input$group),
#         x = get(input$ridge),
#         fill = get(input$group)
#       )) +
#       geom_density_ridges(alpha = 0.6,
#                           stat = "binline",
#                           bins = input$bins) +
#       theme_ridges() +
#       theme(
#         legend.position = "none",
#         panel.spacing = unit(0.1, "lines"),
#         strip.text.x = element_text(size = 8)
#       ) +
#       xlab(paste(input$ridge)) +
#       ylab(paste(input$group))
#   })
#   
#   results_reactive <- reactiveVal(NULL)
#   
#   observeEvent(input$run_analysis, {
#     dependent_var <- input$dependent_var
#     independent_vars <- input$independent_vars
#     
#     # Ensure at least one independent variable is selected
#     if (is.null(independent_vars) ||
#         length(independent_vars) == 0) {
#       return()
#     }
#     
#     # Clean column names
#     clean_stocks_cleaned <- clean_stocks
#     names(clean_stocks_cleaned) <-
#       clean_column_names(names(clean_stocks_cleaned))
#     
#     clean_stocks_cleaned$Sector <-
#       as.factor(clean_stocks_cleaned$Sector)
#     clean_stocks_cleaned$Industry <-
#       as.factor(clean_stocks_cleaned$Industry)
#     
#     formula <-
#       as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
#     fit <- lm(formula, data = clean_stocks_cleaned)
#     
#     step <- stepAIC(fit, direction = "both")
#     
#     results_reactive(step$anova)
#   })
#   
#   output$anova_output <- renderPrint({
#     results <- results_reactive()
#     if (!is.null(results)) {
#       print(results)
#     }
#   })
#   
# }
# shinyApp(ui = ui, server = server)
