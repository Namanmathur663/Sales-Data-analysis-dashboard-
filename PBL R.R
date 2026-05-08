

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(ggplot2); library(dplyr)
  library(plotly); library(DT); library(lubridate); library(scales)
})

# Data with ₹ prices and ratings
set.seed(123)
sales_data <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  product = sample(c("Laptop", "Phone", "Tablet", "Headphones", "Monitor"), 365, replace = TRUE),
  category = sample(c("Electronics", "Accessories", "Mobile"), 365, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 365, replace = TRUE),
  salesperson = sample(c("Alice", "Bob", "Charlie", "Diana", "Eve"), 365, replace = TRUE),
  units_sold = sample(1:20, 365, replace = TRUE),
  unit_price = runif(365, 4000, 80000),
  discount = sample(0:20, 365, replace = TRUE),
  rating = sample(1:5, 365, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))
) %>%
  mutate(
    revenue = units_sold * unit_price * (1 - discount/100),
    month = floor_date(date, "month"),
    year_month = format(date, "%Y-%m")
  )

# ₹ Formatter
rupees_format <- function(x) paste0("₹", scales::comma(round(x/1000, 1)), "K")

# Star formatter
star_format <- function(x) paste0(round(x, 2), " ★")

# UI - VALID COLORS ONLY
ui <- dashboardPage(
  dashboardHeader(title = "🛒 Sales Dashboard - ₹ RUPEES"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Products", tabName = "products", icon = icon("box")),
      menuItem("Ratings", tabName = "ratings", icon = icon("star")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    dateRangeInput("date_range", "Date Range:", 
                   start = min(sales_data$date), end = max(sales_data$date)),
    selectInput("region_filter", "Region:", 
                choices = c("All", sort(unique(sales_data$region))), selected = "All"),
    selectInput("product_filter", "Product:", 
                choices = c("All", sort(unique(sales_data$product))), selected = "All")
  ),
  
  dashboardBody(
    tabItems(
      # Overview
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_revenue", width = 3),
          valueBoxOutput("total_units", width = 3),
          valueBoxOutput("avg_order", width = 3),
          valueBoxOutput("top_product", width = 3)
        ),
        fluidRow(
          box(plotlyOutput("revenue_trend", height = "400px"), width = 12, 
              title = "📈 Revenue Trend (₹)", status = "primary", solidHeader = TRUE)
        )
      ),
      
      # Trends
      tabItem(tabName = "trends",
        fluidRow(
          box(plotlyOutput("monthly_sales", height = "400px"), width = 8, 
              title = "📅 Monthly Revenue (₹)", status = "success", solidHeader = TRUE),
          box(plotlyOutput("category_pie", height = "400px"), width = 4, 
              title = "📱 By Category", status = "warning", solidHeader = TRUE)
        )
      ),
      
      # Products
      tabItem(tabName = "products",
        fluidRow(
          box(plotlyOutput("top_products", height = "450px"), width = 12, 
              title = "🏆 Top Products (₹)", status = "info", solidHeader = TRUE)
        )
      ),
      
      # Ratings
      tabItem(tabName = "ratings",
        fluidRow(
          valueBoxOutput("avg_rating", width = 3),
          valueBoxOutput("total_reviews", width = 3),
          valueBoxOutput("top_rated_product", width = 3),
          valueBoxOutput("rating_trend", width = 3)
        ),
        fluidRow(
          box(plotlyOutput("rating_by_product", height = "400px"), width = 6, 
              title = "⭐ Average Rating by Product", status = "warning", solidHeader = TRUE),
          box(plotlyOutput("rating_by_salesperson", height = "400px"), width = 6, 
              title = "⭐ Average Rating by Salesperson", status = "success", solidHeader = TRUE)
        ),
        fluidRow(
          box(plotlyOutput("rating_distribution", height = "400px"), width = 6, 
              title = "📊 Rating Distribution", status = "info", solidHeader = TRUE),
          box(plotlyOutput("rating_vs_revenue", height = "400px"), width = 6, 
              title = "💰 Rating vs Revenue", status = "primary", solidHeader = TRUE)
        )
      ),
      
      # Data
      tabItem(tabName = "data",
        box(DT::dataTableOutput("sales_table"), width = 12, 
            title = "📋 Sales Data (₹)", status = "primary", solidHeader = TRUE)
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- sales_data %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    if(input$region_filter != "All") data <- data %>% filter(region == input$region_filter)
    if(input$product_filter != "All") data <- data %>% filter(product == input$product_filter)
    data
  })
  
  # KPI Boxes - VALID COLORS
  output$total_revenue <- renderValueBox({
    revenue <- sum(filtered_data()$revenue)
    valueBox(rupees_format(revenue), "Total Revenue", icon("rupee-sign"), "green")
  })
  
  output$total_units <- renderValueBox({
    units <- sum(filtered_data()$units_sold)
    valueBox(units, "Units Sold", icon("box"), "blue")
  })
  
  output$avg_order <- renderValueBox({
    avg <- mean(filtered_data()$revenue / filtered_data()$units_sold)
    valueBox(rupees_format(avg), "Avg Order Value", icon("calculator"), "yellow")
  })
  
  output$top_product <- renderValueBox({
    top_prod <- filtered_data() %>%
      group_by(product) %>%
      summarise(total = sum(revenue), .groups = 'drop') %>%
      slice_max(total, n = 1) %>%
      pull(product)
    valueBox(top_prod, "Top Product", icon("award"), "red")
  })
  
  # Rating KPI Boxes
  output$avg_rating <- renderValueBox({
    avg <- mean(filtered_data()$rating, na.rm = TRUE)
    valueBox(star_format(avg), "Avg Rating", icon("star"), "yellow")
  })
  
  output$total_reviews <- renderValueBox({
    n <- nrow(filtered_data())
    valueBox(n, "Total Reviews", icon("comments"), "blue")
  })
  
  output$top_rated_product <- renderValueBox({
    top <- filtered_data() %>%
      group_by(product) %>%
      summarise(avg_rating = mean(rating), .groups = 'drop') %>%
      slice_max(avg_rating, n = 1) %>%
      pull(product)
    valueBox(top, "Top Rated Product", icon("trophy"), "green")
  })
  
  output$rating_trend <- renderValueBox({
    trend <- filtered_data() %>%
      mutate(period = ifelse(date < median(date), "First Half", "Second Half")) %>%
      group_by(period) %>%
      summarise(avg = mean(rating), .groups = 'drop')
    if(nrow(trend) == 2 && trend$avg[2] >= trend$avg[1]) {
      val <- "↑ Improving"; col <- "green"
    } else if(nrow(trend) == 2) {
      val <- "↓ Declining"; col <- "red"
    } else {
      val <- "→ Stable"; col <- "yellow"
    }
    valueBox(val, "Rating Trend", icon("chart-line"), col)
  })
  
  # Charts
  output$revenue_trend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month) %>%
      summarise(revenue = sum(revenue), units = sum(units_sold), .groups = 'drop')
    
    plot_ly(data, x = ~month, y = ~revenue/100000, type = 'scatter', mode = 'lines+markers',
            name = 'Revenue (₹L)', line = list(color = '#28a745', width = 4)) %>%
      add_trace(y = ~units, name = 'Units', mode = 'lines', 
                line = list(color = '#007bff', dash = 'dash')) %>%
      layout(title = "Revenue Trend (₹ Lakhs) & Units",
             xaxis = list(title = "Month"),
             yaxis = list(title = "₹ Lakhs / Units"))
  })
  
  output$monthly_sales <- renderPlotly({
    data <- filtered_data() %>%
      group_by(year_month) %>%
      summarise(revenue = sum(revenue), .groups = 'drop')
    
    plot_ly(data, x = ~year_month, y = ~revenue/100000, type = 'bar',
            marker = list(color = '#17a2b8')) %>%
      layout(title = "Monthly Revenue (₹ Lakhs)",
             xaxis = list(title = "Month"), 
             yaxis = list(title = "₹ Lakhs"))
  })
  
  output$category_pie <- renderPlotly({
    data <- filtered_data() %>%
      group_by(category) %>%
      summarise(revenue = sum(revenue), .groups = 'drop')
    
    plot_ly(data, labels = ~category, values = ~revenue, type = 'pie',
            textinfo = 'label+percent+value',
            marker = list(colors = c('#dc3545', '#28a745', '#ffc107'))) %>%
      layout(title = "Revenue by Category (₹)")
  })
  
  output$top_products <- renderPlotly({
    data <- filtered_data() %>%
      group_by(product) %>%
      summarise(revenue = sum(revenue), .groups = 'drop') %>%
      top_n(10, revenue) %>%
      mutate(product = reorder(product, revenue))
    
    plot_ly(data, y = ~product, x = ~revenue/1000, type = 'bar',
            orientation = 'h', marker = list(color = '#007bff')) %>%
      layout(title = "Top 10 Products (₹ Thousands)",
             xaxis = list(title = "Revenue (₹K)"))
  })
  
  # Rating Charts
  output$rating_by_product <- renderPlotly({
    data <- filtered_data() %>%
      group_by(product) %>%
      summarise(avg_rating = mean(rating), .groups = 'drop') %>%
      mutate(product = reorder(product, avg_rating))
    
    plot_ly(data, y = ~product, x = ~avg_rating, type = 'bar',
            orientation = 'h', marker = list(color = '#ffc107')) %>%
      layout(title = "Average Rating by Product",
             xaxis = list(title = "Rating (1-5)", range = c(0, 5)),
             yaxis = list(title = ""))
  })
  
  output$rating_by_salesperson <- renderPlotly({
    data <- filtered_data() %>%
      group_by(salesperson) %>%
      summarise(avg_rating = mean(rating), .groups = 'drop') %>%
      mutate(salesperson = reorder(salesperson, avg_rating))
    
    plot_ly(data, y = ~salesperson, x = ~avg_rating, type = 'bar',
            orientation = 'h', marker = list(color = '#28a745')) %>%
      layout(title = "Average Rating by Salesperson",
             xaxis = list(title = "Rating (1-5)", range = c(0, 5)),
             yaxis = list(title = ""))
  })
  
  output$rating_distribution <- renderPlotly({
    data <- filtered_data() %>%
      count(rating) %>%
      mutate(rating = factor(rating, levels = 1:5))
    
    plot_ly(data, x = ~rating, y = ~n, type = 'bar',
            marker = list(color = c('#dc3545', '#fd7e14', '#ffc107', '#17a2b8', '#28a745'))) %>%
      layout(title = "Rating Distribution",
             xaxis = list(title = "Star Rating"),
             yaxis = list(title = "Count"))
  })
  
  output$rating_vs_revenue <- renderPlotly({
    data <- filtered_data() %>%
      group_by(rating) %>%
      summarise(avg_revenue = mean(revenue), .groups = 'drop')
    
    plot_ly(data, x = ~rating, y = ~avg_revenue/1000, type = 'bar',
            marker = list(color = '#007bff')) %>%
      layout(title = "Average Revenue by Rating",
             xaxis = list(title = "Star Rating"),
             yaxis = list(title = "Avg Revenue (₹K)"))
  })
  
  # Table
  output$sales_table <- DT::renderDataTable({
    filtered_data() %>%
      select(date, product, region, salesperson, units_sold, unit_price, revenue, rating) %>%
      mutate(
        unit_price = paste0("₹", scales::comma(round(unit_price))),
        revenue = paste0("₹", scales::comma(round(revenue))),
        rating = paste0(rating, " ★")
      ) %>%
      arrange(desc(revenue))
  }, options = list(pageLength = 15, scrollX = TRUE))
}

# LAUNCH!
shinyApp(ui = ui, server = server)

