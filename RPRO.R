library(shiny)
library(shinydashboard)
library(ggplot2)
library(randomForest)
library(DT)

# ------------------------------------------------------------
#  LOAD DATA
# ------------------------------------------------------------

setwd("D:/Rpro")

fruit_sales <- read.csv("FruitSalesDataset.csv", stringsAsFactors = FALSE)

# Convert revenue from string to numeric
fruit_sales$Revenue <- as.numeric(gsub("[$,]", "", fruit_sales$Revenue))

# Convert categorical variables
fruit_sales$Fruit <- as.factor(fruit_sales$Fruit)
fruit_sales$Month <- as.factor(fruit_sales$Month)
fruit_sales$SalesPerson <- as.factor(fruit_sales$SalesPerson)

# Aggregations
yearly_revenue <- aggregate(Revenue ~ Year, data = fruit_sales, sum)
monthly_revenue <- aggregate(Revenue ~ Month, data = fruit_sales, sum)

min_year <- yearly_revenue[which.min(yearly_revenue$Revenue), ]
max_year <- yearly_revenue[which.max(yearly_revenue$Revenue), ]

min_month <- monthly_revenue[which.min(monthly_revenue$Revenue), ]
max_month <- monthly_revenue[which.max(monthly_revenue$Revenue), ]

fruit_revenue <- aggregate(Revenue ~ Fruit, data = fruit_sales, sum)
fruit_quantity <- aggregate(Quantity..number.of.boxes. ~ Fruit, data = fruit_sales, sum)

salesperson_revenue <- aggregate(Revenue ~ SalesPerson, data = fruit_sales, sum)
salesperson_quantity <- aggregate(Quantity..number.of.boxes. ~ SalesPerson, data = fruit_sales, sum)

correlation_value <- cor(fruit_sales$Revenue, fruit_sales$Quantity..number.of.boxes.)

# ------------------------------------------------------------
# RANDOM FOREST MODEL
# ------------------------------------------------------------

rf_model <- randomForest(
  Revenue ~ Fruit + Month + Quantity..number.of.boxes. + SalesPerson,
  data = fruit_sales,
  ntree = 500
)

# ------------------------------------------------------------
# UI DESIGN
# ------------------------------------------------------------

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = span("🍎 Fruit Sales Analysis Dashboard",
                 style="font-weight:bold; font-size:20px;")
  ),
  
  dashboardSidebar(
    
    # Sidebar Styling
    tags$style(HTML("
        .skin-blue .sidebar-menu>li.active>a {
            background-color:#1e90ff !important;
            color:white !important;
            font-weight:bold;
        }
        .skin-blue .sidebar-menu>li>a:hover {
            background-color:#4682b4 !important;
            color:white !important;
        }
        .main-sidebar {
            background: linear-gradient(180deg, #001f3f, #004080);
        }
    ")),
    
    sidebarMenu(
      menuItem("📊 Revenue Summary", tabName = "revenue", icon = icon("chart-line")),
      menuItem("🍉 Fruit Analysis", tabName = "fruit", icon = icon("apple-alt")),
      menuItem("👥 Salesperson Analysis", tabName = "sales", icon = icon("users")),
      menuItem("🧠 Advanced Insights", tabName = "insights", icon = icon("brain")),
      menuItem("🔮 Prediction", tabName = "predict", icon = icon("magic")),
      menuItem("📄 Dataset", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
        .box {
            border-radius: 15px !important;
            backdrop-filter: blur(6px);
            background: rgba(255, 255, 255, 0.8) !important;
        }
        .content-wrapper {
            background: linear-gradient(135deg, #e0f7ff, #f5ffff);
        }
    "))),
    
    tabItems(
      
      # ------------------------------------------------------------
      # Revenue Summary
      # ------------------------------------------------------------
      
      tabItem(tabName = "revenue",
              
              h2("📊 Yearly & Monthly Revenue Summary",
                 style="font-weight:bold; color:#003366;"),
              
              fluidRow(
                valueBox(paste("₹", max_year$Revenue),
                         paste("Highest Revenue Year:", max_year$Year),
                         color = "green", width = 4, icon = icon("arrow-up")),
                
                valueBox(paste("₹", min_year$Revenue),
                         paste("Lowest Revenue Year:", min_year$Year),
                         color = "red", width = 4, icon = icon("arrow-down")),
                
                valueBox(paste("₹", sum(yearly_revenue$Revenue)),
                         "Total Revenue (All Years)",
                         color = "blue", width = 4, icon = icon("wallet"))
              ),
              
              box(title = "📈 Yearly Revenue Trend", width = 12, status="primary",
                  plotOutput("year_trend"))
      ),
      
      # ------------------------------------------------------------
      # Fruit Analysis
      # ------------------------------------------------------------
      
      tabItem(tabName = "fruit",
              h2("🍉 Fruit-Based Revenue & Quantity Analysis",
                 style="font-weight:bold; color:#003366;"),
              
              fluidRow(
                box(title = "💰 Revenue by Fruit", width = 6, status="warning",
                    plotOutput("fruit_rev_plot")),
                
                box(title = "📦 Quantity Sold by Fruit", width = 6, status="info",
                    plotOutput("fruit_qty_plot"))
              )
      ),
      
      # ------------------------------------------------------------
      # Salesperson Analysis
      # ------------------------------------------------------------
      
      tabItem(tabName = "sales",
              h2("👥 Salesperson Performance Analysis",
                 style="font-weight:bold; color:#003366;"),
              
              fluidRow(
                box(title = "💼 Revenue by Salesperson", width = 6, status="success",
                    plotOutput("sales_rev_plot")),
                
                box(title = "📦 Quantity Sold by Salesperson", width = 6, status="danger",
                    plotOutput("sales_qty_plot"))
              )
      ),
      
      # ------------------------------------------------------------
      # Advanced Insights
      # ------------------------------------------------------------
      
      tabItem(tabName = "insights",
              
              h2("🧠 Advanced Insights", style="font-weight:bold; color:#003366;"),
              
              valueBox(round(correlation_value, 3),
                       "Correlation: Quantity vs Revenue",
                       color = "purple", width = 4,
                       icon = icon("project-diagram")),
              
              box(title = "🔍 Quantity vs Revenue Scatter Plot",
                  width = 12, status="primary",
                  plotOutput("scatter_plot"))
      ),
      
      # ------------------------------------------------------------
      # Prediction Module
      # ------------------------------------------------------------
      
      tabItem(tabName = "predict",
              
              h2("🔮 Revenue Prediction (Random Forest)",
                 style="font-weight:bold; color:#003366;"),
              
              fluidRow(
                
                box(width = 4, status="warning", solidHeader=TRUE,
                    title = "📥 Enter Input Values",
                    
                    selectInput("fruit_input", "Select Fruit:",
                                choices = levels(fruit_sales$Fruit)),
                    
                    selectInput("month_input", "Select Month:",
                                choices = levels(fruit_sales$Month)),
                    
                    numericInput("qty_input", "Enter Quantity (Boxes):",
                                 value = 10, min = 1),
                    
                    selectInput("sales_input", "Select Salesperson:",
                                choices = levels(fruit_sales$SalesPerson)),
                    
                    actionButton("predict_btn", "Predict Revenue",
                                 class = "btn btn-success btn-lg")
                ),
                
                box(width = 8, status="info", solidHeader=TRUE,
                    title = "📤 Predicted Revenue Output",
                    h2(textOutput("prediction_output"),
                       style="font-weight:bold; color:#2c3e50;"))
              )
      ),
      
      # ------------------------------------------------------------
      # Dataset Display
      # ------------------------------------------------------------
      
      tabItem(tabName = "table",
              h2("📄 Dataset Preview",
                 style="font-weight:bold; color:#003366;"),
              box(width = 12, status="primary",
                  dataTableOutput("data_table"))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output) {
  
  output$year_trend <- renderPlot({
    ggplot(yearly_revenue, aes(x = Year, y = Revenue)) +
      geom_line(color="blue", size=1.2) +
      geom_point(size=3, color="red") +
      labs(title="Yearly Revenue Trend", x="Year", y="Revenue")
  })
  
  output$fruit_rev_plot <- renderPlot({
    ggplot(fruit_revenue, aes(x = Fruit, y = Revenue, fill = Fruit)) +
      geom_bar(stat="identity") +
      labs(title="Revenue by Fruit", x="Fruit", y="Revenue")
  })
  
  output$fruit_qty_plot <- renderPlot({
    ggplot(fruit_quantity,
           aes(x = Fruit, y = Quantity..number.of.boxes., fill=Fruit)) +
      geom_bar(stat="identity") +
      labs(title="Quantity Sold by Fruit", x="Fruit", y="Quantity")
  })
  
  output$sales_rev_plot <- renderPlot({
    ggplot(salesperson_revenue,
           aes(x = SalesPerson, y = Revenue, fill=SalesPerson)) +
      geom_bar(stat="identity") +
      labs(title="Revenue by Salesperson", x="Salesperson", y="Revenue")
  })
  
  output$sales_qty_plot <- renderPlot({
    ggplot(salesperson_quantity,
           aes(x = SalesPerson, y = Quantity..number.of.boxes., fill=SalesPerson)) +
      geom_bar(stat="identity") +
      labs(title="Quantity Sold by Salesperson", x="Salesperson", y="Quantity")
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(fruit_sales,
           aes(x = Quantity..number.of.boxes., y = Revenue, color = Fruit)) +
      geom_point(size=3) +
      labs(title="Quantity vs Revenue", x="Quantity (boxes)", y="Revenue")
  })
  
  output$data_table <- renderDataTable({
    fruit_sales
  })
  
  observeEvent(input$predict_btn, {
    
    new_data <- data.frame(
      Fruit = factor(input$fruit_input, levels = levels(fruit_sales$Fruit)),
      Month = factor(input$month_input, levels = levels(fruit_sales$Month)),
      Quantity..number.of.boxes. = input$qty_input,
      SalesPerson = factor(input$sales_input, levels = levels(fruit_sales$SalesPerson))
    )
    
    predicted_value <- predict(rf_model, new_data)
    
    output$prediction_output <- renderText({
      paste("Predicted Revenue: ₹", round(predicted_value, 2))
    })
  })
}

# Run the App
shinyApp(ui, server)
