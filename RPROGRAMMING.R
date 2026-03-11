library(shiny)
library(shinydashboard)
library(ggplot2)
setwd("D:/Rpro")
fruit_sales <- read.csv("FruitSalesDataset.csv", stringsAsFactors = FALSE)
fruit_sales$Revenue <- as.numeric(gsub("[$,]", "", fruit_sales$Revenue))
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
avg_revenue <- aggregate(Revenue ~ Fruit + Month, data = fruit_sales, mean)
correlation_value <- cor(fruit_sales$Revenue, fruit_sales$Quantity..number.of.boxes.)
ui <- dashboardPage(
  dashboardHeader(title = "Fruit Sales Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Revenue Summary", tabName = "revenue", icon = icon("chart-line")),
      menuItem("Fruit Analysis", tabName = "fruit", icon = icon("apple-alt")),
      menuItem("Salesperson Analysis", tabName = "sales", icon = icon("users")),
      menuItem("Advanced Insights", tabName = "insights", icon = icon("brain")),
      menuItem("Dataset", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "revenue",
              h2("Yearly & Monthly Revenue Summary"),
              
              fluidRow(
                valueBox(paste("₹", max_year$Revenue), paste("Highest Revenue Year:", max_year$Year),
                         color = "green", width = 4),
                valueBox(paste("₹", min_year$Revenue), paste("Lowest Revenue Year:", min_year$Year),
                         color = "red", width = 4),
                valueBox(paste("₹", sum(yearly_revenue$Revenue)), "Total Revenue (All Years)",
                         color = "blue", width = 4)
              ),
              
              box(title = "Yearly Revenue Trend", width = 12,
                  plotOutput("year_trend"))
      ),
      
      tabItem(tabName = "fruit",
              h2("Fruit-Based Revenue and Quantity Analysis"),
              
              fluidRow(
                box(title = "Revenue by Fruit", width = 6, plotOutput("fruit_rev_plot")),
                box(title = "Quantity Sold by Fruit", width = 6, plotOutput("fruit_qty_plot"))
              )
      ),
      

      tabItem(tabName = "sales",
              h2("Salesperson Performance Analysis"),
              
              fluidRow(
                box(title = "Revenue by Salesperson", width = 6, plotOutput("sales_rev_plot")),
                box(title = "Quantity Sold by Salesperson", width = 6, plotOutput("sales_qty_plot"))
              )
      ),
      
      tabItem(tabName = "insights",
              h2("Advanced Insights"),
              
              valueBox(round(correlation_value, 3),
                       "Correlation: Quantity vs Revenue",
                       color = "purple"),
              
              box(title = "Quantity vs Revenue Scatter Plot", width = 12,
                  plotOutput("scatter_plot"))
      ),

      tabItem(tabName = "table",
              h2("Dataset Preview"),
              dataTableOutput("data_table")
      )
    )
  )
)


server <- function(input, output) {
  
  output$year_trend <- renderPlot({
    ggplot(yearly_revenue, aes(x = Year, y = Revenue)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(size = 3, color = "red") +
      labs(title = "Yearly Revenue Trend", x = "Year", y = "Revenue")
  })
  
  output$fruit_rev_plot <- renderPlot({
    ggplot(fruit_revenue, aes(x = Fruit, y = Revenue, fill = Fruit)) +
      geom_bar(stat="identity") +
      labs(title="Revenue by Fruit", x="Fruit", y="Revenue")
  })
  
  output$fruit_qty_plot <- renderPlot({
    ggplot(fruit_quantity, aes(x = Fruit, y = Quantity..number.of.boxes., fill = Fruit)) +
      geom_bar(stat="identity") +
      labs(title="Quantity Sold by Fruit", x="Fruit", y="Quantity")
  })
  
  output$sales_rev_plot <- renderPlot({
    ggplot(salesperson_revenue, aes(x = SalesPerson, y = Revenue, fill = SalesPerson)) +
      geom_bar(stat="identity") +
      labs(title="Revenue by Salesperson", x="Salesperson", y="Revenue")
  })
  
  output$sales_qty_plot <- renderPlot({
    ggplot(salesperson_quantity, aes(x = SalesPerson, y = Quantity..number.of.boxes., fill = SalesPerson)) +
      geom_bar(stat="identity") +
      labs(title="Quantity Sold by Salesperson", x="Salesperson", y="Quantity")
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(fruit_sales, aes(x = Quantity..number.of.boxes., y = Revenue, color = Fruit)) +
      geom_point(size = 3) +
      labs(title="Quantity vs Revenue", x="Quantity (boxes)", y="Revenue")
  })
  
  output$data_table <- renderDataTable({
    fruit_sales
  })
}

shinyApp(ui, server)
