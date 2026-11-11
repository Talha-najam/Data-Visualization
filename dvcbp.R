library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

# Load Dataset
data <- read_csv("credit_risk_dataset.csv")

# Dashboard UI
ui <- dashboardPage(

  dashboardHeader(title = "FinTech Credit Risk Analytics"),

  dashboardSidebar(disable = TRUE),

  dashboardBody(

    fluidRow(
      valueBoxOutput("income"),
      valueBoxOutput("loan"),
      valueBoxOutput("default")
    ),

    fluidRow(
      box(
        title = "Interest Rate Vs Loan Percent Income",
        width = 12,
        plotlyOutput("scatter1", height = 300)
      )
    ),

    fluidRow(
      box(
        title = "Income Vs Loan Amount",
        width = 6,
        plotlyOutput("scatter2", height = 300)
      ),

      box(
        title = "Default Rate By Loan Intent",
        width = 6,
        plotlyOutput("bar1", height = 300)
      )
    ),

    fluidRow(
      box(
        title = "Loan Amount By Loan Grade",
        width = 12,
        plotlyOutput("bar2", height = 300)
      )
    )
  )
)

# Dashboard Server
server <- function(input, output) {

  # Average Annual Income
  output$income <- renderValueBox({
    valueBox(
      round(mean(data$person_income, na.rm = TRUE)),
      "Average Annual Income",
      icon = icon("money"),
      color = "blue"
    )
  })

  # Average Loan Amount
  output$loan <- renderValueBox({
    valueBox(
      round(mean(data$loan_amnt, na.rm = TRUE)),
      "Average Loan Amount",
      icon = icon("credit-card"),
      color = "green"
    )
  })

  # Default Rate
  output$default <- renderValueBox({
    valueBox(
      round(mean(data$loan_status, na.rm = TRUE), 4),
      "Default Rate",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  # Interest Rate Vs Loan Percent Income
  output$scatter1 <- renderPlotly({

    p <- ggplot(data, aes(
      x = loan_percent_income,
      y = loan_int_rate,
      color = loan_grade
    )) +
      geom_point(alpha = 0.5) +
      labs(
        x = "Loan Percent Income",
        y = "Loan Interest Rate",
        color = "Loan Grade"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # Income Vs Loan Amount
  output$scatter2 <- renderPlotly({

    p <- ggplot(data, aes(
      x = person_income,
      y = loan_amnt,
      color = factor(loan_status)
    )) +
      geom_point(alpha = 0.5) +
      labs(
        x = "Annual Income",
        y = "Loan Amount",
        color = "Loan Status"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # Default Rate By Loan Intent
  output$bar1 <- renderPlotly({

    df <- data %>%
      group_by(loan_intent) %>%
      summarise(
        default_rate = mean(loan_status, na.rm = TRUE)
      )

    p <- ggplot(df, aes(
      x = reorder(loan_intent, default_rate),
      y = default_rate
    )) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        x = "Loan Intent",
        y = "Default Rate"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # Loan Amount By Loan Grade
  output$bar2 <- renderPlotly({

    df <- data %>%
      group_by(loan_grade) %>%
      summarise(
        total_loan = sum(loan_amnt, na.rm = TRUE)
      )

    p <- ggplot(df, aes(
      x = loan_grade,
      y = total_loan,
      fill = loan_grade
    )) +
      geom_col() +
      labs(
        x = "Loan Grade",
        y = "Total Loan Amount"
      ) +
      theme_minimal()

    ggplotly(p)
  })
}

# Run Dashboard
shinyApp(ui = ui, server = server)
