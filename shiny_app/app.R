
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)

df <- read_csv("/Users/catheacarey/Documents/GitHub/Portfolio/Portfolio/data/realistic_ehr.csv") %>% 
  mutate(visit_date = ymd(visit_date),
         month = floor_date(visit_date, "month"))

ui <- fluidPage(
  titlePanel("EHR Dashboard — Clinic Metrics (Starter)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("clinic", "Clinic ID", choices = sort(unique(df$clinic_id)), selected = unique(df$clinic_id)[1]),
      dateRangeInput("daterange", "Date range", start = min(df$visit_date), end = max(df$visit_date)),
      selectInput("metric", "Metric", choices = c("bup_rate","opioid_rate"), selected = "opioid_rate")
    ),
    mainPanel(
      plotOutput("tsPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    req(input$clinic)
    df %>% filter(clinic_id == as.numeric(input$clinic),
                  visit_date >= input$daterange[1],
                  visit_date <= input$daterange[2]) %>%
      group_by(month) %>%
      summarize(bup_rate = mean(bup_init, na.rm=TRUE),
                opioid_rate = mean(opioid_rx, na.rm=TRUE),
                n_visits = n()) %>%
      ungroup()
  })
  
  output$tsPlot <- renderPlot({
    dat <- filtered()
    ggplot(dat, aes(x=month, y=.data[[input$metric]])) +
      geom_line() + geom_point() +
      labs(x="Month", y=input$metric, title=paste("Clinic", input$clinic, "-", input$metric))
  })
  
  output$summaryTable <- renderTable({
    dat <- filtered()
    dat %>% arrange(desc(month)) %>% head(10)
  })
}

shinyApp(ui, server)
