# ------------------------------------------------
# ---- Shiny App: Tumor Growth + Kaplan–Meier ----
# ------------------------------------------------

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)

# ---- Load data ----

file_path <- "sample_disposition_dashboard_data.xlsx"

berg <- read_excel(file_path, sheet = "Berg")
juni <- read_excel(file_path, sheet = "Juni")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Oncology Dashboard: Tumor Growth & Survival Analysis"),
  
  tabsetPanel(
    type = "tabs",
    
    
    # ---- TAB 1: Tumor Growth ----
    tabPanel("Tumor Growth",
             fluidRow(
               column(
                 width = 2,
                 wellPanel(
                   selectInput("tnfFilter_tumor", "Select TNF Status:",
                               choices = c("All", "Yes", "No"),
                               selected = "All"),
                   helpText("Displays tumor growth over visits by TNF status."),
                   tags$style("body, label, .selectize-input { font-family: 'Courier New'; }")
                 )
               ),
               
               # Main content area: plot + table side by side
               column(
                 width = 9,
                 fluidRow(
                   column(
                     width = 8,
                     plotOutput("tumorPlot", height = "500px")
                   ),
                   column(
                     width = 4.5,
                     tags$div(
                       style = "font-weight:bold; text-decoration:underline; 
                     font-family:'Courier New'; font-size:16px; 
                     margin-bottom:8px; text-align:center;",
                       "Summary Statistics"
                     ),
                     tableOutput("summaryTable")
                   )
                 )
               )
             )
    ),
    
    
    # ---- TAB 2: Kaplan–Meier ----
    tabPanel("Kaplan–Meier",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput("tnfFilter_km", "Select TNF Status:",
                             choices = c("All", "Yes", "No"),
                             selected = "All")
               ),
               mainPanel(
                 width = 9,
                 plotOutput("kmPlot", height = "500px")
               )
             )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- 1. Load Tumor Growth Data ----
  tumor_data <- reactive({
    read_excel("C:/Users/cex/OneDrive/Desktop/Shiny/tumor_growth_data.xlsx", sheet = "Tumor Growth")
  })
  
  # ---- 2. Load Kaplan–Meier Data ----
  km_data <- reactive({
    read_excel("C:/Users/cex/OneDrive/Desktop/Shiny/km_data.xlsx", sheet = "KM_Data")
  })
  
  # ---- TAB 1: Tumor Growth ----
  filtered_tumor <- reactive({
    df <- tumor_data()
    if (input$tnfFilter_tumor != "All") {
      df <- df %>% filter(TNF_Status == input$tnfFilter_tumor)
    }
    df
  })
  
  output$tumorPlot <- renderPlot({
    df <- filtered_tumor()
    subj_count <- n_distinct(df$Subject)
    
    ggplot(df, aes(x = Visit, y = Change_From_Baseline, group = Subject, color = Subject)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 2) +
      geom_hline(yintercept = c(20, 0, -30),
                 linetype = "dotted",
                 color = c("red", "gray", "blue"),
                 linewidth = 1) +
      scale_y_continuous(breaks = seq(-30, 30, by = 10), limits = c(-35, 35)) +
      theme_minimal(base_size = 14, base_family = "Courier New") +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +
      labs(
        title = paste0("Tumor Growth: Change from Baseline\nTNF = ",
                       input$tnfFilter_tumor, " (N=", subj_count, ")"),
        x = "Visit",
        y = "Change from Baseline"
      )
  })
  
  output$summaryTable <- renderTable({
    filtered_tumor() %>%
      filter(Visit != "Baseline") %>%
      group_by(Visit) %>%
      summarise(
        Mean = mean(Change_From_Baseline, na.rm = TRUE),
        Median = median(Change_From_Baseline, na.rm = TRUE),
        SD = sd(Change_From_Baseline, na.rm = TRUE),
        #N = n_distinct(Subject)
      ) %>%
      arrange(Visit)
  }, digits = 2)
  
  # ---- TAB 2: Kaplan–Meier ----
  filtered_km <- reactive({
    df <- km_data()
    if (input$tnfFilter_km != "All") {
      df <- df %>% filter(TNF_Status == input$tnfFilter_km)
    }
    df
  })
  
  output$kmPlot <- renderPlot({
    df <- filtered_km()
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for the selected filter", cex = 1.3, 
           family = "Courier New")
      return()
    }
    
    fit <- survfit(Surv(Time, Event) ~ TNF_Status, data = df)
    
    ggsurvplot(
      fit,
      data = df,
      risk.table = TRUE,
      pval = TRUE,
      conf.int = TRUE,
      ggtheme = theme_minimal(base_size = 14, base_family = "Courier New"),
      legend.title = "TNF Status",
      legend.labs = levels(factor(df$TNF_Status)),
      title = "Kaplan–Meier Survival Curve",
      subtitle = paste0(
        "TNF Filter: ", input$tnfFilter_km,
        " | Subjects (N=", n_distinct(df$Subject), ")"
      ),
      xlab = "Time (Weeks)",
      ylab = "Survival Probability",
      palette = c("steelblue", "darkred")
    )
  })
  
}

# ---- RUN APP ----
shinyApp(ui, server)
