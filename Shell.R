# ------------------------------------------------
# ---- Shiny App: Tumor Growth + Kaplan–Meier ----
# ------------------------------------------------

library(shiny)

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("Oncology Dashboard: Tumor Growth & Survival Analysis"),
  
  # global style
  tags$style("body, label, .selectize-input { font-family: 'Courier New'; }"),
  
  tabsetPanel(
    type = "tabs",
    
    # ---- TAB 1: Tumor Growth ----
    tabPanel("Tumor Growth"),
    
    # ---- TAB 2: Kaplan–Meier ----
    tabPanel(
      "Kaplan–Meier",
      #h3("Kaplan–Meier Survival Analysis Module")
    )
    
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
}

# ---- RUN APP ----
shinyApp(ui, server)
