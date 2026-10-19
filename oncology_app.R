# ------------------------------------------------
# ---- Oncology Dashboard    ---------------------
# ------------------------------------------------


# ------Load Libraries ---------------------------

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(lubridate)
library(ggnewscale)


# -------------------------
# Load Data
# -------------------------

file_path <- "oncology_adam.xlsx"

# ---- 1. Tumor Growth Data ----

tumor_data <- reactive({read_excel(file_path, sheet = "Tumor Growth")})

# ---- 2. Kaplan–Meier Data ----

km_data <- reactive({read_excel(file_path, sheet = "KM_Data")})

# ---- 3. Overall Response and DOR ----


adsl <- read_excel(file_path, sheet = "ADSL")
df   <- read_excel(file_path, sheet = "ADTTE")

adeff <- read_excel(file_path, sheet = "ADRS") %>%
  filter(PARAMCD == "BOR") %>%
  mutate(BOR = AVALC) %>%
  select(USUBJID, BOR)

adrs <- read_excel(file_path, sheet = "ADRS")

# ----------------------------------------
# Process data: Derive PR/PD Dates for DOR
# ----------------------------------------

dor <- adrs %>%
  mutate(ADT = as.Date(as.POSIXct(ADT, origin = "1970-01-01", tz = "UTC"))) %>%
  group_by(USUBJID) %>%
  mutate(
    first_pr_date = if (any(AVALC %in% c("CR", "PR"))) {
      min(ADT[AVALC %in% c("CR", "PR")], na.rm = TRUE)
    } else {
      NA_Date_
    },
    pd_date = if (any(AVALC == "PD")) {
      min(ADT[AVALC == "PD"], na.rm = TRUE)
    } else {
      NA_Date_
    },
    last_visit_date = max(ADT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(PARAMCD == "DOR") %>%
  left_join(
    adsl %>% mutate(RFSTDT = as.Date(RFSTDT)),
    by = "USUBJID"
  ) %>%
  mutate(
    start = as.numeric(first_pr_date - RFSTDT),
    end = case_when(
      !is.na(pd_date) ~ as.numeric(pd_date - RFSTDT),
      TRUE            ~ as.numeric(last_visit_date - RFSTDT)
    )
  ) %>%
  select(USUBJID, start, end, pd_date, RFSTDT, last_visit_date)


# ------------------
# Prepare OS Dataset
# ------------------

os_df <- df %>%
  filter(PARAMCD == "OS") %>%
  left_join(adeff, by = "USUBJID") %>%
  left_join(
    dor %>% filter(!is.na(start) & !is.na(end)),
    by = "USUBJID"
  ) %>%
  mutate(
    TRTA = TREATMENT,
    CNSR = factor(CNSR, levels = c(0,1), labels = c("Death","Censored")),
    BOR  = factor(BOR, levels = c("CR","PR","SD","PD","NE")),
    enddt = case_when(
      !is.na(pd_date) ~ as.numeric(pd_date - RFSTDT),
      TRUE            ~ as.numeric(as.Date(ADT) - RFSTDT)
    )
  )

# -------------------------
# Color Aesthetics
# -------------------------

# ---- For Treatment ----


trt_colors <- c(
  "Drug A" = "#6BAED6",
  "Drug B" = "#FDAE6B"
)

# ---- For Subject Status ----

status_colors <- c(
  "Death"    = "#D62728",
  "Censored" = "#000000"
)

# ---- For BOR ----

bor_colors <- c(
  "CR" = "#000080",
  "PR" = "darkgreen",
  "SD" = "orange",
  "PD" = "red",
  "NE" = "grey"
)




# ---- UI ----

ui <- fluidPage(
  titlePanel("Oncology Dashboard: Tumor Growth, Survival Analysis and Response"),
  
  tabsetPanel(
    type = "tabs",
    
    
    # ---- TAB 1: Tumor Growth ----
    
    tabPanel("Spider Plot",
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
    ),
    
    
    # ---- TAB 3: Swimmer–Plot for Overall Response and DOR embedment ----
    
    tabPanel(
      title = "Swimmer Plot",
      
      sidebarLayout(
        
        sidebarPanel(
          width = 2,
          
          selectInput(
            inputId = "trta",
            label = "Select Treatment:",
            choices = c("All", sort(unique(os_df$TRTA))),
            selected = "All"
          ),
          
          checkboxInput(
            inputId = "show_dor",
            label = "DOR",
            value = FALSE
          )
        ),
        
        mainPanel(
          width = 9,
          plotOutput("swimmerPlot", height = "700px")
        )
      )
    )
    
    
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  
  
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
  
  
  # ---- TAB 3: Seimmer–Plot ----
  
  filtered_data <- reactive({
    data <- os_df
    if (input$trta != "All") {
      data <- data %>% filter(TRTA == input$trta)
    }
    data %>%
      arrange(AVAL) %>%
      mutate(USUBJID = factor(USUBJID, levels = unique(USUBJID)))
  })
  
  output$swimmerPlot <- renderPlot({
    
    plot_df <- filtered_data()
    
    ggplot(plot_df, aes(y = USUBJID)) +
      
      # OS swimmer bars
      
      geom_segment(
        aes(x = 0, xend = AVAL, yend = USUBJID, color = TRTA),
        linewidth = 2.5,
        lineend = "butt"
      ) +
      
      scale_color_manual(values = trt_colors, name = "Treatment") +
      
      # -------------------------
    # DOR OVERLAY
    # -------------------------
    
    {
      if (isTRUE(input$show_dor)) {
        geom_segment(
          data = plot_df %>% filter(!is.na(start) & !is.na(enddt)),
          aes(x = start, xend = enddt, y = USUBJID, yend = USUBJID),
          inherit.aes = FALSE,
          linewidth = 2.5,
          color = "#808000",
          lineend = "butt"
        )
      }
    } +
      
      ggnewscale::new_scale_color() +
      
      geom_text(
        aes(x = -10, label = BOR, color = BOR),
        hjust = 1.5,
        size = 3.5,
        family = "Courier New"
      ) +
      
      
      # -------------------------
    # Legend for Treatment
    # -------------------------
    
    scale_color_manual(values = bor_colors, guide = "none") +
      
      # --------------------------------------
    # Overlay Subject status' shapes in plot
    # --------------------------------------
    
    geom_point(
      aes(x = AVAL, shape = CNSR, fill = CNSR),
      size = 3,
      color = "black",
      stroke = 0.2
    ) +
      
      
      # -------------------------
    # Legend for Subject Status
    # -------------------------
    
    scale_shape_manual(
      name = "Subject Status",
      values = c("Death" = 24, "Censored" = 21)
    ) +
      scale_fill_manual(
        name = "Subject Status",
        values = status_colors
      ) +
      
      # 1. Controls spacing above and below the bars
      #    The expand = expansion(add = c(0.6, 1.2)) part adds padding:
      #    - 0.6 units at the bottom
      #    - 1.2 units at the top
      #    This prevents the first and last subjects from being squashed against the plot border.
      #    Without this, the top and bottom subjects would sit right on the frame.
      # 
      # 2. Ensures the Y‑axis treats USUBJID as discrete categories
      #    Because USUBJID is a factor, scale_y_discrete():
      #    - Displays each subject as a separate row
      #    - Keeps the order you set earlier:
      
      scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
      
      labs(
        title = "BOR",
        x = "Overall Survival Duration (Days)",
        y = "Subject ID"
      ) +
      
      theme_minimal(base_family = "Courier New") +
      theme(
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(face = "bold", size = 11),
        axis.title.y = element_text(face = "bold", size = 11),
        legend.text  = element_text(size = 9, color = "black"),
        legend.title = element_text(face = "bold", size = 10),
        plot.title   = element_text(face = "bold", size = 10, hjust = 0.01),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "right"
      )
    
  }, res = 120)
  
  
}

# ---- RUN APP ----
shinyApp(ui, server)