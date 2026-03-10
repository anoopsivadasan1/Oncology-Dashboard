# install.packages("styler")


library(dplyr)
library(tidyr)


data()

View(sleep)

sum_sleep <- sleep %>%
  group_by(group) %>%
  summarize(
    n = n(),
    Mean = mean(extra),
    SD = sd(extra),
    Min. = min(extra),
    Max. = max(extra)
  ) %>%
  mutate(MeanSD = paste(Mean, " (", round(SD, 3), ")", sep = "")) %>%
  mutate(
    n = as.character(n),
    Min. = as.character(Min.),
    Max. = as.character(Max.)
  ) %>%
  mutate(group_text = ifelse(group == 1, "Group A", "Group B")) %>%
  select(group_text, n, MeanSD, Min., Max.) %>%
  # Transpose (pivot) data based on specific groups
  
  pivot_longer(
    #  where(is.numeric),
    #  select("n","MeanSD", "Min.", "Max."),
    cols = c("n", "MeanSD", "Min.", "Max."),
    names_to = "Label",
    values_to = "Stats"
  )

View(sum_sleep)



ui <- fluidPage(
  # Title of the app
  h1("A Basic Reactive Shiny App"),
  
  # Input Functions
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cyl_sel",
                         label = "Select Group",
                         choices = unique(sum_sleep$group_text),
                         selected = unique(sum_sleep$group_text))
    ),
    
    # Output Functions
    mainPanel(
      dataTableOutput("cartable")
    )
  )
)

server <- function(input, output) {
  
  # Output Object and Render function
  output$cartable <- renderDataTable({
    
    # Filter data based on selected input
    sum_sleep %>%
      dplyr::filter(group_text %in% input$cyl_sel)
  })
}

# Run the app
shinyApp(ui, server)