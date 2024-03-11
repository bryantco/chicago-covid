# Setup ------------------------------------------------------------------------
library(shiny)
library(plotly)

source("scripts/gen_week_year_level_df.R")

# Read and process day level df ------------------------------------------------
covid_day_level_df <- read_csv("data/covid_chicago_2024_03_07.csv")
covid_year_week_level_df <- gen_year_week_level_df(covid_day_level_df)

# SHINY FRONT END --------------------------------------------------------------
ui <- fluidPage(
  sliderInput(
    "date_input", 
    "Select date range",
    value = c(
      min(covid_year_week_level_df$week_to_plot),
      max(covid_year_week_level_df$week_to_plot)
    ),
    min = min(covid_year_week_level_df$week_to_plot),
    max = max(covid_year_week_level_df$week_to_plot),
    step = 30,
    width = "400px",
    timeFormat = "%b %Y"
  ),
  plotlyOutput("covid_week_level_plot", width = "100%", height = "600px")
)


# SHINY BACK END ---------------------------------------------------------------
server <- function(input, output, session) {
  
  output$covid_week_level_plot <- renderPlotly({
    
    min_date_input <- input$date_input[1]
    max_date_input <- input$date_input[2]
    
    min_date_input_plot <- floor_date(min_date_input, unit = "months")
    max_date_input_plot <- ceiling_date(max_date_input, unit = "months") - 1
    
    covid_year_week_level_df_to_plot <- covid_year_week_level_df %>% 
      filter(week_to_plot >= min_date_input_plot & week_to_plot <= max_date_input_plot)

    
    p <- ggplot(covid_year_week_level_df_to_plot) +
      geom_col(aes(x = week_to_plot, y = cases_total),
               fill = "#872657",
               color = "white",
               width = 7) +
      theme_minimal() +
      labs(x = "", y = "Weekly Cases") +
      scale_x_date(labels = scales::label_date_short(), date_breaks = "2 months",
                   date_minor_breaks = "1 month") +
      scale_y_continuous(expand = c(0, 1)) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = "gray"),
        axis.text.x = element_text(angle = 45),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.ticks.length.x = unit(0.3, "cm")
      ) + 
      ggtitle("Weekly COVID-19 Cases")
    
    ggplotly_title <- paste0("Weekly COVID-19 Cases", "<br>",
                             "<sup>", min_date_input_plot, " through ", max_date_input_plot,
                             "</sup>", "<br>")
    ggplotly(p) %>% 
      layout(title = list(text = ggplotly_title))
  })
}


shinyApp(ui, server)

