#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Neighborhood Diversity"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("city",
                        "City Name:",
                        choices = unique(data_by_year$metro_name)),
            sliderInput("span",
                        "Span Parameter:",
                        min = 0,
                        max = 1,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("scatter"),
          plotlyOutput("map"),
          plotlyOutput("bar")
        )
    )
)


server <- function(input, output) {
  output$scatter <- renderPlotly({
    p <- data_by_dist %>%
      filter(metro_name == input$city) %>%
      ggplot(aes(x = entropy, y = distmiles)) +
      geom_point(aes(key = tract_id)) +
      geom_smooth(span = input$span, se = FALSE, method = "loess") +
      theme_classic()

    ggplotly(p, source = "plotly_scatterplot") %>%
      event_register("plotly_selected")
  })

  output$map <- renderPlotly({
    p <- data_by_dist %>%
      filter(metro_name == input$city) %>%
      ggplot() +
      geom_sf(aes(fill = entropy)) +
      theme_classic()
  }
  )

  output$bar <- renderPlotly({
    p <- data_by_dist %>%
      pivot_longer(
        cols = c("aian", "asian", "black", "hispanic", "two_or_more", "white"),
        names_to = "race",
        values_to = "count"
      ) %>%
      filter(metro_name == input$city) %>%
      group_by(race) %>%
      summarize(num = sum(count)) %>%
      ggplot(aes(x = race, y = num, fill = race)) +
      geom_col() +
      theme_classic()
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
