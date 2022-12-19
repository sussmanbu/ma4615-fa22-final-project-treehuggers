#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Packages
library(shiny)
library(tidyverse)
# library(sf)
# library(tmap)
# library(raster)

# Load Data
load("parks_asthmaCA_race_ethnicity.RData")
df <- parks_asthmaCA_race_ethnicity

# Load Various Mapping Requirements
#CA_state <- load(here::here("shiny_app/interactive/CA_state.RData"))
#CA_counties <- load(here::here("shiny_app/interactive/CA_counties.RData"))

# Create Variable Choices for Reactive Filter
race_choices <- c("Black", "Hispanic", "White")

county_choices <- df %>% pull(county_name) %>% unique() %>% sort()


ui <- fluidPage(
  
  # Application title
  titlePanel("Asthma Hospitalizations by County"),
  
  sidebarLayout(
    sidebarPanel(
      # selectInput(inputId = "race",
      #             label = "Race/Ethnicity",
      #             choices = race_choices
      # ),
      selectInput(inputId = "county",
                  label = "County",
                  choices = county_choices)
      ),
  mainPanel(
    # plotOutput("map"),
    plotOutput("barPlot")
  ))
)

server <- function(input, output) {
  # output$map <- renderPlot({
  #  st_sf(df %>% filter(race_ethnicity == input$race) %>%
  #          group_by(county_name)) %>% 
  #     tm_shape() + tm_polygons(col = "age_adjusted_hospitalization_rate", palette = "viridis") 
  #   #+
  #     # tm_shape(CA_counties) + tm_borders() +
  #     # tm_shape(CA_state) + tm_borders(lwd = 2)
  # })
  output$barPlot <- renderPlot({
    df %>%
      filter(county_name == input$county) %>%
      ggplot(aes(y = race_ethnicity, x = age_adjusted_hospitalization_rate, fill = race_ethnicity)) +
      geom_col(position = "dodge") +
      labs(title = "Age-Adjusted Hospitalization Rate by Race/Ethnicity in Selected County",
           x = "Age-Adjusted Hospitalization Rate",
           y = "Race/Ethnicity", fill = "Race/Ethnicity")
  })
}

shinyApp(ui = ui, server = server)
