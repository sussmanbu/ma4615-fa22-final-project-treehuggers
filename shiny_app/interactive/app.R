#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Packages
library(tidyverse)
library(sf)
library(USAboundaries)
library(tmap)
library(shiny)

# Load Data
df <- load(here::here("shiny_app/interactive/map_race_ethnicity.RData"))

# Load Various Mapping Requirements
epsg_CA <- 26943
epsg_wgs84 <- 4326
CA_state <- USAboundaries::us_states() %>% filter(name == "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_CA)
CA_counties <- USAboundaries::us_counties(states = "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_CA) %>% 
  select(-namelsad, -state_name, -state_abbr, -jurisdiction_type) %>% 
  mutate("county_name" = str_to_upper(name)) %>% select(-name)

# Create Variable Choices for Reactive Filter
race_choices <- c("Black", "Hispanic", "White")

county_choices <- df %>% pull(county_name) %>% unique() %>% sort()


ui <- fluidPage(
  
  # Application title
  titlePanel("Asthma Hospitalizations by County"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "race",
                  label = "Race/Ethnicity",
                  choices = race_choices
      )),
  mainPanel(
    plotOutput("map")
  ))
)


server <- function(input, output) {
  
  output$map <- renderPlot({
    df %>%
      filter(race_ethnicity == input$race) %>%
      group_by(county_name) %>%
      tm_shape() + tm_polygons(col = "age_adjusted_hospitalization_rate", palette = "viridis") +
      tm_shape(CA_counties) + tm_borders() +
      tm_shape(CA_state) + tm_borders(lwd = 2)
  })
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
