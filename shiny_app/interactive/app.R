#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(sf)
library(USAboundaries)
library(tmap)
library(shiny)

map_kids_v_adults <- load(here::here("shiny_app/interactive/map_kids_v_adults.RData"))
map_race_ethnicity <- load(here::here("shiny_app/interactive/map_race_ethnicity.RData"))

epsg_CA <- 26943
epsg_wgs84 <- 4326
CA_state <- USAboundaries::us_states() %>% filter(name == "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_CA)
CA_counties <- USAboundaries::us_counties(states = "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_CA) %>% 
  select(-namelsad, -state_name, -state_abbr, -jurisdiction_type) %>% 
  mutate("county_name" = str_to_upper(name)) %>% select(-name)

strata_choices <- map_kids_v_adults %>% 
  pull(strata_name) %>% unique() %>% sort()
race_choices <- map_race_ethnicity %>%
  pull(race_ethnicity) %>% unique() %>% sort()

ui <- fluidPage(
    
  titlePanel("Asthma Hospitalizations by County"),
    selectInput(inputId = "strata", 
                label = "County Hospitalizations by Age Group (Kid vs. Adult)", 
                choices = strata_choices),
        mainPanel(
           plotOutput("map")
           ),
    selectInput(inputId = "race",
                label = "County Hospitalizations by Race/Ethnicity",
                choices = race_choices),
        mainPanel(
            plotOutput("map2"),
            plotOutput("map3")
        )
    )


server <- function(input, output) {

    output$map <- renderPlot({
      
        map_kids_v_adults %>% 
         filter(strata_name == input$strata) %>% 
          group_by(county_name) %>% 
          mutate("strata_hospitalizations" = sum(number_hospitalizations)) %>% 
        tm_shape() + tm_polygons(col = "strata_hospitalizations", n = 4, palette = "viridis") + 
        tm_shape(CA_counties) + tm_borders() + 
        tm_shape(CA_state) + tm_borders(lwd = 2)
    
    })
     output$map2 <- renderPlot({
      
      map_race_ethnicity %>% 
        filter(race_ethnicity == input$race) %>% 
        group_by(county_name) %>% 
        mutate("race_hospitalizations" = sum(number_hospitalizations)) %>% 
        tm_shape() + tm_polygons(col = "race_hospitalizations", n = 4, palette = "viridis", colorNA = NULL) + 
        tm_shape(CA_counties) + tm_borders() + 
        tm_shape(CA_state) + tm_borders(lwd = 2)
      
    })
     output$map3 <- renderPlot({
       map_race_ethnicity %>%
         filter(race_ethnicity == input$race) %>%
         group_by(county_name) %>%
         mutate("race_avg_hospitalization_rate" = mean(age_adjusted_hospitalization_rate)) %>%
         tm_shape() + tm_polygons(col = "race_avg_hospitalization_rate", palette = "viridis") +
         tm_shape(CA_counties) + tm_borders() +
         tm_shape(CA_state) + tm_borders(lwd = 2)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
