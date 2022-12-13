#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

epsg_CA <- 26943
epsg_wgs84 <- 4326
CA_state <- USAboundaries::us_states() %>% filter(name == "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_CA)
CA_counties <- USAboundaries::us_counties(states = "California") %>%
  st_set_crs(epsg_wgs84) %>% st_transform(espg_CA) %>% 
  select(-namelsad, -state_name, -state_abbr, -jurisdiction_type) %>% 
  mutate("county_name" = str_to_upper(name)) %>% select(-name)

map_race_ethnicity <- load(here::here("shiny_app/interactive", "map_race_ethnicity.RData"))
race_choices <- map_race_ethnicity %>%
  pull(race_ethnicity) %>% unique() %>% sort()
# pull also does not want to work currently because it says
# "no applicable method for pull applied to object of class character


ui <- fluidPage(
    
  titlePanel("Asthma Hospitalizations by County and Race/Ethnicity"),
    selectInput(inputId = "race", 
                label = "Race/Ethnicity", 
                choices = race_choices),
        mainPanel(
           plotOutput("map")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
    # this filter currently does not want to work ughhh
        map_race_ethnicity %>% 
         filter(race_ethnicity == input$race) %>% 
          group_by(county_name) %>% 
          mutate("race_hospitalizations" = sum(number_hospitalizations)) %>% 
        tm_shape() + tm_polygons(col = "race_hospitalizations") + tm_shape(CA_counties) + tm_borders() + tm_shape(CA_state) + tm_borders(lwd = 2)
    
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
