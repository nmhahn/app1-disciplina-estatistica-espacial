library(shiny)
library(leaflet)

library(tidyverse)
library(lubridate)
library(RSocrata)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
    arrange(desc(crash_date)) %>%
    transmute(
        injuries = if_else(injuries_total > 0, "injuries", "none"),
        crash_date,
        crash_hour,
        report_type = if_else(report_type == "", "UNKNOWN", report_type),
        num_units,
        posted_speed_limit,
        weather_condition,
        lighting_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude, longitude
    ) %>%
    na.omit()



# ui object
ui <- fluidPage(
    titlePanel(p("Nick's Shiny App - Chicago Traffic Crashes", style = "color:#3474A7")),
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "daterange","Date Range:",
                start = min(crash$crash_date),
                end = max(crash$crash_date),
                min = min(crash$crash_date),
                max = max(crash$crash_date)
            ),
            selectInput(
                inputId = "injure",
                label = "Select variable",
                choices = c("injuries", "none")
            ),
            p("Made with", a("Shiny",
                             href = "http://shiny.rstudio.com"
            ), "."),
            img(
                src = "imageShiny.png",
                width = "70px", height = "70px"
            ),
            p("Source code:", a("GitHub",
                                href = "https://github.com/nmhahn/app1-disciplina-estatistica-espacial"
                                )),
            img(
                src = "GitHub-Mark.png",
                width = "70px", height = "70px"
            ),
        ),
        mainPanel(
            leafletOutput(outputId = "mymap", height=900)
        )
    )
)

# server object
server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        
        crash_filtrado = crash[(crash$crash_date>=input$daterange[1] & crash$crash_date<=input$daterange[2]),]
        crash_filtrado = crash_filtrado[which(crash_filtrado$injuries==input$injure),]
        
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addMarkers(lng = crash_filtrado$longitude,
                       lat = crash_filtrado$latitude,
                       clusterOptions = markerClusterOptions(),
                       popup = paste("crash_date =",crash_filtrado$crash_date, "<br>",
                                     "num_units =",crash_filtrado$num_units, "<br>",
                                     "prim_contributory_cause =",crash_filtrado$prim_contributory_cause
                                     )
                       )
                       
    })
}

shinyApp(ui, server)