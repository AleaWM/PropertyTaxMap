#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
#install.packages("leaflet.extras")
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(readxl)

# link to the API output as a JSON file
cook_shp <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/plss/MapServer/1/query?outFields=*&where=1%3D1&f=geojson"
                    )
muni_shp <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/politicalBoundary/MapServer/2/query?outFields=*&where=1%3D1&f=geojson")

schools_shp <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/cultural/MapServer/9/query?outFields=*&where=1%3D1&f=geojson")

BOR_shp <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/politicalBoundary/MapServer/10/query?outFields=*&where=1%3D1&f=geojson"
                   )
elem_schools_shp <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/clerkTaxDistricts/MapServer/2/query?outFields=*&where=1%3D1&f=geojson")

townships <- read_sf("https://gis.cookcountyil.gov/traditional/rest/services/politicalBoundary/MapServer/3/query?outFields=*&where=1%3D1&f=geojson")

#Tax_bill_deductions <- read_excel("~/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/Tax bill deductions by grouped class_forAugust9.xlsx")



# Define UI for application that draws a map
ui <- fluidPage(
  leafletOutput("map")
  
)

muni_shortnames <- read_excel("~/PhD Fall 2021 - Spring 2022/Merriman RA/ptax/muni_shortnames.xlsx")

## Map exemption takeup rate for homeowners ## 

C2_w_HO_exe <- read_csv("C2_w_HOexe_takeuprate.csv") %>% # made in 5_exemptionsScenarios.rmd
  select(clean_name, `2`) %>% left_join(muni_shortnames) 

TS_comprates <- read_csv("Township_Composite_Taxrates.csv") %>% left_join(muni_shortnames) 


pal <- colorNumeric("viridis", NULL)


# Define the server part of the Shiny app
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      # addPolygons(
      #   data = cook_shp,
      #   #   layerId = ~AGENCY_DESCRIPTION,
      #   group = "Cook Boundary",
      #   options = pathOptions(
      #     fillOpacity = 0.2,
      #     color = "black",
      #     opacity = 1,
      #     fill = C2_w_HO_exe$`2`
      #   )
      # ) %>%
      # addPolygons(
      #   data = BOR_shp,
      #   group = "BOR Boundary",
      #   options = pathOptions(
      #     fillOpacity = 0.2,
      #     color = "black",
      #     opacity = 1
      #   )
      # )  %>%
      # 
      # 
      addPolygons(
        data = townships,
        fillColor = pal(TS_comprates$tax_rate_current),
        label = ~NAME,  # Replace 'name' with the column name containing municipality names
        layerId = ~NAME,
        group = "Township Composite Tax Rates",
        options = pathOptions(
          fillOpacity = 0.6,
          weight = 1,
          color = "black",
          opacity = 1,
        )) %>%
      
      addPolygons(
        data = elem_schools_shp,
        label = ~AGENCY_DESCRIPTION,
        layerId = ~AGENCY_DESCRIPTION,
        group = "Elementary School Districts",
        options = pathOptions(
          fillOpacity = 0.6,
          weight = 1,
          color = "black",
          opacity = 1,
          fill = TRUE
        )
      ) %>%
      
      addPolygons(
        data = muni_shp,
        fillColor = pal(C2_w_HO_exe$`2`),
        
        label = ~MUNICIPALITY,  # Replace 'name' with the column name containing municipality names
        layerId = ~MUNICIPALITY,
        group = "Municipalities",
        options = pathOptions(
          fillOpacity = 1,
          weight = 1,
          color = "black",
          opacity = 1,
          fill = pal(C2_w_HO_exe$`2`)        )
      )  %>%
      # addLegend("bottomright", pal = pal, values = ~TS_comprates$tax_rate_current,
      #           title = "Current Composite Tax Rates",
      #          # labFormat = labelFormat(prsefix = ""),
      #           opacity = 1
      # )
      # 
# 
#       addChoropleth(
#         data = C2_w_HO_exe,
#         valueProperty = `2`,  # Column name containing the values
#         layerID = ~clean_name,  # Column name containing municipality names
#         group = "Class 2 Takeup Rate",  # Assigning a group name
#         legend = legend(position = right,
#                         title = "Class 2 Takeup Rate")
#         ) %>%
      # addLegend(data = C2_w_HO_exe,
      #           position = "bottomright",
      #           values = ~`2`,
      #           #colors = pal(C2_w_HO_exe$`2`)
      #           colors = colorNumeric(palette = "viridis", domain = muni_shp$AGENCY_DESC),
      #           title = "Takeup Rate",
      #           opacity = 1) %>%
      addLayersControl(  # Adding the layers control
        overlayGroups = c( "Municipalities", "Township Composite Tax Rates",   "Elementary School Districts" 
                         #  "BOR Boundary",  "Cook Boundary"
                         ),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
}

# Run the Shiny app
shinyApp(ui, server)

#nicknames <- readxl::read_excel("muni_shortnames.xlsx")

#class_dict <- read_csv("class_dict_expanded.csv")


