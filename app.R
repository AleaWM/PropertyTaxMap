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

nicknames <- readxl::read_excel("muni_shortnames.xlsx")

#class_dict <- read_csv("class_dict_expanded.csv")

TS_comprates <- read_csv("Township_Composite_Taxrates.csv") %>% 
  arrange(short_name) 

TS_joined <- left_join(townships, TS_comprates, by = c("NAME" = "short_name"))

C2_w_HO_exe <- read_csv("C2_w_HOexe_takeuprate.csv")
Munis_joined <- right_join(muni_shp, C2_w_HO_exe, by = c("MUNICIPALITY" = "clean_name"))


elem_comprates <- read_csv("elem_schooldist_comprates.csv")

# labels <- sprintf(
#   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#   nicknames$clean_name, C2_w_HO_exe$`2`
# ) 

# Define UI for application that draws a map
ui <- fluidPage(
  titlePanel("Cook County Property Tax Summaries"),
    mainPanel(
      leafletOutput("map")          
      )
  )

  


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
      #   group = "Cook County",
      #   layer = "Board of Review",
      #   options = pathOptions(
      #     fillOpacity = 0.2,
      #     color = "black",
      #     opacity = 1
      #   )
      # )  %>%


      addPolygons(
        data = TS_joined,
        fillColor = pal(TS_joined$tax_rate_current),
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
        fillColor = pal(elem_comprates$tax_rate_current),
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
    # style = list("font-weight" = "normal", padding = "3px 8px"),
    # textsize = "15px",
    # direction = "auto"))
        layerId = ~MUNICIPALITY,
        group = "Municipalities",
        options = pathOptions(
          fillOpacity = 1,
          weight = 1,
          color = "black",
          opacity = 1,
          fill = pal(C2_w_HO_exe$`2`)
          )
      )  %>%
#       addLegend(position = "bottomright", pal = pal, values = ~C2_w_HO_exe$`2`,
#                 title = "Current Composite Tax Rates",
#                # labFormat = labelFormat(prsefix = ""),
#                 opacity = 1
#       ) %>%
# 
# # 
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
        overlayGroups = c( "Municipalities", "Township Composite Tax Rates",
                           "Elementary School Districts" 
                         #  "BOR Boundary",  "Cook Boundary"
                         ),
        
        options = layersControlOptions(collapsed = FALSE)
      )
  })

}

# Run the Shiny app
shinyApp(ui, server)



#########################33

labels <- sprintf(
  township_shp$NAME, TS_comprates$tax_rate_current
) %>% lapply(htmltools::HTML)

ui <- fluidPage(
  titlePanel("Cook County Property Tax Summaries"),
  mainPanel(
    leafletOutput("map2")          
  )
)


pal1 = colorBin("magma", domain = TS_joined$tax_rate_current , bins = 7)
pal2 = colorBin("YlOrRd", domain = Munis_joined$`2` , bins = 7)

# Define the server part of the Shiny app
server <- function(input, output, session) {
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(
        data = TS_joined,
        fillColor = ~pal1(TS_joined$tax_rate_current),
        label = ~NAME,  # Replace 'name' with the column name containing municipality names
       #label = labels, 
       layerId = ~NAME,
       group = "Township Composite Tax Rates",
       
       # labelOptions = labelOptions(
       #   style = list("font-weight" = "normal", padding = "3px 8px"),
       #   textsize = "15px",
       #   direction = "auto"),
        options = pathOptions(
          fillOpacity = 0.6,
          weight = 1,
          color = "black",
          opacity = 1
         )
      #   highlightOptions = highlightOptions(
      #     weight = 5,
      #     color = "#666",
      #     dashArray = "",
      #     fillOpacity = 0.7,
      #     bringToFront = TRUE)
      ) %>%
          addLegend(data = TS_joined,
            position = "bottomright", pal = pal1,
            values = ~TS_joined$tax_rate_current,
            title = "Current Composite Tax Rates",
            opacity = 1
          )       %>%
      
      addPolygons(
        data = Munis_joined,
        fillColor = ~pal2(Munis_joined$`2`),
        label = ~MUNICIPALITY,  # Replace 'name' with the column name containing municipality names
        # style = list("font-weight" = "normal", padding = "3px 8px"),
        # textsize = "15px",
        # direction = "auto"))
        layerId = ~MUNICIPALITY,
        group = "Municipalities",
        options = pathOptions(
          fillOpacity = 0.6,
          weight = 1,
          color = "black",
          opacity = 1
        )) %>%
      addLegend(data = Munis_joined,
                position = "bottomleft", pal = pal2,
                values = ~Munis_joined$`2`,
                title = "% Residential",
                opacity = 1
      )       %>%
      addLayersControl(  # Adding the layers control
        overlayGroups = c( "Municipalities", "Township Composite Tax Rates"

        ),
        
        options = layersControlOptions(collapsed = FALSE)
      )
      
  })
  
}

# Run the Shiny app
shinyApp(ui, server)




