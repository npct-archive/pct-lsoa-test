#     This is UI base that runs on every connected client
#
#     Copyright (C) 2016 Nikolai Berkoff, Ali Abbas and Robin Lovelace
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as
#     published by the Free Software Foundation, either version 3 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(leaflet)
library(shinyjs)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" = "govtarget",
               "Gender equality" = "gendereq",
               "Go Dutch" = "dutch",
               "Ebikes" = "ebike")

line_types <- c("Fast Routes" = "d_route",
                "Fast & Quiet Routes" = "route")

attrs_zone <- c("Number of cyclists"    = "slc",
               "Increase in Cycling" = "sic",
               "HEAT Value"          = "slvalue_heat",
               "CO2 reduction"       = "sico2")

map_base_attrs <- c("Roadmap (Black & White)"   = "roadmap",
                    "Roadmap (OpenCycleMap)" = "opencyclemap",
                    "Satellite" = "satellite",
                    "Index of Deprivation" = "IMD",
                    "Hilliness" = "hilliness")

shinyUI(
  navbarPage(
    title = "Propensity to Cycle Tool",
    id="nav",
    tabPanel(
      "Map",
      useShinyjs(),
      div(
        class="outer",
        tags$head(
          includeScript("../../pct-shiny/www/assets/extra.js"),
          includeCSS("../../pct-shiny/www/stylesheet.css"),
          includeHTML(file.path("../../pct-shiny/regions_www", "favicon.html"))
        ),
        br(),
        leafletOutput("map", width="100%", height="95%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          fixed = TRUE,  top = 110,  right = 20, width = 180,
          height = "auto",  style = "opacity: 0.9",
          tags$div(title="Show/Hide Panel",
                   a(id = "toggle_panel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(
            id = "input_panel",
            tags$div(title="Scenario (see manual)",
                     selectInput("scenario", "Scenario:", scenarios, selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     selectInput("line_type", "Cycling Flows", line_types, selected = "none", selectize = F)
            ),
            tags$div(title="Change base of the map",
                     selectInput("map_base", "Map Base:", map_base_attrs, selectize = F)
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Untick to update lines when you move the map",
                       checkboxInput("freeze", "Freeze Lines", value = F)
              ),
              tags$div(title="Number of lines to show",
                       sliderInput("nos_lines", label = "Top N Lines (most cycled)", 1, 200, value = 30, ticks = F)
              ),
              conditionalPanel(
                condition = "input.line_type != 'rnet' && input.scenario != 'olc'",
                tags$div(title="Order the top flows by",
                         selectInput("line_order", "Order lines/flows by", attrs_zone, selected = "slc", selectize = F)
                )
              )
            )
          )
        )
      )
    )
  )
)
