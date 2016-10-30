#     This is server base that the every client will connect to.
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

# # # # #
# Setup #
# # # # #

# Colours
zcols <- "RdYlBu" # for colourbrewer scale (see get_colour_ramp in pct-shiny-funs.R)

# packages required
cran_pkgs <- c("shiny", "RColorBrewer", "httr", "rgdal", "rgeos", "leaflet", "DT", "shinyjs", "sp", "dplyr")

repo_sha <- ""# as.character(readLines(file.path(shiny_root, "repo_sha")))

lapply(cran_pkgs, library, character.only = T)

# Functions
source(file.path("../../pct-shiny/pct-shiny-funs.R"), local = T)

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session){
    # To set initialize to_plot
  observe({
   # to_plot$cents <<-   readRDS(file.path(region$data_dir, "c.Rds"))
    to_plot$r_fast <<- readRDS(file.path("../data/rf_LSOA_Cam.Rds" ))
    to_plot$r_quiet <<- readRDS(file.path("../data/rq_LSOA_Cam.Rds"))
  })

  # For all plotting data
  to_plot <- NULL
  # For any other persistent values
  helper <- NULL

  helper$e_lat_lng <- ""

  # Select and sort lines within a bounding box - given by flows_bb()
  sort_lines <- function(lines, group_name, sort_by, nos){
    if(!sort_by %in% names(lines)) return(NULL)
    # If other than route network lines are selected, subset them by the bounding box
    if (group_name != "route_network"){
      poly <- flows_bb()
      if(is.null(poly)) return(NULL)
      poly <- spTransform(poly, CRS(proj4string(lines)))
      keep <- gContains(poly, lines,byid=TRUE )
      if(all(!keep)) return(NULL)
      lines_in_bb <- lines[drop(keep), ]
      # Sort by the absolute values
      lines_in_bb[ tail(order(abs(lines_in_bb[[sort_by]])), nos), ]
    }else{
      # For the route network, just sort them according to the percentage of display
      # Sort by the absolute values
      lines[ tail(order(abs(lines[[sort_by]])), nos), ]
    }

  }

  attrs_zone <- c("Scenario Level of Cycling (SLC)" =    "slc",
                 "Scenario Increase in Cycling (SIC)" = "sic")


  observe({ # For highlighting the clicked line
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    e_lat_lng <- paste0(event$lat,event$lng)

    # Fix bug when a line has been clicked then the click event is
    # re-emmited when the map is moved
    if( e_lat_lng == helper$e_lat_lng)
      return()
    helper$e_lat_lng <<- e_lat_lng

    isolate({
      id_group_name <- unlist(strsplit(event$id, "-"))
      id <- id_group_name[1]
      group_name <- id_group_name[2]

      if (event$group == "centres"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = F,
                    color = get_line_colour("centres") ,
                    opacity = 0.7,
                    layerId = "highlighted")
      } else if (event$group == "zones"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = FALSE,
                    color = "black",
                    opacity = 0.7 ,
                    layerId = "highlighted")
      } else {
        line <- switch(group_name,
                       'straight_line' = to_plot$l[to_plot$l$id == id,],
                       'faster_route' = to_plot$r_fast[to_plot$r_fast$id == id,],
                       'quieter_route' = to_plot$r_quiet[to_plot$r_quiet$id == id,],
                       'route_network' = to_plot$rnet[to_plot$rnet$id == id,]
        )
        if (!is.null(line))
          addPolylines(leafletProxy("map"), data = line, color = "white",
                       opacity = 0.4, layerId = "highlighted")
      }
    })
  })

  # Plot if lines change
  observe({
    # Needed to force lines to be redrawn when scenario, zone or base map changes
    input$scenario
    input$map_base

    leafletProxy("map")  %>% clearGroup(., "straight_line") %>%
      clearGroup(., "quieter_route") %>% clearGroup(., "faster_route") %>% clearGroup(., "route_network") %>%
      removeShape(., "highlighted")

    leafletProxy("map") %>% {
      switch(input$line_type,
             'straight' = plot_lines(., to_plot$l, input$nos_lines, straight_popup, "straight_line", get_line_colour("straight_line")),
             'route'= {
               plot_lines(., to_plot$r_quiet, input$nos_lines, route_popup, "quieter_route", get_line_colour("quieter_route"))
               plot_lines(., to_plot$r_fast, input$nos_lines, route_popup,"faster_route",  get_line_colour("faster_route"))
             },
             'd_route'= plot_lines(., to_plot$r_fast, input$nos_lines, route_popup,"faster_route",  get_line_colour("faster_route")),
             'rnet' = plot_lines(., to_plot$rnet, input$nos_lines, network_route_popup, "route_network", get_line_colour("route_network"))
      )
    }
    if(input$line_type == 'rnet')
      updateSliderInput(session, inputId = "nos_lines", min = 10, max= 50, step = 20, label = "Percent (%) of Network")
    else{

      if (input$line_order == "slc")
        updateSliderInput(session, inputId = "nos_lines", min = 1, max = 200, step = 1,  label = "Top N Lines (most cycled)")
      else
        updateSliderInput(session, inputId = "nos_lines", min = 1, max = 200, step = 1,  label = "Top N Lines")
    }

  })
  get_zone_multiplier <- function(zoom){ zoom^4/8200 }

  # This code displays centroids if zoom level is greater than 11 and lines are displayed
  observe({
    if(is.null(input$map_zoom) ) return()
    input$map_base
    zoom_multiplier <- get_zone_multiplier(input$map_zoom)
    if(input$map_zoom < 11 || input$line_type == 'none')
      hideGroup(leafletProxy("map"), "centres")
    else
      showGroup(leafletProxy("map"), "centres")
  })

  # Set transparency of zones to 0.5 when displayed, otherwise 0
  transp_rate <- reactive({
    if (input$show_zones) 0.5 else 0.0
  })

  # Identify suffix of lines variables
  line_attr <- reactive({
    if(input$scenario == 'olc') 'olc'
    else if (input$line_type != 'rnet') input$line_order
    else 'slc'
  })

  # Identify suffix of zones variables
  zone_attr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  # Identify complete name of lines variable
  line_data <- reactive({
    data_filter(input$scenario, line_attr())
  })

  # Identify complete name of zones variable
  zone_data <- reactive({
    data_filter(input$scenario, zone_attr())
  })

  # Reactive function for the lines data
  # 1) Called when other than 'none' is selected for the Cycling Flows
  # 2) Also called when freeze lines is unchecked and the user navigates the map
  # 3) Or when the user changes the Top Lines slider
  plot_lines_data <- reactive({
    (input$line_type != 'none' && ((!input$freeze && !is.null(input$map_bounds)) || input$nos_lines > 0)) && (line_data() %in% names(to_plot$l@data))
  })

  # Returns the map bounding box
  map_bb <- reactive({
    if (is.null(input$map_bounds)){ return (NULL)}
    if(input$map_bounds$north > 88){
      leafletProxy("map") %>% setView(0.12, 52.205, 14)
      return(NULL)
    }
    lat <- c(input$map_bounds$west , input$map_bounds$east, input$map_bounds$east, input$map_bounds$west )
    lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)
    c1 <- cbind(lat, lng)
    r1 <- rbind(c1, c1[1, ])
    bounds <- SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat"))
    proj4string(bounds)=CRS("+init=epsg:4326 +proj=longlat")
    bounds
  })

  # Updates the bounding box (bb) to the current map bb unless the map is frozen
  # Returns a bb
  flows_bb <- reactive({
    if(!input$freeze || is.null(helper$bb)){
      helper$bb <<- map_bb()
    }
    helper$bb
  })

  # Adds polylines on the map, depending on types and number of lines
  plot_lines <- function(m, lines, nos, popup_fn, group_name, color){
    if (group_name == "route_network") {
      nos <- nos / 100 * nrow(lines)
      min <- 1
      max <- 20
    } else {
      min <- 5
      max <- 12
    }

    line_opacity <- 0.8
    if (group_name == 'quieter_route' || group_name == 'faster_route')
      line_opacity <- 0.5

    sorted_l <- sort_lines(lines, group_name, line_data(), nos)

    to_plot$ldata <<- sorted_l
    if(is.null(sorted_l))
      m
    else{
      addPolylines(m, data = sorted_l, color = color
                   # Plot widths proportional to attribute value
                   # Remove NAs from the weights
                   , weight = normalise( sorted_l[[line_data()]][!is.na(sorted_l[[line_data()]]) ], min = min, max = max)
                   , opacity = line_opacity
                   , group = group_name
                   , popup = popup_fn(sorted_l, input$scenario)
                   , layerId = paste0(sorted_l[['id']], '-', group_name))
    }
  }
  # Updates map tile according to the selected map base
  map_tile_url <- reactive({
    switch(input$map_base,
           'roadmap' = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png",
           'opencyclemap' = "https://c.tile.thunderforest.com/cycle/{z}/{x}/{y}.png",
           'hilliness' = "http://{s}.tiles.wmflabs.org/hillshading/{z}/{x}/{y}.png"
    )
  })

  # Set map attributes
  output$cite_html <- renderUI({
    HTML(paste('Ver', a(repo_sha, href= paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target='_blank'),
               'released under a', a('GNU Affero GPL', href= "../licence.html", target='_blank'),
               'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
    ))
  })


  # Initialize the leaflet map
  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = map_tile_url(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = ifelse(input$map_base == "IMD", 0.3, 1),
                                   maxZoom = ifelse(input$map_base == "IMD", 14, 18), reuseTiles = T)) %>%
      {
        if (input$map_base == 'IMD'){
            addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_urbanmask_dark/{z}/{x}/{y}.png",
              options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
            addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_labels_cdrc/{z}/{x}/{y}.png",
              options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
        }else .
      }
  )

  # Adds map legend
  observe({
    input$map_base
    leafletProxy("map") %>% addLegend("topleft", colors = get_colour_palette(zcols, 10),
                                      labels = c("0-1%",
                                                 "2-3%",
                                                 "4-6%",
                                                 "7-9%",
                                                 "10-14%",
                                                 "15-19%",
                                                 "20-24%",
                                                 "25-29%",
                                                 "30-39%",
                                                 "40%+"),
                                      title = "% Cycling to work",
                                      opacity = 0.5
    )
  })

  # Creates legend as a barplot for IMD map base
  output$imd_legend <- renderPlot({
    my_lab <- c("Most deprived decile", "2nd", "3rd", "4th", "5th",
               "6th", "7th", "8th", "9th", "Least deprived decile",
               "Data missing", "Data not available")

    my_lab <- rev(my_lab)

    my_colors <- c("#a50026","#d73027", "#f46d43","#fdae61","#fee08b",
                  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850",
                  "#006837", "#aaaaaa", "#dddddd")

    my_colors <- rev(my_colors)

    # Set the labelling of Y-axis to bold
    par(font.lab = 2, mar=c(0.0,5.8,0.0,1.0))

    bp <- barplot(rep(1,12), beside = TRUE, col = my_colors,
                  ylab = "IMD From 2015\nIndex of Multiple Deprivation", horiz = T, axes = F)

    text(0, bp, my_lab, cex=0.8, pos=4, font=2, col = "black")
  })

  # Creates data for the lines datatable
  output$lines_datatable <- DT::renderDataTable({
    # Call a function which reactively reads replot variable
    # Only render lines data when any of the Cycling Flows is selected by the user
    if(!plot_lines_data()){
      # Set the warning message that no lines have been selected by the user
      output$warning_message <- renderUI(HTML("<strong>No lines selected: </strong> Lines must be displayed on map </br>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }

    # When route network is selected, show 'no lines available
    if(input$line_type == 'rnet'){
      # Set the warning message that no lines have been selected by the user
      output$warning_message <- renderUI(HTML("<strong>No lines available </strong> </br>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }

    # Empty the warning message - as some lines have been selected by the user
    output$warning_message <- renderUI("")

    # Reuse the lines data stored in the ldata session variable
    lines_to_plot <- to_plot$ldata@data[,unname(line_col_names)]
    decimal_line_cols <- which(vapply(lines_to_plot, function(x) { is.numeric(x) && as.integer(x) != x }, FUN.VALUE = logical(1)))
    DT::datatable(lines_to_plot, options = list(pageLength = 10), colnames = line_col_names, rownames = FALSE,
                  callback = JS("table.ajax.url(history.state + table.ajax.url());")) %>%
      formatRound(columns = decimal_line_cols, digits=2)
  })


  # Hide/show panels on user-demand
  shinyjs::onclick("toggle_panel", shinyjs::toggle(id = "input_panel", anim = FALSE))
  shinyjs::onclick("toggle_legend", shinyjs::toggle(id = "zone_legend", anim = FALSE))
  shinyjs::onclick("toggle_map_legend", shinyjs::toggle(id = "map_legend", anim = FALSE))

  observe({
    if (input$map_base == 'IMD')
      shinyjs::hide("zone_legend")
    else
      shinyjs::show("zone_legend")
  })

  # Function to add a layers control for the routes, so that users can easily select quiet routes
  observe({
    input$line_type
    if (input$line_type == 'route'){
      leafletProxy("map") %>% addLayersControl(
        position = c("bottomright"),
        overlayGroups = c("quieter_route", "faster_route"),
        options = layersControlOptions(collapsed = T)
      )
    }else
      leafletProxy("map") %>% removeLayersControl()
  })
})