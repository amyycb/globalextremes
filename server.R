library(shiny)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(bsplus)
library(shinyWidgets)
library(shinyjs)
library(viridis)
library(stringr)
library(leafpop)
library(reshape2)
library(shinythemes)
library(plotly)
library(leaflet.extras)
library(mapview)

#setwd("C:/Users/Amy/OneDrive - Newcastle University (1)/Documents/IDF/app")
library(webshot)
library(evd)
library(devtools)
install_github("wch/webshot")
library(htmlwidgets)
library(webshot)
#webshot::install_phantomjs(force=T)
library(geosphere)

if (!require("processx")) install.packages("processx")
if (!require("reticulate")) install.packages("reticulate")

server <- function(input, output) {
  
  methods <- c("sga", "rfa")
  durs <- c("1h", "3h", "6h", "24h")
  data <- list()
  
    
  clicked <- reactiveVal(NULL)
  nearest <- reactiveVal(NULL)
  nearest100 <- reactiveVal(NULL)
  
  param <- reactive({
    if (input$method & (input$filter == "reg")) {
        p = "cluster"
      }
      else if (input$method & (input$filter == "val")) {
        p = paste0(input$rl, "_vs_PXR2")
      } else {
        if ((!input$method) & (input$display == "param")) {
          p = input$param1
        } else {
          p = input[[input$display]]
        }
      }
    return(p)
  })
  
  for (m in methods) {
    data[[m]] = list()
  
    for (dur in durs) {
      #if (m == "rfa") {
      tab <- read.csv(paste0("data/", toupper(m), "_", dur, "_all.csv"))
      
      #} else {
      #  tab <- read.csv(paste0("data/", toupper(m), "_", dur, ".csv"))
      #}
      data[[m]][[dur]] <- tab
    }
  }

  filteredData <- reactive({
    req(data)
    return(data[[ifelse(input$method, "rfa", "sga")]][[input$dur]])
  })
  
  
  output$selectedParams <- renderUI({
    tags$ul(
      tags$li(strong("Method: "), ifelse(input$method, "RFA", "SGA")),
      tags$li(strong("Duration: "), input$dur),
      tags$li(strong("Parameter: "), param()),
      #tags$li(strong("Curve Type: "), input$dvsi),
      #tags$li(strong("Log Scale: "), ifelse(input$logscale, "Yes", "No"))
    )
  })


  
  pal <- reactive({
    if (param() == "cluster") {
      # regions 
      # scale_fill_gradientn(colors = ocean.balance(26), na.value = "black")
      cols <- brewer.pal(8, "Dark2")
      ramp <- rep(cols, length.out = max(filteredData()$cluster))
      col <- colorFactor(ramp, filteredData()$cluster)
    } else if ((input$filter == "val") & (input$method)) {
      # validation 
      ramp <- inferno(100, alpha = 1, begin = 0, end = 0.9, direction = -1) 
      col <- colorNumeric(ramp, filteredData()[,paste0(input$rl, "_vs_PXR2")], na.color="grey")
    } else {
      if ((input$display =='param') & (param() == 'shape')) {
        # shape parameter
        cols <- c("#053061","#2166ac","#4393c3","#92c5de","Gainsboro",
                   "#fddbc7","#f4a582","#d6604d","#b2182b") 
        
        vals = c(-0.48, -0.25, -0.15,-0.05 ,0, 0.05, 0.15, 0.25, 0.48)
        ramp <- colorRampPalette(cols)(100)
        col <- colorNumeric(ramp, vals, na.color="grey")
      } else if (input$display =='param') {
        # location and scale parameters
        ramp <- inferno(100, alpha = 1, begin = 0, end = 0.9, direction = -1) 
        col <- colorNumeric(ramp, filteredData()[,param()], na.color="grey")
      } else {
        # return levels
        ramp <- inferno(100, alpha = 1, begin = 0, end = 0.9, direction = -1) 
        col <- colorNumeric(ramp, filteredData()[,input$rl], na.color="grey")
      }
    }
  })
      
  popup <- reactive({
    
    index <- ifelse(input$method, paste0("Index: ", round(as.numeric(filteredData()$index), 3), "<BR>"), "")
    info <- paste0(
      "<B>Station ID: ", filteredData()$station_id, "</B><BR>", 
      "<B>Location</B><BR>",
      "Latitude: ", round(as.numeric(filteredData()$lat), 3), "<BR>",
      "Longitude: ", round(as.numeric(filteredData()$lon), 3), "<BR>",
      "<B>Record length ",
      switch(input$method, " (region) ", ""),
      "</B><BR>",
      filteredData()$years, " (years)<BR>", # add that it is region years for rfa!
      "<B>GEV parameters</B><BR>", 
      index,
      "Location: ", round(as.numeric(filteredData()$location), 3), "<BR>",
      "Scale: ", round(as.numeric(filteredData()$scale), 3), "<BR>",
      "Shape: ", round(as.numeric(filteredData()$shape), 3), "<BR>",
      "<B>Return levels </B><BR>", 
      "10 year: ", round(as.numeric(filteredData()$rp10), 3), "<BR>",
      "30 year: ", round(as.numeric(filteredData()$rp30), 3), "<BR>",
      "100 year: ", round(as.numeric(filteredData()$rp100), 3), "<BR>"
    )
    return(info)
  })
  
  clicked <- reactiveVal()
  
  clearMap <- function() {
  
    leafletProxy("map") %>% clearMarkers() %>% removeControl(layerId="leg")%>% 
      clearControls() %>%
      setMaxBounds(-159, -46, 178, 65) # sort so dont get repeats!
                   
  }
  
  makeMap <- function(df, param, pal, title, size, opacity) {
    if (param %in% colnames(df)) {
      if (!(((input$filter == "reg") & (input$method)))) {
        leafletProxy("map", data = df) %>%
            addCircleMarkers(
                  lng=df$lon, lat=df$lat,
                  radius=size, stroke=T, weight=1, fill=T, 
                  color=pal(df[, param]), opacity=0.9, 
                  fillColor=pal(df[, param]), fillOpacity = opacity,
                  label=round(as.numeric(df[, param]) , 3),
                  popup=~popup(),
                  layerId=ifelse(input$method, df$cluster, df$X)
            ) %>%
               addLegend(layerId="leg",
                  position="bottomleft", 
                  title=title,
                  pal=pal, 
                  values=~data[[ifelse(input$method, "rfa", "sga")]][[input$dur]][, param] # sort this
              )  %>%
              addSearchOSM(
                options = searchOptions(zoom=8, collapsed = FALSE, position = "topright")
              ) %>%
              addReverseSearchOSM(
                showSearchLocation = F,
                showBounds = F,
                showFeature = F,
                fitBounds = TRUE,
                displayText = T,
                group = NULL
              )
        } else {
          leafletProxy("map", data = df) %>%
            addCircleMarkers(
                  lng=df$lon, lat=df$lat,
                  radius=size, stroke=T, weight=1, fill=T, 
                  color=pal(df[, param()]), opacity=0.9, 
                  fillColor=pal(df[, param]), fillOpacity = opacity,
                  label=round(as.numeric(df[, param]) , 3),
                  popup=~popup(),
                  layerId=ifelse(input$method, df$cluster, df$X) 
            ) %>%
            addSearchOSM(
              options = searchOptions(zoom=8, collapsed = FALSE, position = "bottomright")
            ) %>%
            addReverseSearchOSM(
              showSearchLocation = F,
              showBounds = F,
              showFeature = F,
              fitBounds = TRUE,
              displayText = F,
              group = "test")
        }
    } else {
      print(param)
      print(df)
    }
  }
  
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldGrayCanvas")
  })
  
  observeEvent(
    list(input$method, param(), input$rl, filteredData(), input$param, input$param1, input$display, input$size, input$opacity, input$dur, input$filter), {
      
      if (input$method & (input$filter == "reg")) {
        param = "cluster"
      }
      else if (input$method & (input$filter == "val")) {
        param = paste0(input$rl, "_vs_PXR2")
      } else {
        if ((!input$method) & (input$display == "param")) {
          param = input$param1
        } else {
          param = input[[input$display]]
        }
      }
      
      clearMap()
      if (param %in% colnames(filteredData())) {
      makeMap(
        df=filteredData(),
        param=param,
        pal=pal(),
        title=str_to_title(param), #input[[input$display]]),
        size=input$size,
        opacity=input$opacity
        )
      }
    }
  )
  
  observeEvent(input$geosearch_showlocation, {print(input$osm_search_coords)})

  output$downloadMap <- downloadHandler(
    ## save html to png
    filename = function() { paste('map', 'png', sep = '.') },
    content = function(file) {
      return(mapshot2(leafletProxy("map"), file =file,
        cliprect = "viewport"))
    },
    contentType="image/png" 
  )
  
  get_latlon <- function(click, type) {
    return(list(type=type, lat=click$lat, lon=click$lng))
  }
  
  # Get lat/lon of clicked point
  observeEvent(input$map_click, {
    if (!is.null(input$map_click$lat)) {
      clicked(get_latlon(input$map_click, "map"))
    }
  })
  
  # Get lat/lon of clicked marker
  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click$lat)) {
      clicked(get_latlon(input$map_marker_click, "marker"))
    }
  })
  
  # Get nearest info from lat/lon of clicked marker/map location
  observeEvent(list(input$map_click, input$map_marker_click), {

    if (!is.null(clicked())) {
      
      
      # Calculate distances
      dists <- distHaversine(c(clicked()$lon, clicked()$lat), 
                             data.matrix(filteredData()[,c("lon", "lat")])) / 1000
      
      min_dist <- min(dists)
      
      if (is.finite(min_dist)) {
        near = filteredData()[dists == min_dist,]
        
        if (input$method) {
          cluster = near$cluster
        } else {
          cluster = NULL
        }
        
        nearest(list(type=clicked()$type, lat=clicked()$lat, lon=clicked()$lng, 
                id=near$station_id, df=near, cluster=cluster, distance=min_dist))
        
        near100 = filteredData()[(dists < 100) & (dists > min_dist),]
        
        nearest100(list(type=clicked()$type, lat=clicked()$lat, lon=clicked()$lng, 
                id=near100$station_id, df=near100, cluster=cluster, distance=100))
        
      } else {
        nearest(NULL)
        nearest100(NULL)
      }
 
    } 
  })
  
  # input$map_center
  # Observe reverse search events and store lat/lon
  observeEvent(input$map_reverse_search, {
    if (!is.null(input$map_reverse_search$lat) && !is.null(input$map_reverse_search$lng)) {
      reverseSearchResult(list(lat = input$map_reverse_search$lat, lon = input$map_reverse_search$lng))
    }
  })
  
  # Get lat/lon of clicked point
  observeEvent(input$map_reverse_search, {
   
    if (!is.null(input$reverseSearchResult()$lat)) {
      clicked(get_latlon(input$reverseSearchResult(), "rev"))

    }
  })
  
observeEvent(input$map_click, {
  click <- input$map_click
  lat <- click$lat
  lon <- click$lng
  })


  plotlyData <- reactive({
    
    if ((!is.null(nearest())) & (nearest()$distance < 200)) {
      rps <- c(10, 30, 100)
      durs <- c(1, 3, 6, 24)
      get_dur <- function(dur) {
        df <- data[[ifelse(input$method, "rfa", "sga")]][[paste0(dur, "h")]]
        out <- df[df$station_id == nearest()$id, ]
        if (nrow(out) > 0) {
          out$dur <- as.integer(gsub("\\D", "", dur))
            return(out[, c("dur", "rp10", "rp30", "rp100")])
        }
      }
      
      station_data <- do.call(rbind, lapply(durs, get_dur))
      if (!is.null(station_data) && nrow(station_data) > 0) {
        melted <- reshape2::melt(station_data, id.vars = "dur", value.name = "depth")
        melted$rp <- gsub("\\D", "", melted$variable)
        melted$intensity <- melted$depth / melted$dur
        melted <- melted[order(melted$rp), ]
        melted <- melted[order(melted$dur), ]
        if (input$dvsi == "IDF") {
          melted$y = melted$intensity
        } else {
          melted$y = melted$depth
        }
      return(melted)
      } #else {
        #out <- "No curve data to plot."
      #} 
    } #else {
      #out <- "No curve data to plot."
    #}
    
  })
  
  plotlyData100 <- reactive({
    if (!is.null(nearest100())) {
      rps <- c(10, 30, 100)
      durs <- c(1, 3, 6, 24)
  
      get_dur <- function(dur, station_id) {
        df <- data[[ifelse(input$method, "rfa", "sga")]][[paste0(dur, "h")]]
        out <- df[df$station_id == station_id, ]
        if (nrow(out) > 0) {
          out$dur <- as.integer(gsub("\\D", "", dur))
          return(out[, c("dur", "rp10", "rp30", "rp100", "station_id")])
        }
      }
  
      all_station_data <- do.call(rbind, lapply(nearest100()$id, function(station_id) {
        do.call(rbind, lapply(durs, function(dur) get_dur(dur, station_id)))
      }))
  
      if (!is.null(all_station_data) && nrow(all_station_data) > 0) {
        melted <- reshape2::melt(all_station_data, id.vars = c("dur", "station_id"), value.name = "depth")
        melted$rp <- as.integer(gsub("\\D", "", melted$variable))
        melted$intensity <- melted$depth / melted$dur
        melted <- melted[order(melted$rp, melted$dur), ]
        melted$y <- if (input$dvsi == "IDF") melted$intensity else melted$depth
        return(melted)
      }
    }
  })

  
  get_station_data <- function(id, method, curve) {
    rps <- c(10, 30, 100)
    durs <- c(1, 3, 6, 24)
    get_dur <- function(dur) {
      df <- data[[ifelse(method, "rfa", "sga")]][[paste0(dur, "h")]]
      out <- df[df$station_id == id, ]
      if (nrow(out) > 0) {
        out$dur <- as.integer(gsub("\\D", "", dur))
          return(out[, c("dur", "rp10", "rp30", "rp100", "cluster")])
      }
    }
    
    station_data <- do.call(rbind, lapply(durs, get_dur))
    if (!is.null(station_data) && nrow(station_data) > 0) {
          melted <- reshape2::melt(station_data, id.vars = c("dur", "cluster"), value.name = "depth")
          melted$rp <- gsub("\\D", "", melted$variable)
          melted$intensity <- melted$depth / melted$dur
          melted <- melted[order(melted$rp), ]
          melted <- melted[order(melted$dur), ]
          if (curve == "IDF") {
            melted$y = melted$intensity
          } else {
            melted$y = melted$depth
          }
      return(melted)
        
    }
    
  }
  
  
  get_rls <- function(id, curve) {
    
    durs <- c(1, 3, 6, 24)
    for (dur in durs) {
      
    }
    rps <- c(10, 30, 100)
    get_rl <- function(dur) {
      df <- data[[ifelse(input$method, "rfa", "sga")]][[paste0(dur, "h")]]
      
      out <- df[df$cluster == cluster, ]
      min_rps <- min(out[, c("rp10", "rp30", "rp100")])
      max_rps <- min(out[, c("rp10", "rp30", "rp100")])
      
      cl_rps = gev_return_level(scale=out$scale * out$index, shape=out$shape, location=out$location * out$index, periods=rps)
      if (nrow(out) > 0) {
        
        return(data.frame("dur"=dur, "rp10"=cl_rps[1], "rp30"=cl_rps[2], "rp100"=cl_rps[3]))
      }
    }
    
    station_data <- data.frame(do.call(rbind, lapply(durs, get_rl)))
    if (!is.null(station_data) && nrow(station_data) > 0) {
          melted <- reshape2::melt(station_data, id.vars = "dur", value.name = "depth")
          melted$rp <- gsub("\\D", "", melted$variable)
          melted$intensity <- melted$depth / melted$dur
          melted <- melted[order(melted$rp), ]
          melted <- melted[order(melted$dur), ]
          if (curve == "IDF") {
            melted$y = melted$intensity
          } else {
            melted$y = melted$depth
          }
      return(melted)
        
    }
  }
  
  output$plotlyTable <- renderTable({
    if ((!is.null(nearest())) & (!is.null(plotlyData()))) {

      dcast(plotlyData(), dur ~ variable, value.var = ifelse(input$dvsi == "DDF", "depth", "intensity"))
    }
  })
 
  
  clusterData <- reactive({
    if (!is.null(nearest())) {
      if (input$method) {
          return(get_cluster_rls(nearest()$cluster, input$dvsi))
        } else {
          return(NULL)
        }
    }
  })
  
  output$plotText <- reactive({
    thresh = 100
    thresh_max = 200
    
    if (is.null(nearest())) {
      txt1 = "Select a location or station on the map."
    } else {
      
      d = round(nearest()$distance)
  
      if (d <= 0.001) {
        txt1 = paste0("You have selected the station ", nearest()$id, ".")
      } else if ((d > 0.001) & (d < thresh)) {
        txt1 = paste0("The nearest station is ", d, "km away from the 
                     location selected on the map. This is station ", 
                      nearest()$id, ".")
      } else if ((d > thresh) & (d <= thresh_max)) {
        txt1 = paste0("WARNING! The nearest station is ", d, "km away from the 
                     location selected on the map.")
      } else {
        txt1 = paste0("The nearest station is ", d, "km away from the", 
                     "location selected on the map. This is further than 200km.",
                     "To display a curve please select a location nearer a 
                      station.")
      }
      
      if (d <= thresh_max) {
        location <- signif(nearest()$df$location, 3)
        scale <- signif(nearest()$df$scale, 3)
        shape <- signif(nearest()$df$shape, 3)
        
        if (is.null(nearest()$df$index)) {
          txt2 = paste0("location: ", location, ", scale: ", scale, 
                       ", shape: ", shape)
        } else {
          index <- signif(nearest()$df$index, 3)
          txt2 = paste0("location: ", location, ", scale: ", scale, 
                       ", shape: ", shape, ", index: ", index)
        }
      } else {
        txt2 = paste0("To display a curve please select a location nearer a 
                      station.")
      }
    return(txt1)
    }
  })
  
  # Get idf info
  output$idfplot <- renderPlotly({
    if (!is.null(nearest())) {
      if ((is.data.frame(plotlyData())) & ((!is.null(plotlyData())))) {
        
        trend_data <- plotlyData() %>%
          group_by(variable) %>%
          do({
            model <- lm(log(y) ~ log(dur), data = .)
            a <- exp(coef(model)[1])
            b <- coef(model)[2]
            dur_seq <- seq(min(.$dur), max(.$dur), length.out = 100)
            data.frame(dur = dur_seq, y = a * dur_seq^b, variable = unique(.$variable))
          })
        
        g <- plot_ly(plotlyData(), x=~dur, y=~y, color=~variable, type='scatter') %>%
            #mode='lines+markers', line=list(width=3)) %>%
            add_trace(data = trend_data, x = ~dur, y = ~y, color = ~variable,
              type = 'scatter', mode = 'lines', line = list(dash = 'dot', width = 2),
              showlegend = FALSE) %>%
            layout(title=list(text=paste0(ifelse(input$dvsi=="IDF", "Intensity", "Depth"), 
                  "-Duration-Frequency Curve: ", nearest()$id), size=14, y = 1.05, x = 0.5, xanchor = 'center', yanchor =  'top'),
              xaxis = list(title = "Duration (h)", type = ifelse(input$logscale, "log", "linear")),
              yaxis = list(title = ifelse(input$dvsi == "IDF", "Intensity (mm/h)", "Depth (mm)"), 
                           type = ifelse(input$logscale, "log", "linear")),
              legend=list(title=list(text="Return period\n(years)")))
        
        #if  ((is.data.frame(plotlyData100())) & ((!is.null(plotlyData100())))) {
        #  
        #  for (id100 in unique(plotlyData100()$station_id)) {
        #    df100 <- plotlyData100()[plotlyData100()$station_id == id100, ]
#  
#            g <- g %>% add_trace(data=df100, x=~dur, y=~y, color=~variable, 
 #                       type='scatter', mode='lines+markers', line = list( 
  #                      opacity = 0.3, width = 0.5, dash="dot"), marker = list(symbol = 'square', 
   #                                                              opacity=0.2), showlegend=F)
              
    #      }
        #}
        return(g)
      }
    }
  })
  
  # Function to calculate return levels
  gev_return_level <- function(scale, shape, location, periods) {
    return_levels <- location + (scale / shape) * ((-log(1 - 1 / periods))^(-shape) - 1)
    return(return_levels)
  }
  
  observe({
    if ((!is.null(clicked())) && ((!is.null(plotlyData())))) {
      output$downloadData <- downloadHandler(
          filename = function() {
        # Downloadable csv of selected dataset ----
          paste("idf_", nearest()$id, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(plotlyData()[, c("dur", "depth", "rp", "intensity")], file, row.names = FALSE)
        }
      )
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("idf_", nearest()$id, ".html", sep = "")
        },
        content = function(file) {
          kaleido(idfplot, file=file)
        }
      )
    }
  })
}
    
    
    




