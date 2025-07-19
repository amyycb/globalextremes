#options("install.lock"=FALSE) # install.packages("shiny", INSTALL_opts = c('--no-lock'))
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
setwd("C:/Users/Amy/OneDrive - Newcastle University (1)/Documents/IDF/app")

ui <- bootstrapPage(
  theme = shinytheme("journal"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel(
    fluidRow(
      column(10, "Data portal: GSDR-IDF"),
      column(2, img(src = "ncl-logo.png", height=35)),
      column(12,
        paste("ADD INFO ABOUT THE APP AND LINKS HERE.")
      )
    ),
    windowTitle= "Data portal"
  ),
  sidebarLayout(
    sidebarPanel(
      bs_accordion(id = "accordion")  |> 
      bs_set_opts(use_heading_link = T, panel_type = "primary", collapse = FALSE) |> 
      bs_append(title = "Data filters", 
        fluidRow(
          column(12,
              prettySwitch("method", 
              label="Regional Factor Analysis",
              fill=T, 
              inline=T,
              value=T,
              status="success"
            ),
          ),
          column(12,
            sliderTextInput('dur', 'Duration', grid=T, force_edges=F,
              choices=c('1 hour'='1h', '3 hour'='3h', '6 hour'='6h','24 hour'='24h'),
              selected='1h',
            )
          ),
          column(12,
            radioButtons("display", "Data type", 
              choices=c("Parameter"="param", "Return period"="rl"),
              selected="rl",
              inline=F
            )    
          ),
          column(12,
            conditionalPanel(condition="input.display == 'rl'",
              selectInput('rl', "",
                choices=c('10 years'='rp10', '30 years'='rp30', '100 years'='rp100'),
                selected='rp10'
              )
            ),
            conditionalPanel(condition="input.display == 'param'",
              selectInput('param', "",
                choices=c("Location"='location', "Scale"='scale', "Shape"='shape'),
                selected='shape'
              )
            ),
            downloadButton("downloadMap", "Download map")
          )
        )
      ) |>
      bs_append(title = "Display settings", 
        fluidRow(
          column(6,
            sliderInput("size", "Size", 
              min = 0.5, max = 12, step = 0.5, value = 3, ticks = F, round = T
            )
        ),
        column(6,
          sliderInput("opacity", "Fill", 
            min = 0.1, max = 1, step = 0.1, value = 0.3, ticks = F, round = T
            )
          )
        )
      ) |>
      bs_append(title = "Frequency curves", open=T,
        fluidRow(
          column(12, 
            radioButtons('dvsi', 'Curve type',
              choices = c("Depth"='DDF', "Intensity"='IDF'),
              inline = T,
              selected = 'IDF'
            )
          ),
          column(12,
            prettySwitch("logscale", 
              label="Logarithm scale axes",
              fill=T, 
              inline=T,
              value=F,
              status="success"
            )
          ),
          column(12,
            plotlyOutput("idfplot", height="200px", width="100%"),
            downloadButton("downloadPlot", "Download figure")
          )
        )
      ),
      width=5
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "600px"),
      width=7
    )
  ),
  tags$footer(
    HTML("<p><b>Data Portal: Unravelling the Complex Interplay Between Daily and Sub-Daily Rainfall Extremes in Different Climates</b><br> App developer: Amy C. Green<br></p>"),
    HTML("<a href='mailto:amy.green3@newcastle.ac.uk'><nobr>Contact us</nobr></a>"),
    style = "border-top:1px solid #ecf0f1;margin-top: 21px;padding-top: 18px;text-align: left;font-size: 0.7em;"
  ),
  useShinyjs()
)

server <- function(input, output) {
  
  methods <- c("sga", "rfa")
  durs <- c("1h", "3h", "6h", "24h")
  data <- list()

  for (m in methods) {
    data[[m]] = list()
  
    for (dur in durs) {
      tab <- read.csv(paste0("data/", toupper(m), "_", dur, "_gev.csv"))
      data[[m]][[dur]] <- tab
    }
    
  }
  
  filteredData <- reactive({
    req(data)
    return(data[[ifelse(input$method, "rfa", "sga")]][[input$dur]])
  })


  pal <- reactive({
    if (input$display =='param') {
      if (input$param == 'shape') {
        cols <- c("#053061","#2166ac","#4393c3","#92c5de","Gainsboro",
                 "#fddbc7","#f4a582","#d6604d","#b2182b") 
        vals = c(-0.48, -0.25, -0.15,-0.05 ,0, 0.05, 0.15, 0.25, 0.48)
        ramp <- colorRampPalette(cols)(100)
        col <- colorNumeric(ramp, vals, na.color="black")
      } else {
        ramp <- inferno(100, alpha = 1, begin = 0, end = 0.9, direction = -1) 
        col <- colorNumeric(ramp, filteredData()[,input$param], na.color="black")
      }
    } else {
      ramp <- inferno(100, alpha = 1, begin = 0, end = 0.9, direction = -1) 
      col <- colorNumeric(ramp, filteredData()[,input$rl], na.color="black")
    }
    return(col)
  })
 
  popup <- reactive({
    info <- paste0(
      "<B>Station ID: ", filteredData()$X, "</B><BR>", # fix this !
      "<B>Location</B><BR>",
      "Latitude: ", round(as.numeric(filteredData()$lat), 3), "<BR>",
      "Longitude: ", round(as.numeric(filteredData()$lon), 3), "<BR>",
      "<B>Record length </B><BR>",
      filteredData()$years, " (years)<BR>", # add that it is region years for rfa!
      "<B>GEV parameters</B><BR>", 
      "Location: ", round(as.numeric(filteredData()$location), 3), "<BR>",
      "Scale: ", round(as.numeric(filteredData()$scale), 3), "<BR>",
      "Shape: ", round(as.numeric(filteredData()$shape), 3), "<BR>",
      "<B>Return levels </B><BR>", 
      "10 year: ", round(as.numeric(filteredData()$rp10), 3), "<BR>",
      "30 year: ", round(as.numeric(filteredData()$rp30), 3), "<BR>",
      "100 year: ", round(as.numeric(filteredData()$rp100), 3), "<BR>"
    )
  })
  
  clearMap <- function() {
    leafletProxy("map") %>% clearMarkers() %>% removeControl(layerId="leg")%>% 
      clearControls() %>%
      setMaxBounds(-159, -46, 178, 65) # sort so dont get repeats!
                   
  }
  
  observeEvent(input$map_marker_click, {
    req(clicked())
    
    dist_thresh <- 1000
    if (input$method) {
      dists <- sqrt((filteredData()$lat - clicked()$lat) ** 2 + (filteredDAta()$lon - filteredData()$lon) ** 2)
      min_dist <- min(dists)
      min_cl <- df[dists == min_dist,]$cluster
      if (min_dist < dist_thresh) {
        print(paste(min_dist, min_cl))
      } else {
        print(paste("No cluster nearby.", min_dist))
      }
    } else {
      print("Select RFA for nearest cluster identification.")
    }
  })  

  #}
  # click - get nearest point (say what distance is as caveat)
  # if within certain distance display point info
  # if tick a box to say cluster info - display cluster info for nearest point too? And highlight cluster points on map?
  # add pxr.point comparrison button
  # add cluster parameter
  # add downlaod plot button
  makeMap <- function(df, param, pal, title, size, opacity) {
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
            ) %>%
            addSearchOSM(#
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
    }
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.WorldGrayCanvas")
  })
  
  observeEvent(
    list(input$rl, filteredData(), input$param, input$display, input$size, input$opacity, input$dur), {
      clearMap()
      makeMap(
        df=filteredData(),
        param=input[[input$display]],
        pal=pal(),
        title=str_to_title(input[[input$display]]),
        size=input$size,
        opacity=input$opacity
        )
      #get_nearest(clicked(), filteredData())
    }
  )
  
  observeEvent(input$method, {
    updatePrettySwitch(
      #session = session,
      inputId = "method",
      label = ifelse(input$method, "Regional Factor Analysis", "Single Gauge Analysis")
    )
  })
  
  clicked <- reactiveVal()
  
  observeEvent(input$map_marker_click, {
    clicked(input$map_marker_click)
    print(clicked)
  })
  
  grab_data <- function(dur, method, lat, lon) {
  
    result <- tryCatch({
      # Attempt to subset the data
      df <- data[[method]][[dur]]
      fil <- df[which.min(abs(df$lat - as.numeric(lat)) + abs(df$lon - as.numeric(lon))),]
      
      if (dim(fil)[1] > 0) {
        return(as.list(fil[, c("rp10", "rp30", "rp100")]))
      } else {
        return(list(rp10=NA, rp30=NA, rp100=NA))
      }
      
    }, error = function(e) {
      return(list(rp10=NA, rp30=NA, rp100=NA))
    })
    
    return(result)
  }
    
  output$idfplot <- renderPlotly({
    
    req(clicked())
    
    durs <- c(1, 3, 6, 24)
    station_data <- lapply(durs, grab_data, ifelse(input$method, "rfa", "sga"), clicked()$lat, clicked()$lng)
    melted <- melt(station_data)
    colnames(melted) <- c("depth", "rp", "dur")
    
    rps <- c(10, 30, 100)
    #melted$rp <- as.numeric(gsub("\\D", "", melted$variable))
    melted$intensity = melted$depth / melted$dur
    melted <- melted[order(melted$dur), ]
    
    titlefont = list(size = 12)
    tickfont = list(size = 9)
    
    xaxis <- switch(input$logscale,
     T = list(title="Duration (hrs)", type="log", 
                        tickfont=tickfont, titlefont=titlefont),
     F = list(title="Duration (hrs)",tickfont=tickfont, 
                     titlefont=titlefont)
    )
    yaxis <- switch(input$dvsi,
     "IDF" = list(title="Intensity (mmh<sup>-1</sup>)", tickfont=tickfont, 
                  titlefont=titlefont), 
     "DDF" = list(title="Depth (mm)", tickfont=tickfont, titlefont=titlefont)
    )
    
    plot_ly(data=melted) %>% 
      add_trace(
        x=~dur, 
        y=~switch(input$dvsi,
          "IDF"=intensity,
          "DDF"=depth
        ), 
        color=~paste0(rp, " yr"), 
        type="scatter",
        mode = 'lines+markers',
        hoverinfo = 'text', 
        alpha=0.5,
        text=~paste0(
          "Duration: ", dur, 'hrs<br>Intensity: ', round(intensity, 2), 
          'mmh<sup>-1</sup><br>Depth: ', round(depth, 2), 
          'mm<br>Return period: ', rp, 'yrs'),
        name=~paste(rp, 'yrs')
        ) %>% 
      layout(
        #title=list(
        #  text=paste0("<b>Station ID: </b>", station()), 
        #  titlefont=titlefont
        #),
        xaxis = xaxis,
        yaxis = yaxis, 
        legend = list(
          title=list(
            text = "<b>Return<br>period</b>"), 
            titlefont=titlefont
          ),
        margin=list(l = 10, r = 10, b = 10, t = 10, pad = 10)
      ) 
  })
  
  output$idftitle <- renderText({
    return(paste0(input$dvsi, " curves"))
  })
  
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("map", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      mapshot2(output$map(), file=file, selfcontained = TRUE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("idf-curve", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      plotly_IMAGE(output$idfplot, format = ".png", out_file = file)
    }
  )
  

}


shinyApp(ui, server)