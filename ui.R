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

ui <- fluidPage(
  titlePanel(
    fluidRow(
      column(10, "🌧 Data portal: GSDR-IDF"),
      column(2, img(src = "ncl-logo.png", height=35)),
    ),
    windowTitle= "Data portal"
  ),
  tabsetPanel(
    tabPanel("GSDR-IDF",
      sidebarLayout(
        sidebarPanel(
          bs_accordion(id = "accordion")  |> 
          bs_set_opts(use_heading_link = T, panel_type = "primary", collapse = FALSE) |> 
          bs_append(title = "Data filters", collapse = F,
            fluidRow(
              column(12,
                prettySwitch("method", "Regional Frequency Analysis",
                  fill=T, 
                  inline=T,
                  value=T,
                  status="success"
                )
              ),
              conditionalPanel("input.method",
                column(12,
                  radioButtons("filter", "", 
                    choices=c("Regions"="reg", "Results"="res", "Comparison"="val"),
                    selected="res",
                    inline=T
                  )
                )
              ),
              column(12,
                sliderTextInput('dur', 'Duration', grid=T, force_edges=F,
                  choices=c('1 hour'='1h', '3 hour'='3h', '6 hour'='6h','24 hour'='24h'),
                  selected='1h',
                )
              ),
              conditionalPanel("(!input.method) || (input.method && (input.filter == 'res'))",
                column(12,
                  radioButtons("display", "Data type", 
                    choices=c("Parameter"="param", "Return period"="rl"),
                    selected="rl",
                    inline=F
                  )
                )
              ),
              
              conditionalPanel("(input.method &&(input.filter == 'val')) || ((!input.method) && (input.display == 'rl')) || ((input.method) && (input.filter == 'res') && (input.display == 'rl'))", 
                column(12,
                  selectInput('rl', "",
                    choices=c('10 years'='rp10', '30 years'='rp30', '100 years'='rp100'),
                    selected='rp10'
                  )
                )
              ), 
              conditionalPanel(condition="(input.method) && (input.filter == 'res') && (input.display == 'param')",
                column(12, 
                  selectInput('param', "",
                    choices=c("Location"='location', "Scale"='scale', "Shape"='shape', "Index"='index'),
                    selected='shape'
                  )
                )
              ),
              conditionalPanel(condition="(!input.method) && (input.display == 'param')",
                column(12, 
                  selectInput('param1', "",
                    choices=c("Location"='location', "Scale"='scale', "Shape"='shape'),
                    selected='shape'
                  )
                )
              ),
              conditionalPanel("(input.method && (input.filter == 'val'))",
                column(12, 
                  helpText("N.B. Comparisons between datasets and methods were only 
                        done for gauges or gird cells where results from all 3 methods 
                        were available (SGA, RFA and PXR2.point). We estimated PXR2 
                        point rainfall return levels (PXR2.point) by dividing PXR2 
                       return levels by the appropriate ARF.")
                )
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
                textOutput("plotText")
              ),
              column(3,
                downloadButton("downloadData", "Data")
              ),
              column(9,
                prettyCheckbox("show", label="Show selected data table",
                               value=F, status="primary")
              ),
              conditionalPanel("input.show", 
                column(12,
                  tableOutput("plotlyTable")
                )
              ),
              column(12,
                plotlyOutput("idfplot", height="250px", width="100%"),
                tags$script('
                  document.getElementById("download").onclick = function() {
                  var gd = document.getElementById("regPlot");
                  Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                    var a = window.document.createElement("a");
                    a.href = url; 
                    a.type = "image/png";
                    a.download = "plot.png";
                    document.body.appendChild(a);
                    a.click();
                    document.body.removeChild(a);                      
                  });
                  }
                ')
              )
            )
          ),
         
          wellPanel(
            # Styled header
            div(
              #class = "bg-primary text-white p-2 rounded-top",
              strong("Selected Parameters")
            ),
            # Styled content box
            div(
              class = "border border-primary rounded-bottom p-3",
              #style = "font-size: 1.1em;",
              htmlOutput("selectedParams")
            )
          ),
          width=5
        ),
        mainPanel(
          fluidRow(
            column(12,
              leafletOutput("map", width = "100%", height = "600px"),
            #),
            #column(12, 
            #  downloadButton("downloadMap", label="Map")
            )
          ),
          width=7
        )
      )
    ), # end of main tab
    tabPanel("Description",
      tags$head(
        tags$style(HTML("
          .summary-section {
            background-color: #f9f9f9;
            padding: 20px;
            border-radius: 10px;
            margin-top: 20px;
          }
          .summary-title {
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 10px;
          }
          .summary-text {
            font-size: 16px;
            margin-bottom: 20px;
          }
          .video-container {
            display: flex;
            flex-wrap: wrap;
            gap: 20px;
          }
          .video-frame {
            flex: 1 1 45%;
            height: 300px;
          }
        "))
      ),
      
      div(HTML('
<h2>Description</h2>

<h3>🔍 What is the GSDR-IDF Data Portal?</h3>
<p>The <strong>GSDR-IDF Data Portal</strong> (<a href="" target="_blank">Green et al., (submitted)</a>) is an interactive web application that provides global <strong>Intensity-Duration-Frequency (IDF) curves</strong> for extreme rainfall. It is based on the <a href="https://doi.org/10.1175/JCLI-D-18-0143.1" target="_blank">Global Sub-Daily Rainfall (GSDR)</a> dataset, the largest quality-controlled collection of hourly rain gauge data, covering over 24,000 stations worldwide.</p>
<p>The portal allows users to explore rainfall extremes, view and compare return levels, and download data and visualisations for durations of 1, 3, 6, and 24 hours and return periods of 10, 30, and 100 years (<a href="https://doi.org/10.1016/j.wace.2024.100735" target="_blank">Guerreiro et al., 2024</a>).</p>

<h3>⚠️ Why is this important?</h3>
<p>Short-duration extreme rainfall is a major cause of <strong>flash flooding, infrastructure failure, and economic loss</strong>. These events are becoming more frequent and intense due to climate change. However, global resources to assess them have been limited — especially for sub-daily rainfall.</p>
<p>The GSDR-IDF dataset helps address this gap by providing robust, globally consistent estimates of rainfall extremes, supporting flood risk management, climate adaptation, and resilient infrastructure design.</p>

<h3>🧪 How does it work?</h3>
<p>The portal uses <strong>Extreme Value Analysis (EVA)</strong> to estimate rainfall return levels. Two methods are applied:</p>
<ul>
  <li><strong>Single Gauge Analysis (SGA):</strong> Uses sufficiently long time series data (≥30 years) from individual gauges.</li>
  <li><strong>Regional Frequency Analysis (RFA):</strong> Pools data from nearby gauges (≥15 years) using L-moments and clustering.</li>
</ul>
<p>Both methods fit the <strong>Generalized Extreme Value (GEV)</strong> distribution to annual maxima rainfall data. The resulting parameters are used to generate intensity-duration-frequency (IDF) and depth-duration-frequency (DDF) curves.</p>
<p>All data has been quality-controlled using a robust rule-based framework (<a href="https://doi.org/10.1016/J.ENVSOFT.2021.105169" target="_blank">Lewis et al., 2021</a>), with detailed methodology for the EVA described in <a href="https://doi.org/10.1016/j.wace.2024.100735" target="_blank">Guerreiro et al., 2024</a>.</p>

<h3>👤 Who is it for and how do I use it?</h3>
<p>The portal is designed for <strong>engineers, hydrologists, researchers, and planners</strong>. It is easy to use and requires no coding knowledge.</p>
<p><strong>Key features:</strong></p>
<ul>
  <li>🗺️ <strong>Interactive map:</strong> Click on a station to view IDF curves and metadata.</li>
  <li>🔧 <strong>Filters:</strong> Choose method (SGA/RFA), duration, return period, and GEV parameters.</li>
  <li>📥 <strong>Download:</strong> Export plots as PNG or data as CSV.</li>
  <li>📍 <strong>Location-aware:</strong> The IDF curves are derived from point-based rain gauge data, which may not reflect broader regional conditions, especially in areas with sparse data. The portal includes a distance-based warning: if a location is 100–200 km from the nearest gauge, a caution is shown; beyond 200 km, curves are not displayed due to reliability concerns. Users should interpret results cautiously in regions with few gauges or high climate variability.</li>
</ul>
<p>For a full guide, see the User Guide tab and <a href="<placeholder for video link>" target="_blank">video tutorials</a> or visit the <a href="https://github.com/amyycb/globalextremes" target="_blank">GitHub repository</a>.</p>
<p>This application is part of the scientific dataset publication <strong>Global intensity-duration frequency curves based on global sub-daily precipitation (GSDR-IDF)</strong>. If you use this app or its data, please cite it as follows:</p>
<p><strong>Citation:</strong> Green, A.C., Guerreiro, S.B. &amp; Fowler, H.J. <em>Global Intensity-Duration-Frequency curves based on observed sub-daily precipitation (GSDR-IDF)</em>. Submitted to <em>Scientific Data</em> (Nature), School of Engineering and Tyndall Centre for Climate Change Research, Newcastle University, Newcastle upon Tyne, UK.</p>
'))


    ),# end of description tab
    tabPanel("📘 User Guide",
      tags$head(
        tags$style(HTML("
          .summary-section {
            background-color: #f9f9f9;
            padding: 20px;
            border-radius: 10px;
            margin-top: 20px;
          }
          .summary-title {
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 10px;
          }
          .summary-text {
            font-size: 16px;
            margin-bottom: 20px;
          }
          .video-container {
            display: flex;
            flex-wrap: wrap;
            gap: 20px;
          }
          .video-frame {
            flex: 1 1 45%;
            height: 300px;
          }
        "))
      ),
      div(HTML('<div class="user-guide">
  <h2>📘 User Guide</h2>

  <h3>1. Getting Started</h3>
  <p>Open the GSDR-IDF Data Portal in your browser. The homepage features an interactive global map of rain gauge stations. You can zoom, pan, and search by location to begin exploring rainfall extremes.</p>

  <h3>2. Navigating the Map</h3>
  <ul>
    <li>🗺️ <strong>Click on a station</strong> to view rainfall statistics and IDF/DDF curves.</li>
    <li>🔍 <strong>Search by address or postcode</strong> to locate a specific area.</li>
    <li>📍 <strong>Use filters</strong> to refine your view by method, duration, return period, and more.</li>
  </ul>

  <h3>3. Using Filters</h3>
  <p>Customize your analysis using the sidebar filters:</p>
  <ul>
    <li><strong>Method:</strong> Single Gauge Analysis (SGA) or Regional Frequency Analysis (RFA)</li>
    <li><strong>Duration:</strong> 1, 3, 6, or 24 hours</li>
    <li><strong>Return Period:</strong> 10-, 30-, or 100-years</li>
    <li><strong>Data Type:</strong> GEV parameters, return levels, or regional clusters or comparison with PXR2</li>
    <li><strong>Curve Type:</strong> Intensity or depth</li>
    <li><strong>Map Scale:</strong> Linear or logarithmic</li>
  </ul>

  <h3>4. Viewing and Downloading Data</h3>
  <ul>
    <li>📈 After selecting a station, view the corresponding IDF or DDF curve.</li>
    <li>📥 Download:
      <ul>
        <li>Curve plots as PNG images</li>
        <li>Raw data (return levels) as CSV files</li>
      </ul>
    </li>
  </ul>

  <h3>5. Distance-Based Warnings</h3>
  <p>To ensure accuracy:</p>
  <ul>
    <li>⚠️ A warning appears if your location is more than <strong>100 km</strong> from the nearest station.</li>
    <li>🚫 No curve is shown if the distance exceeds <strong>200 km</strong>.</li>
  </ul>

  <h3>6. Learn by Example – Video Tutorials</h3>
  <p>Watch these example videos to understand how to work with rainfall data and create IDF curves:</p>

  <h4>🎥 Example: ADD</h4>
  <iframe width="560" height="315" src="https://www.youtube.com/embed/OxgiUKOfOzE" title="Creating an IDF Curve" frameborder="0" allowfullscreen></iframe>
  <p><a href="https://www.youtube.com/watch?v=OxgiUKOfOzE" target="_blank">Watch on YouTube</a></p>

  <h3>7. Source Code and Documentation</h3>
  <p>Visit the <a href="https://github.com/amyycb/globalextremes" target="_blank">GSDR-IDF GitHub repository</a> for source code, sample data, and deployment instructions.</p>
</div>
'))
    ) # end of user guide tab
  ), # end of tabset 
  tags$footer(
    HTML("<p><b>Data Portal: GSDR-IDF </b><br> App developer: Amy C. Green<br></p>"),
    HTML("<a href='mailto:amy.green3@newcastle.ac.uk'><nobr>Contact us</nobr></a>"),
    style = "border-top:1px solid #ecf0f1;margin-top: 21px;padding-top: 18px;text-align: left;font-size: 0.7em;"
  ),
  useShinyjs()
)