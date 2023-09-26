# SHINY SWIPE MAP: JAVA ISLAND NIGHTTIME LIGHTS
# SHOUTOUT TO MILOS PAPOVIC (https://github.com/milos-agathon)


# INSTALLING LIBRARY

library(tidyverse)
library(sf)
library(terra)
library(shiny)

# SET ROI

jawa_sf <- st_read("data/jawa.shp")

# # GET DATA
## if you get runtime error, just download and place it to your
## working folder manually
# 
# urls <- c(
#   "https://eogdata.mines.edu/nighttime_light/annual/v21/2012/VNL_v21_npp_201204-201212_global_vcmcfg_c202205302300.average_masked.dat.tif.gz",
#   "https://eogdata.mines.edu/nighttime_light/annual/v22/2022/VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif.gz"
# )
# 
# for (url in urls) {
#   download.file(url = url,
#                 destfile = basename(url),
#                 mode = "wb")
# }

# LOAD DATA

raster_files <- list.files(
  path = getwd(),
  pattern = "npp",
  full.names = T
)

globe_lights <- lapply(
  paste0("/vsigzip/", raster_files),
  terra::rast
)

# CROP DATA

jawa_lights_list <- lapply(
  globe_lights,
  function(x){
    terra::crop(
      x,
      terra::vect(jawa_sf),
      snap = "in",
      mask = T
    )
  }
)

# REMOVE ZEROS AND SUBZEROS

jawa_lights_final <- lapply(
  jawa_lights_list,
  function(x){
    terra::ifel(
      x <= 0,
      NA,
      x
    )
  }
)

# RASTER TO DF

jawa_lights_df <- lapply(
  jawa_lights_final,
  function(x){
    as.data.frame(
      x,
      xy = T,
      na.rm = T
    )
  }
)

str(jawa_lights_df)

col_names <- c("x", "y", "value")

jawa_lights_df <- lapply(
  jawa_lights_df,
  setNames,
  col_names
)

# MAP

color <- c("#1f4762", "#FFD966", "white")

pal <- colorRampPalette(
  color, bias = 8
)(512)

years <- c(2012, 2022)

names(jawa_lights_df) <- years

str(jawa_lights_df)

map <- lapply(
  names(jawa_lights_df),
  function(df){
    ggplot(
      data = jawa_lights_df[[df]]
    ) +
      geom_sf(
        data = jawa_sf,
        fill = NA,
        color = color[[1]],
        size = .5
      ) +
      geom_tile(
        aes(
          x = x,
          y = y,
          fill = value
        )
      ) +
      scale_fill_gradientn(
        name = "",
        colors = pal
      ) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(
          size = 60,
          color = "white",
          hjust = .5,
          vjust = 0
        ),
        plot.margin = unit(
          c(
            t = 0, r = 0,
            l = 0, b = 0
          ), "lines"
        )
      ) +
      labs(title = df)
  }
)

for (i in 1:2){
  file_name = paste0(
    "jawa_map_", i, ".png")
  png(
    file_name,
    width = 800,
    height = 800,
    units = "px",
    bg = "#182833"
  )
  
  print(map[[i]])
  dev.off()
}

# MOVE IMAGES TO SHINY FOLDER

.libPaths()
current_dir <- getwd()
shiny_dir <- paste0(.libPaths(), "/shiny/www")
images_list <- list.files(
  path = current_dir,
  pattern = "jawa_map"
)

file.copy(
  from = file.path(
    current_dir,
    images_list
  ),
  to = shiny_dir,
  overwrite = T,
  recursive = F,
  copy.mode = T
)


# SHINY SLIDER
# CSS

css <- HTML("div#comparison {
width: 80vw;
height: 80vw;
max-width: 800px;
max-height: 800px;
overflow: hidden; }
div#comparison figure {
background-image: url('jawa_map_1.png');
background-size: cover;
position: relative;
font-size: 0;
width: 100%;
height: 100%;
margin: 0;
}
div#comparison figure > img {
position: relative;
width: 100%;
}
div#comparison figure div {
background-image: url('jawa_map_2.png');
background-size: cover;
position: absolute;
width: 50%;
box-shadow: 0 5px 10px -2px rgba(0,0,0,0.3);
overflow: hidden;
bottom: 0;
height: 100%;
}

input[type=range]{
  -webkit-appearance:none;
  -moz-appearance:none;
  position: relative;
  top: -2rem; left: -2%;
  background-color: rgba(255,255,255,0.1);
  width: 102%;
}
input[type=range]:focus {
  outline: none;
}
input[type=range]:active {
  outline: none;
}

input[type=range]::-moz-range-track {
  -moz-appearance:none;
  height:15px;
  width: 98%;
  background-color: rgba(255,255,255,0.1);
  position: relative;
  outline: none;
}
input[type=range]::active {
  border: none;
  outline: none;
}
input[type=range]::-webkit-slider-thumb {
  -webkit-appearance:none;
  width: 20px; height: 15px;
  background: #fff;
    border-radius: 0;
}
input[type=range]::-moz-range-thumb {
  -moz-appearance: none;
  width: 20px;
  height: 15px;
  background: #fff;
    border-radius: 0;
}
input[type=range]:focus::-webkit-slider-thumb {
  background: rgba(255,255,255,0.5);
}
input[type=range]:focus::-moz-range-thumb {
  background: rgba(255,255,255,0.5);
}"
)


# JS
js <- HTML('
function moveDivisor() {
	divisor.style.width = slider.value+"%";
}

$(document).on("shiny:connected", function(event){
var divisor = document.getElementById("divisor"),
slider = document.getElementById("slider");
});
'
)

# HTML
ui <- shiny::fluidPage(
  tags$head(tags$style(css)),
  tags$head(tags$script(js)),

  HTML(
    '<div id="comparison">
        <figure>
        <div id="divisor"></div>
    </figure>
    <input type="range" min="0" max="100" value="50" id="slider" oninput="moveDivisor()">
    </div>'
  )
)

server <- function(input, output, session){}

shiny::shinyApp(ui, server)
