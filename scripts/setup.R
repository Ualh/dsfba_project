#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c("here","tidyverse", "tidyr", "readxl","ggrepel",
              "gghighlight", "patchwork", "maps","dplyr","scales",
              "kableExtra", "reactable","sf", "shiny", "leaflet",
              "plotly", "rsconnect", "gifski", "gganimate", 
              "factoextra", "viridis", "reshape2", "heatmaply",
              "gtsummary", "flexdashboard", "zoo", "xts", "dygraphs",
              "car")
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = TRUE
)


