#carga de paquetes

rm(list =ls())
# install.packages("devtools")
# devtools::install_github("JaseZiv/chessR")

for( package in c("shiny", "dplyr", "datasets", "shinycssloaders","shinydashboard","shinyjs", "ggplot2", "DT", "kableExtra","tidyverse", "plotly","chessR","lubridate")) {
  
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = T)
  }
}

##################################################################################################################################################################

# load_data <- function() {
#   Sys.sleep(2)
#   hide("loading_page")
#   show("main_content")
# }