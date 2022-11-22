library(flexdashboard)
library(tidyverse)

source("2_grafy.R")
source("3_model.R")

rmarkdown::render(
  input = "dashboard.Rmd",
  flex_dashboard(),
  output_file = "dashboard.html"
)
