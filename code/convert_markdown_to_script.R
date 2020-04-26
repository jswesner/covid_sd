library(knitr)
library(here)
purl("README.Rmd", output = here::here("code/analysis_script.R"))
     