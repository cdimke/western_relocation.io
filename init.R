#This is the preamble script for the Fire Weather project.  
#It should be run once each time a user begins a new R session to work on the project.

#########################
#Load init functions
source("./init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "data.table",     #for more efficient utilities
            "furrr",          #facilitates simple implementation of parallel processing
            "future",         #support for furrr
            "conflicted",     #resolves function conflict across packages
            "lubridate",      #working with dates
            "units",
            "readr",
            "readxl",
            "progress",
            "lwgeom",
            "haven",
            "stringr",
            "mosaic",
            "purrr",
            "dotwhisker",
            "broom",
            "gridExtra",
            "viridis",
            "gtable",
            "grid",
            "HDCI",
            "gtsummary",
            "gt",
            "readr",
            "DescTools",
            "tigris",
            "leaflet",
            "imputeTS",
            "mapview",
            "sf",
            "pals",
            "stargazer",
    "imputeTS",
    "htmlwidgets"
))
rm(list=ls())

#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("extract", "raster")
conflict_prefer("year", "lubridate")