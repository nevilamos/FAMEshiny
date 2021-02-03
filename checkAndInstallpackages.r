packages<-c("Matrix.utils",
            "tidyr",
            "raster",
            "fasterize",
            "sf",
            "foreach",
            "doParallel",
            "gdalUtils",
            "dplyr",
            "tiff",
            "reshape2",
            "plotly",
            "shiny",
            "shinydashboard",
            "tabularaster",
            "knitr",
            "shinyFiles",
            "shinycssloaders",
            "shinyjs",
            "rmarkdown")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)

remove.packages("shinyjs")
