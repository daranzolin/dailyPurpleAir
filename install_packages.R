local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/" 
  options(repos = r)
})

install.packages("dplyr")
install.packages("tidyr")
install.packages("jsonlite")
install.packages("lawn")
install.packages("sf")
install.packages("geojsonio")
install.packages("stars")