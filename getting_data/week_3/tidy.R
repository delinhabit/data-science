library(dplyr)
source("utils.R")


loadGDPData <- function(url) {
    dataFile <- downloadData(url, "GDP.csv")
    gdp <- read.csv(dataFile, skip=4, nrows=190)
    gdp <- select(gdp, CountryCode=X, CountryName=X.3, Rank=X.1, GDP=X.4)
    gdp[1:190, ]
}


loadEdData <- function(url) {
    dataFile <- downloadData(url, "EDSTATS_Country.csv")
    read.csv(dataFile)
}


joinData <- function(gdp, edStats) {
    data <- inner_join(gdp, edStats, by = c("CountryCode"))
    data <- data[!is.na(data$Rank), ]
    arrange(data, desc(Rank))
}
