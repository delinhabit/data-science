library(dplyr)

### Download the data file if not already downloaded

if (!file.exists("data")) {
    dir.create("data")
}

nei_file = "data/summarySCC_PM25.rds"
scc_file = "data/Source_Classification_Code.rds"

if (!all(file.exists(nei_file, scc_file))) {
    url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    data_file = "data/NEI_data.zip"
    download.file(url, destfile = data_file, method = "curl")
    unzip(data_file, exdir = "data/")
}

### Load the data

nei_data <- readRDS(nei_file)
scc_data <- readRDS(scc_file)

### Analyze and plot


