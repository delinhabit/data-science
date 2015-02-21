library(dplyr)
library(ggplot2)

### Download the data file if not already downloaded

if (!file.exists("data")) {
    dir.create("data")
}

nei_file = "data/summarySCC_PM25.rds"

if (!file.exists(nei_file)) {
    url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    data_file = "data/NEI_data.zip"
    download.file(url, destfile = data_file, method = "curl")
    unzip(data_file, exdir = "data/")
}

### Load the data

nei_data <- tbl_df(readRDS(nei_file))

### Analyze and plot

baltimore_emissions_by_year_and_source_type <- nei_data %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarise(total = sum(Emissions)) %>%
    mutate(total = log(total))

png("plot3.png")

ggplot(baltimore_emissions_by_year_and_source_type, aes(year, total)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~type) +
    scale_size_area() +
    labs(x = "Year", y = expression("log " * PM[2.5]))

# Use invisible() to silence errors when running with Rscript
invisible(dev.off())
