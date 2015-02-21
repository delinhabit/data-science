library(dplyr)
library(ggplot2)

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

nei_data <- tbl_df(readRDS(nei_file))
scc_data <- tbl_df(readRDS(scc_file))

### Analyze and plot

coal_scc = scc_data$SCC[grepl("Coal", scc_data$EI.Sector)]
coal_emissions_by_year <- nei_data %>%
    filter(SCC %in% coal_scc) %>%
    group_by(year) %>%
    summarise(total = sum(Emissions)) %>%
    mutate(total = log(total))

png("plot4.png")

ggplot(coal_emissions_by_year, aes(year, total)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_size_area() +
    labs(x = "Year", y = expression("log " * PM[2.5])) +
    ggtitle("Evolution of coal combustion-related emissions (USA)")

# Use invisible() to silence errors when running with Rscript
invisible(dev.off())
