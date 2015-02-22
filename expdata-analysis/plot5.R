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

# For the purpose of this analysis, I'm considering a "motor vehicle" any
# vehicle operated by a motor that is considered a source of pollution.
# In the SCC dataset, that means all the entries that have EI.Sector start
# with "Mobile - "

scc_mv <- scc_data$SCC[grepl("Mobile - ", scc_data$EI.Sector)]
mv_emissions_by_year <- nei_data %>%
    filter(SCC %in% scc_mv, fips == "24510") %>%
    group_by(year) %>%
    summarise(total = sum(Emissions)) %>%
    mutate(total = log(total))

png("plot5.png")

ggplot(mv_emissions_by_year, aes(year, total)) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_size_area() +
    labs(x = "Year", y = expression("log " * PM[2.5])) +
    ggtitle("Evolution of motor vehicle emissions (Baltimore, MA)")

# Use invisible() to silence errors when running with Rscript
invisible(dev.off())
