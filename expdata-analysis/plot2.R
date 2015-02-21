library(dplyr)

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

baltimore_emissions_by_year <- nei_data %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(total = sum(Emissions)) %>%
    mutate(total = total / 1e3)

png("plot2.png")

plot(
    baltimore_emissions_by_year,
    pch = 19,
    xlab = "Year",
    ylab = "Emissions (thousands of tons)",
    main = expression("Evolution of " * PM[2.5] * " emissions (Baltimore, MA)"))

model <- lm(total ~ year, baltimore_emissions_by_year)
abline(model, lwd = 1, col = "darkblue")

# Use invisible() to silence errors when running with Rscript
invisible(dev.off())
