# Social and Economic Consequences of Severe Weather Events in U.S.
Ion Scerbatiuc  
May 22, 2015  

## Synopsis

TDB

## Data Processing

For this analysis we use the following libraries as dependencies and hence they have to be loaded into the environment.


```r
library(dplyr)
library(ggplot2)
```

### Downloading the data

The compressed data file is located on the Reproducible Reasearch course storage location, for convenience, so we need to first download it from there.


```r
if (!file.exists("StormData.csv.bz2")) {
    download.file(
        "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
        destfile = "StormData.csv.bz2", 
        method = "curl")
}
```

### Loading and cleaning the data

Let's first load the raw data from the downloaded archive file.


```r
storm_data_raw <- read.csv(bzfile("StormData.csv.bz2"), stringsAsFactors = FALSE)
```

Next let's clean the data and select only the relevant columns and rows for this analysis. Since the purpose of the analysis is to identify the social and economic impact of the weather events, we need only the columns that have values in the corresponding columns: `FATALITIES`, `INJURIES`, `PROPDMG` and `CROPDMG`.


```r
storm_data <- storm_data_raw %>%
    filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0) %>%
    transmute(
        event.type=EVTYPE, fatalities=FATALITIES, injuries=INJURIES,
        damage.p=PROPDMG, damage.p.multiplier=toupper(PROPDMGEXP),
        damage.c=CROPDMG, damage.c.multiplier=toupper(CROPDMGEXP))
```

#### Cleaning the damage amounts

The damage amounts are specified as raw values and multipliers. The expected multipliers are: empty, H, K, M and B. Any other values are unknown or invalid. Let's see how many of those we have:


```r
valid.multipliers <- c("", "H", "K", "M", "B")

table(with(storm_data, 
           damage.p.multiplier %in% valid.multipliers & 
               damage.c.multiplier %in% valid.multipliers))
```

```
## 
##  FALSE   TRUE 
##    269 254364
```

We can see that the number of observations with unknown multipliers is very low comparative to the total number of events. Let's assume that these values do no change the overall statistics and hence we can remove the observations from our analysis. 


```r
storm_data <- storm_data %>% filter(
    damage.p.multiplier %in% valid.multipliers,
    damage.c.multiplier %in% valid.multipliers)
```

Now, let's compute the absolute dolar amounts for the remaining observations:


```r
as.multiplier <- function(char) {
    multipliers <- c(1e2, 1e3, 1e6, 1e9)
    names(multipliers) <- c("H", "K", "M", "B")
    ifelse (char %in% names(multipliers), multipliers[char], 1)
}

storm_data <- storm_data %>%
    mutate(
        damage.p = damage.p * as.multiplier(damage.p.multiplier),
        damage.c = damage.c * as.multiplier(damage.c.multiplier)) %>%
    select(-damage.p.multiplier, -damage.c.multiplier)
```

#### Cleaning event types

The `EVTYPE` column is very messy, so let's try and clean it up a little bit.

First, there are a lot of variation of `THUNDERSTORM WINDS` like `TSTM WIND`, `TSTM WIND (G45)` and so on. Let's unify all those records under a single value.


```r
pattern = "T[^S]*S[^T]*T[^M]*M WIND(S?)"
storm_data[grep(pattern, storm_data$event.type), ]$event.type <- "THUNDERSTORM WIND"
```

A few other categories of event types have also duplicates. Let's normalize them to a single value.


```r
storm_data[grep("HEAT", storm_data$event.type), ]$event.type <- "HEAT"
storm_data[grep("HURRICANE", storm_data$event.type), ]$event.type <- "HURRICANE"
storm_data[grep("FLASH FLOOD", storm_data$event.type), ]$event.type <- "FLASH FLOOD"
```

#### Tidying up the data

In order to interpret the results of our analysis, we need two tidy datasets: number of casualties by event type and the damage amounts by event type.


```r
casualties <- storm_data %>%
    group_by(event.type) %>%
    summarise(
        fatalities = sum(fatalities),
        injuries = sum(injuries),
        totals = sum(fatalities + injuries)) %>%
    filter(totals > 0) %>%
    arrange(-totals)

damage <- storm_data %>%
    group_by(event.type) %>%
    summarise(
        property = sum(damage.p),
        crops = sum(damage.c),
        totals = sum(damage.p + damage.c)) %>%
    filter(totals > 0) %>%
    arrange(-totals)
```

## Results

TBD