compute_paths <- function(directory, id = 1:332) {
    filenames <- sprintf("%03d.csv", id)
    lapply(filenames, function(x) {
        paste(c(directory, "/", x), collapse="")
    })
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    paths <- compute_paths(directory, id)
    dataframes <- lapply(paths, read.csv)
    
    dataset <- data.frame()
    for (frame in dataframes) {
        dataset <- rbind(dataset, frame)
    }
    
    mean(dataset[[pollutant]], na.rm=TRUE)
}