compute_paths <- function(directory, id = 1:332) {
    filenames <- sprintf("%03d.csv", id)
    lapply(filenames, function(x) {
        paste(c(directory, "/", x), collapse="")
    })
}

compute_completes <- function(id, dataframes) {
    nobs <- sapply(dataframes, function(d) {
        sum(!is.na(d$sulfate) & !is.na(d$nitrate))
    })
    
    completes <- data.frame(id, nobs)
    names(completes) <- c("id", "nobs")
    completes
}

complete <- function(directory, id = 1:332) {
    paths <- compute_paths(directory, id)
    dataframes <- lapply(paths, read.csv)
    compute_completes(id, dataframes)
}