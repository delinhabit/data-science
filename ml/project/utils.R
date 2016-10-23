downloadData <- function(url, filename) {
    # Download the data from the provided URL and return the path to the
    # downloaded file.
    #
    # Arguments
    #    url - the URL from wich to download the data file
    #    filename (optional) - the name of the destination file. If not
    #                          specified will use the original filename.
    #
    if (!file.exists("data")) {
        dir.create("data")
    }

    if(missing(filename)) {
        filename <- basename(url)
    }

    destfile = paste(c("data", filename), collapse="/")
    if (!file.exists(destfile)) {
        download.file(url, destfile = destfile, method = "curl")
    }

    destfile
}
