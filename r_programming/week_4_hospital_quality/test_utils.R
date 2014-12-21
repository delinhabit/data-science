outcome.data <- NULL

mbest <- function(state, outcome) {
    # Memoized version of the best function that will use the already read
    # data to speed up the computation
    if (is.null(outcome.data)) {
        outcome.data <<- read.csv(
            "outcome-of-care-measures.csv",
            colClasses = "character")
    }

    best(state, outcome, outcome.data = outcome.data)
}

mrankhospital <- function(state, outcome, num) {
    # Memoized version of the rankhospital function that will use the
    # already read data to speed up the computation
    if (is.null(outcome.data)) {
        outcome.data <<- read.csv(
            "outcome-of-care-measures.csv",
            colClasses = "character")
    }

    rankhospital(state, outcome, num, outcome.data = outcome.data)
}

mrankall <- function(outcome, num) {
    # Memoized version of the rankhospital function that will use the
    # already read data to speed up the computation
    if (is.null(outcome.data)) {
        outcome.data <<- read.csv(
            "outcome-of-care-measures.csv",
            colClasses = "character")
    }

    rankall(outcome, num, outcome.data = outcome.data)
}