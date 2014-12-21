valid.outcomes <- list(11, 17, 23)
names(valid.outcomes) <- list("heart attack", "heart failure", "pneumonia")

rankstate <- function(state.data, outcome.column, num) {
    state.data[[outcome.column]] <- suppressWarnings(
        as.numeric(state.data[[outcome.column]]))
    ordering <- order(
        state.data[, outcome.column],
        state.data$Hospital.Name,
        na.last = NA)
    sorted.data <- state.data[ordering, ]

    rank <- if (num == "best") {
        1
    } else if (num == "worst") {
        nrow(sorted.data)
    } else if (num > nrow(sorted.data)) {
        NA
    } else {
        suppressWarnings(as.numeric(num))
    }
    if (is.na(rank)) return(NA)

    sorted.data$Hospital.Name[[rank]]
}

rankall <- function(outcome, num = "best", outcome.data = NULL) {
    if (is.null(outcome.data)) {
        outcome.data <- read.csv(
            "outcome-of-care-measures.csv",
            colClasses = "character")
    }
    if (!outcome %in% names(valid.outcomes)) stop("invalid outcome")

    outcome.column <- valid.outcomes[[outcome]]
    states.data <- split(outcome.data, outcome.data$State)
    ranks <- sapply(
        states.data,
        rankstate,
        outcome.column = outcome.column,
        num = num)
    data.frame(hospital = ranks, state = names(ranks))
}