valid.outcomes <- list(11, 17, 23)
names(valid.outcomes) <- list("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome, outcome.data = NULL) {
    if (is.null(outcome.data)) {
        outcome.data <- read.csv(
            "outcome-of-care-measures.csv",
            colClasses = "character")
    }

    if (!state %in% outcome.data$State) stop("invalid state")
    if (!outcome %in% names(valid.outcomes)) stop("invalid outcome")

    outcome.column <- valid.outcomes[[outcome]]
    state.data <- outcome.data[outcome.data$State == state, ]

    state.data[[outcome.column]] <- as.numeric(state.data[[outcome.column]])
    ordering <- order(
        state.data[, outcome.column],
        state.data$Hospital.Name,
        na.last = NA)
    sorted.data <- state.data[ordering, ]
    sorted.data$Hospital.Name[[1]]
}
