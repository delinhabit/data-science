test_best <- function() {
    outcome.data <- read.csv(
        "outcome-of-care-measures.csv",
        colClasses = "character")

    mbest <- function(state, outcome) {
        # Memoized version of the best function that will use the already read
        # data to speed up the computation
        best(state, outcome, outcome.data = outcome.data)
    }

    checkEquals(mbest("TX", "heart attack"), "CYPRESS FAIRBANKS MEDICAL CENTER")
    checkEquals(mbest("TX", "heart failure"), "FORT DUNCAN MEDICAL CENTER")
    checkEquals(mbest("MD", "pneumonia"), "GREATER BALTIMORE MEDICAL CENTER")
}

test_bestWithoutProvidingData <- function() {
    checkEquals(best("MD", "heart attack"), "JOHNS HOPKINS HOSPITAL, THE")
}

test_bestInvalidState <- function() {
    obs <- tryCatch(best("BB", "heart attack"), error=simpleError)
    checkEquals(
        obs$message,
        "Error in best(\"BB\", \"heart attack\"): invalid state\n")
}

test_bestInvalidOutcome <- function() {
    obs <- tryCatch(best("NY", "hert attack"), error=simpleError)
    checkEquals(
        obs$message,
        "Error in best(\"NY\", \"hert attack\"): invalid outcome\n")
}