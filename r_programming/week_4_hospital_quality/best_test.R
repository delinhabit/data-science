source('best.R')
source('test_utils.R')

test_best <- function() {
    checkEquals(mbest("TX", "heart attack"), "CYPRESS FAIRBANKS MEDICAL CENTER")
    checkEquals(mbest("TX", "heart failure"), "FORT DUNCAN MEDICAL CENTER")
    checkEquals(mbest("MD", "pneumonia"), "GREATER BALTIMORE MEDICAL CENTER")
}

test_bestWithoutProvidingData <- function() {
    checkEquals(best("MD", "heart attack"), "JOHNS HOPKINS HOSPITAL, THE")
}

test_bestInvalidState <- function() {
    obs <- tryCatch(mbest("BB", "heart attack"), error=simpleError)
    checkEquals(
        obs$message,
        paste("Error in best(state, outcome,",
              " outcome.data = outcome.data): invalid state\n",
              sep=""))
}

test_bestInvalidOutcome <- function() {
    obs <- tryCatch(mbest("NY", "hert attack"), error=simpleError)
    checkEquals(
        obs$message,
        paste("Error in best(state, outcome,",
              " outcome.data = outcome.data): invalid outcome\n",
              sep=""))
}