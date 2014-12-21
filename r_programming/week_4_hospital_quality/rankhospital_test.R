source('rankhospital.R')
source('test_utils.R')

test_rankhospitalByNumber <- function() {
    checkEquals(
        mrankhospital("TX", "heart failure", 4),
        "DETAR HOSPITAL NAVARRO")
}

test_rankhospitalByBest <- function() {
    checkEquals(
        mrankhospital("TX", "heart failure", "best"),
        "FORT DUNCAN MEDICAL CENTER")
}

test_rankhospitalByWorst <- function() {
    checkEquals(
        mrankhospital("MD", "heart attack", "worst"),
        "HARFORD MEMORIAL HOSPITAL")
}

test_rankhospitalNoRank <- function() {
    checkTrue(is.na(mrankhospital("MN", "heart attack", 5000)))
}

test_rankhospitalInvalid <- function() {
    checkTrue(is.na(mrankhospital("MN", "heart attack", "invalid")))
}

test_rankhospitalByInvalidState <- function() {
    obs <- tryCatch(mrankhospital("BB", "heart attack"), error=simpleError)
    checkEquals(
        obs$message,
        paste("Error in rankhospital(state, outcome, num,",
              " outcome.data = outcome.data): invalid state\n",
              sep=""))
}

test_bestInvalidOutcome <- function() {
    obs <- tryCatch(mrankhospital("NY", "hert attack"), error=simpleError)
    checkEquals(
        obs$message,
        paste("Error in rankhospital(state, outcome, num,",
              " outcome.data = outcome.data): invalid outcome\n",
              sep=""))
}

test_rankhospitalWithoutProvidingData <- function() {
    checkEquals(
        rankhospital("TX", "heart failure", 3),
        "CYPRESS FAIRBANKS MEDICAL CENTER")
}