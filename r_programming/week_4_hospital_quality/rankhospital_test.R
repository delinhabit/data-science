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
    tryCatch(
        mrankhospital("BB", "heart attack"),
        error = function(e) {
            checkEquals(e$message, "invalid state")
        })
}

test_rankhospitalInvalidOutcome <- function() {
    tryCatch(
        mrankhospital("NY", "hert attack"),
        error = function(e) {
            checkEquals(e$message, "invalid outcome")
        })
}

test_rankhospitalWithoutProvidingData <- function() {
    checkEquals(
        rankhospital("TX", "heart failure", 3),
        "CYPRESS FAIRBANKS MEDICAL CENTER")
}
