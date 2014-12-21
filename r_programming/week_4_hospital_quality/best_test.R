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
    tryCatch(
        mbest("BB", "heart attack"),
        error = function(e) {
            checkEquals(e$message, "invalid state")
        })
}

test_bestInvalidOutcome <- function() {
    tryCatch(
        mbest("NY", "hert attack"),
        error = function(e) {
            checkEquals(e$message, "invalid outcome")
        })
}
