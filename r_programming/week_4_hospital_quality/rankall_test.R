source('rankall.R')
source('test_utils.R')

test_rankallWithNumber = function () {
    states = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL")
    hospitals <- c(
        NA,
        "D W MCMILLAN MEMORIAL HOSPITAL",
        "ARKANSAS METHODIST MEDICAL CENTER",
        "JOHN C LINCOLN DEER VALLEY HOSPITAL",
        "SHERMAN OAKS HOSPITAL",
        "SKY RIDGE MEDICAL CENTER",
        "MIDSTATE MEDICAL CENTER",
        NA,
        NA,
        "SOUTH FLORIDA BAPTIST HOSPITAL")

    result <- head(mrankall("heart attack", 20), 10)
    checkEquals(as.vector(result$hospital), hospitals)
    checkEquals(as.vector(result$state), states)
}

test_rankallWithWorst = function () {
    states = c("WI", "WV", "WY")
    hospitals = c(
        "MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC",
        "PLATEAU MEDICAL CENTER",
        "NORTH BIG HORN HOSPITAL DISTRICT")

    result <- tail(mrankall("pneumonia", "worst"), 3)
    checkEquals(as.vector(result$hospital), hospitals)
    checkEquals(as.vector(result$state), states)
}

test_rankallWithBest = function () {
    states = c("TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
    hospitals = c(
        "WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL",
        "FORT DUNCAN MEDICAL CENTER",
        "VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER",
        "SENTARA POTOMAC HOSPITAL",
        "GOV JUAN F LUIS HOSPITAL & MEDICAL CTR",
        "SPRINGFIELD HOSPITAL",
        "HARBORVIEW MEDICAL CENTER",
        "AURORA ST LUKES MEDICAL CENTER",
        "FAIRMONT GENERAL HOSPITAL",
        "CHEYENNE VA MEDICAL CENTER")

    result <- tail(mrankall("heart failure", "best"), 10)
    checkEquals(as.vector(result$hospital), hospitals)
    checkEquals(as.vector(result$state), states)
}

test_rankallInvalidOutcome <- function() {
    obs <- tryCatch(mrankall("hert attack"), error=simpleError)
    checkEquals(
        obs$message,
        paste("Error in rankall(outcome, num,",
              " outcome.data = outcome.data): invalid outcome\n",
              sep=""))
}