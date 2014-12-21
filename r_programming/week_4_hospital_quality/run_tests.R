library('RUnit')

source('best.R')

test.suite <- defineTestSuite(
    "best",
    dirs = file.path(getwd()),
    testFileRegexp = '^best_test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)