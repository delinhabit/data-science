library('RUnit')

test.suite <- defineTestSuite(
    "Week 4 assignments",
    dirs = file.path(getwd()),
    testFileRegexp = '^\\w+_test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
