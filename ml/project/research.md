# Machine Learning for Human Activity Recognition
Ion Scerbatiuc  
October 22, 2016  



## Synopsis

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. 

One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this research paper we will use sensory data from various accelerometers to predict how well the participants performed on the given physical excercise.

Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

Read more about the experiment and the available data sources here: http://groupware.les.inf.puc-rio.br/har#ixzz4NqVb46KZ

## Getting and cleaning the data


```r
source('utils.R')

trainingDataFile <- downloadData('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv')
trainingData <- read.csv(trainingDataFile)
```
