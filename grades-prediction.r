library(dplyr)

csvData <- read.csv(file="C:\\Users\\Taha\\Desktop\\eps-97-grades-cleaned.csv", header=TRUE, sep=",");
csvData <- select(grades, Assignment..Quiz.1..Real., Assignment..Quiz.2..Real.);
csvData <- csvData[complete.cases(csvData), ];
csvData$quiz_1_2 <- csvData$Assignment..Quiz.1..Real. + csvData$Assignment..Quiz.2..Real.

avg <- sum(csvData$quiz_1_2) / nrow(csvData)

n <- c(10, 20, 40);
for(i in n)
{
    cnt = 0
    for(j in 1:10000)
    {
        sample_data <- sample_n(csvData, i, replace=TRUE);
        sample_mean = mean(sample_data$quiz_1_2);
        sample_sd = sd(sample_data$quiz_1_2);
        err = 1.96 * sample_sd / sqrt(i);
        if(between(avg, sample_mean - err, sample_mean + err)) {
          cnt <- cnt + 1
        }
    }
    print(paste("for n =", as.character(i), "count = ", as.character(cnt)));
}
