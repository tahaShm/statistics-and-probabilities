library(dplyr)

grades <- read.csv(file="C:\\Users\\Taha\\Desktop\\eps-96-grades-cleaned.csv", header=TRUE, sep=",");
grades <- select(grades, Assignment..Final.Exam..Real.);
grades <- filter(grades, grades$Assignment..Final.Exam..Real. != '-')
grades$final <- grades$Assignment..Final.Exam..Real.;
grades$Assignment..Final.Exam..Real. <- NULL;
grades$final <- as.numeric(levels(grades$final))[grades$final]

data <- grades$final;
m1 = mean(grades$final)
m2 = mean(grades$final ** 2)
alpha = (m1 * m1) / (m2 - m1 * m1)
teta = (m2 - m1 * m1) / m1
xfit = seq(from = min(data), to = max(data), length = length(data));
sample_gamma = dgamma(xfit, shape = alpha, scale = teta);
hist(data, freq=FALSE)
lines(xfit, sample_gamma, col = "Red");


n = 15
sample_data <- sample_n(grades, n)$final;
print(sample_data)
miu = 24
sample_mean = mean(sample_data)
sample_sd = sd(sample_data)
err = 1.96 * sample_sd / sqrt(n);
if (between(miu, sample_mean - err, sample_mean + err)){
    print("True");
}else{
    print("False");
}
