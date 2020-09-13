query_time <- scan(file = "./data.csv", what = integer(), sep = "\n")

hist(query_time,
     main = "Adding 10 nodes query time histogram",
     xlab = "Delivery time in microseconds",
     ylab = "Number of occurences",
     border="blue", col="green",
     breaks = 20)
